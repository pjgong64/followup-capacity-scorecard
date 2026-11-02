###############################################################################
#  fi_06_scorecard.R  --  Study 3 (Financial Innovation), step 6
#
#  A points scorecard an officer can read, built from the EX-ANTE information
#  set only (everything known at the start of the season), in the units the
#  bank already uses (baht, rai, months, grade). Standard construction
#  (Siddiqi, 2006; Thomas, Edelman & Crook, 2002):
#
#    1. Binning. Numeric characteristics: 20 quantile bins, merged until the
#       bad rate is monotone in the direction of the overall association, every
#       bin holds >= 5% of rows, and there are <= 8 bins. Categorical ones:
#       one bin per level, levels under 1% pooled. A missing value gets its own
#       bin. All limits are fixed here, before any data are seen.
#    2. WOE = ln(share of bads / share of goods) per bin; IV per characteristic.
#    3. Screening: IV >= 0.02; of any pair whose WOE columns correlate above
#       |0.70|, the lower-IV one is dropped.
#    4. Unweighted logistic regression on the WOE columns. A characteristic
#       whose coefficient comes out negative (risk running against its own WOE)
#       is dropped and the model refitted, until every coefficient is positive.
#    5. Scaling: 600 points at good:bad odds of 50:1, 20 points to double the
#       odds (PDO). Higher score = safer. Points rounded to integers.
#    6. Reason codes: for each borrower, the three characteristics on which he
#       falls furthest below the best attainable points.
#
#  Every step uses DEVELOPMENT rows only:
#    rolling  build on 2020, evaluate on 2021  (selection-period check)
#    main     build on 2020+2021, evaluate on 2022 (reported once)
#  The sub-district default history is target-encoded OUT OF FOLD on the
#  development rows (5 borrower folds, smoothing m = 100) and from all
#  development rows for the scored year, so no row sees its own outcome.
#  The scored year's PD is then moved to the EM-estimated prior (unlabelled
#  scores), as in fi_02, for use by fi_08.
#
#  Output: fi_out/fi06_scorecard_<run>.csv   the points table
#          fi_out/fi06_scorecard_main.md     the same, readable
#          fi_out/fi06_score_<run>.rds, fi06_pd_<run>.rds, fi06_reasons_main.rds,
#          fi06_all_scores_main.rds, fi06_spec_main.rds (for fi_09)
#          fi_out/fi06_log.txt
###############################################################################

suppressPackageStartupMessages(library(data.table))

RAW_FILE <- "1_data_before_out.txt"
RUNS <- list(
  exante_rolling = list(dir = file.path("factorial7_out", "rolling_2020_to_2021", "exante", "pred"),
                        dev_years = 1L, test_year = 2L),
  exante_main    = list(dir = file.path("factorial7_out", "exante", "pred"),
                        dev_years = 1:2, test_year = 3L)
)
N_START <- 20; MIN_SHARE <- 0.05; MAX_BINS <- 8; CAT_MIN <- 0.01
IV_MIN <- 0.02; COR_MAX <- 0.70
PDO <- 20; BASE_SCORE <- 600; BASE_ODDS <- 50
TE_M <- 100; TE_FOLDS <- 5
K_REASON <- 19                  # feasible capacity (fi_04) at which reasons are tallied
EM_MAX <- 500; EM_TOL <- 1e-10
SEED <- 20261026
OUT_DIR <- "fi_out"

LOG <- file.path(OUT_DIR, "fi06_log.txt"); cat("", file = LOG)
say <- function(...) { s <- sprintf(...); cat(s, "\n", sep = ""); cat(s, "\n", sep = "", file = LOG, append = TRUE) }
say("fi_06_scorecard  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
set.seed(SEED)

## ---------------------------------------------------- characteristics
# Ex-ante only: nothing measured on 31 December (Out, Dep, CL and their ratios
# are absent). Labels are the officer-facing names used in the reason codes.
NUMERIC <- c(Outb = "Outstanding principal at start of year (THB)",
             CLb = "Credit limit at start of year (THB)",
             Depb = "Deposits at start of year (THB)",
             nLC = "Number of active loan contracts",
             nIS = "Number of income sources",
             nP = "Number of rice plots",
             Area = "Rice area planted (rai)",
             AreaR = "Area on land-rights document (rai)",
             nRV = "Number of rice varieties",
             CD = "Cultivation days",
             eCostrai = "Estimated cost per rai (THB)",
             eCost = "Estimated total cost (THB)",
             eYierai = "Estimated yield per rai",
             eYie = "Estimated total yield (kg)",
             ePrcton = "Estimated price per tonne (THB)",
             eInc = "Estimated rice income (THB)",
             Profit_buffer = "Estimated margin (eInc - eCost) / eInc",
             Land_util = "Planted / registered area",
             Area_default_hist = "Sub-district default history (earlier years)")
CATEG <- c(Int = "Customer grade (contract rate)",
           Previous_Default = "Previous-year repayment status",
           Has_Credit_History = "Has credit history with the bank",
           IDRV = "Main rice variety",
           MP = "Planting month", MH = "Harvest month",
           IDDT = "Land-rights document type")
NOT_ACTIONABLE <- c("Area_default_hist", "IDDT", "MP", "MH", "IDRV")

raw <- fread(RAW_FILE, sep = "|", showProgress = TRUE,
             select = c("Year", "ID", "ADM3", "Y", setdiff(names(NUMERIC), c("Profit_buffer", "Land_util", "Area_default_hist")),
                        names(CATEG)))
raw[, Profit_buffer := fifelse(eInc > 0, (eInc - eCost) / eInc, NA_real_)]
raw[, Land_util := fifelse(AreaR > 0, Area / AreaR, NA_real_)]
for (cc in names(CATEG)) raw[[cc]] <- as.character(raw[[cc]])

## ---------------------------------------------------- binning helpers
woe_tab <- function(bin, y) {
  t <- data.table(bin = bin, y = y)[, .(n = .N, bad = sum(y)), by = bin]
  t[, good := n - bad]
  B <- sum(t$bad); G <- sum(t$good)
  t[, woe := log(((bad + 0.5) / B) / ((good + 0.5) / G))]
  t[, iv := ((bad + 0.5) / B - (good + 0.5) / G) * woe]
  t
}
bin_numeric <- function(x, y) {
  ok <- !is.na(x)
  br <- unique(quantile(x[ok], probs = seq(0, 1, length.out = N_START + 1), names = FALSE, type = 7))
  if (length(br) < 3) return(NULL)
  cuts <- br[-c(1, length(br))]                      # interior cut points
  dir <- sign(suppressWarnings(cor(x[ok], y[ok], method = "spearman")))
  if (!is.finite(dir) || dir == 0) dir <- 1
  repeat {
    b <- findInterval(x[ok], cuts, left.open = TRUE) + 1L
    empty <- setdiff(seq_len(length(cuts) + 1L), unique(b))
    if (length(empty)) { cuts <- cuts[-unique(pmin(empty, length(cuts)))]; if (!length(cuts)) return(NULL); next }
    s <- data.table(b = b, y = y[ok])[, .(n = .N, r = mean(y)), keyby = b]
    nb <- nrow(s)
    viol <- which(dir * diff(s$r) < 0)               # bad rate going the wrong way
    small <- which(s$n < MIN_SHARE * sum(ok))
    if (!length(viol) && !length(small) && nb <= MAX_BINS) break
    if (nb <= 2) break
    if (length(viol)) {
      i <- viol[which.min(abs(diff(s$r))[viol])]     # merge the closest violating pair
    } else if (length(small)) {
      j <- small[which.min(s$n[small])]
      i <- if (j == 1) 1L else if (j == nb) nb - 1L else
        if (abs(s$r[j] - s$r[j - 1]) <= abs(s$r[j] - s$r[j + 1])) j - 1L else j
    } else i <- which.min(abs(diff(s$r)))
    cuts <- cuts[-i]                                  # remove the boundary between bins i and i+1
  }
  cuts
}
apply_numeric <- function(x, cuts) {
  b <- paste0("b", findInterval(x, cuts, left.open = TRUE) + 1L)
  b[is.na(x)] <- "missing"
  b
}
label_numeric <- function(cuts) {
  lo <- c(-Inf, cuts); hi <- c(cuts, Inf)
  f <- function(v) if (is.infinite(v)) "" else format(signif(v, 4), big.mark = ",", scientific = FALSE)
  setNames(c(sprintf("%s..%s", vapply(lo, f, ""), vapply(hi, f, "")), "missing"),
           c(paste0("b", seq_along(lo)), "missing"))
}
bin_categ <- function(x, y) {
  t <- table(x, useNA = "no"); keep <- names(t)[t >= CAT_MIN * length(x)]
  keep
}
apply_categ <- function(x, keep) { b <- ifelse(x %in% keep, x, "other"); b[is.na(x)] <- "missing"; b }

## ---------------------------------------------------- target encoding
te_area <- function(dev, tst) {
  prior <- mean(dev$Y)
  folds <- sample.int(TE_FOLDS, uniqueN(dev$ID), replace = TRUE)
  fmap <- data.table(ID = unique(dev$ID), f = folds)
  dev <- merge(dev[, .(ID, Year, ADM3, Y, .r = .I)], fmap, by = "ID")
  te_dev <- numeric(nrow(dev))
  for (k in seq_len(TE_FOLDS)) {
    st <- dev[f != k, .(s = sum(Y), n = .N), by = ADM3]
    idx <- which(dev$f == k)
    m <- st[match(dev$ADM3[idx], st$ADM3)]
    te_dev[idx] <- ifelse(is.na(m$n), prior, (m$s + TE_M * prior) / (m$n + TE_M))
  }
  out_dev <- numeric(nrow(dev)); out_dev[dev$.r] <- te_dev
  st <- dev[, .(s = sum(Y), n = .N), by = ADM3]
  m <- st[match(tst$ADM3, st$ADM3)]
  list(dev = out_dev, tst = ifelse(is.na(m$n), prior, (m$s + TE_M * prior) / (m$n + TE_M)))
}

fast_auc <- function(score, y) {
  n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  r <- rank(score, ties.method = "average"); (sum(as.numeric(r[y == 1])) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
clip <- function(p, e = 1e-12) pmin(pmax(p, e), 1 - e)
odds <- function(p) clip(p) / (1 - clip(p))
shift_prior <- function(p, a, b) { r <- (b / (1 - b)) / (a / (1 - a)); o <- odds(p) * r; o / (1 + o) }
em_prior <- function(p, a) { h <- a; for (i in seq_len(EM_MAX)) { n <- mean(shift_prior(p, a, h)); if (abs(n - h) < EM_TOL) return(n); h <- n }; h }

## ---------------------------------------------------- build, per run
FACTOR <- PDO / log(2); OFFSET <- BASE_SCORE - FACTOR * log(BASE_ODDS)
for (run in names(RUNS)) {
  R <- RUNS[[run]]
  say("\n================ %s: build on Year %s, score Year %d ================", run,
      paste(R$dev_years, collapse = "+"), R$test_year)
  dev <- raw[Year %in% R$dev_years]; tst <- raw[Year == R$test_year]
  te <- te_area(dev, tst); dev[, Area_default_hist := te$dev]; tst[, Area_default_hist := te$tst]
  y <- dev$Y

  # 1-2. bins, WOE, IV
  spec <- list(); ivs <- c()
  for (v in names(NUMERIC)) {
    cuts <- bin_numeric(dev[[v]], y); if (is.null(cuts)) next
    b <- apply_numeric(dev[[v]], cuts); wt <- woe_tab(b, y)
    spec[[v]] <- list(type = "num", cuts = cuts, woe = setNames(wt$woe, wt$bin), tab = wt)
    ivs[v] <- sum(wt$iv)
  }
  for (v in names(CATEG)) {
    keep <- bin_categ(dev[[v]], y); b <- apply_categ(dev[[v]], keep); wt <- woe_tab(b, y)
    if (nrow(wt) < 2) next
    spec[[v]] <- list(type = "cat", keep = keep, woe = setNames(wt$woe, wt$bin), tab = wt)
    ivs[v] <- sum(wt$iv)
  }
  ivs <- sort(ivs, decreasing = TRUE)
  say("information value (development rows):")
  for (v in names(ivs)) say("   %-20s IV %.4f  %d bins", v, ivs[v], nrow(spec[[v]]$tab))

  to_woe <- function(d, v) {
    S <- spec[[v]]
    b <- if (S$type == "num") apply_numeric(d[[v]], S$cuts) else apply_categ(d[[v]], S$keep)
    w <- S$woe[b]; w[is.na(w)] <- 0                  # unseen bin -> neutral
    unname(w)
  }
  # 3. screening
  cand <- names(ivs)[ivs >= IV_MIN]
  W <- as.data.table(lapply(setNames(cand, cand), function(v) to_woe(dev, v)))
  cm <- abs(cor(W)); drop <- character(0)
  for (i in seq_along(cand)) for (j in seq_along(cand)) if (i < j && cm[i, j] > COR_MAX) {
    lo <- if (ivs[cand[i]] >= ivs[cand[j]]) cand[j] else cand[i]; drop <- c(drop, lo)
  }
  vars <- setdiff(cand, unique(drop))
  say("screened: IV < %.2f dropped %d; |r| > %.2f dropped %s", IV_MIN, length(ivs) - length(cand), COR_MAX,
      if (length(drop)) paste(unique(drop), collapse = ", ") else "none")

  # 4. logistic regression, sign check
  repeat {
    fit <- glm(y ~ ., data = W[, ..vars], family = binomial(), model = FALSE, x = FALSE, y = FALSE)
    b <- coef(fit)[-1]; neg <- names(b)[!is.na(b) & b <= 0]
    if (!length(neg)) break
    worst <- neg[which.min(b[neg])]; vars <- setdiff(vars, worst)
    say("   dropped %s (coefficient %.3f against its own WOE), refitting", worst, min(b[neg]))
  }
  b0 <- coef(fit)[1]; b <- coef(fit)[-1]; nv <- length(vars)
  say("final scorecard: %d characteristics: %s", nv, paste(vars, collapse = ", "))

  # 5. points
  tabs <- rbindlist(lapply(vars, function(v) {
    S <- spec[[v]]; t <- copy(S$tab)
    t[, points := round(-(b[[v]] * woe + b0 / nv) * FACTOR + OFFSET / nv)]
    t[, `:=`(characteristic = v,
             label = if (v %in% names(NUMERIC)) NUMERIC[[v]] else CATEG[[v]],
             range = if (S$type == "num") label_numeric(S$cuts)[bin] else bin,
             share = n / sum(n), bad_rate = bad / n, IV_char = ivs[[v]], coef = b[[v]])]
    t[, ord := if (S$type == "num") suppressWarnings(as.integer(sub("b", "", bin))) else rank(-points, ties.method = "first")]
    t[order(ord)]
  }))
  setcolorder(tabs, c("characteristic", "label", "range", "n", "share", "bad_rate", "woe", "points", "IV_char", "coef"))
  fwrite(tabs[, !c("ord", "bin")], file.path(OUT_DIR, sprintf("fi06_scorecard_%s.csv", run)))

  score_of <- function(d) {
    pts <- sapply(vars, function(v) {
      S <- spec[[v]]
      bb <- if (S$type == "num") apply_numeric(d[[v]], S$cuts) else apply_categ(d[[v]], S$keep)
      p <- setNames(tabs[characteristic == v, points], tabs[characteristic == v, bin])[bb]
      p[is.na(p)] <- round(OFFSET / nv - b0 / nv * FACTOR)   # unseen bin: neutral points
      p
    })
    pts
  }
  P_dev <- score_of(dev); P_tst <- score_of(tst)
  s_dev <- rowSums(P_dev); s_tst <- rowSums(P_tst)
  say("score range on the scored year: %d .. %d (median %d)", min(s_tst), max(s_tst), as.integer(median(s_tst)))

  # discrimination: points vs the continuous ex-ante LR and XGB
  kt <- as.data.table(readRDS(file.path(R$dir, "keys_test.rds"))); setnames(kt, tolower(names(kt)))
  if (!identical(nrow(kt), nrow(tst))) stop("row count differs from keys_test")
  pos <- match(kt$id, tst$ID); if (anyNA(pos)) stop("keys_test ids not found")
  y_t <- tst$Y[pos]; stopifnot(all(y_t == kt$y))
  sc_t <- s_tst[pos]
  auc_sc <- fast_auc(-sc_t, y_t)
  auc_lr <- fast_auc(readRDS(file.path(R$dir, "pred_LR__F_SP.rds")), y_t)
  auc_xg <- fast_auc(readRDS(file.path(R$dir, "pred_XGB__F_SP.rds")), y_t)
  say("AUC on Year %d: scorecard %.4f | ex-ante LR %.4f | ex-ante XGB %.4f  (binning costs %.4f, LR vs XGB %.4f)",
      R$test_year, auc_sc, auc_lr, auc_xg, auc_lr - auc_sc, auc_xg - auc_lr)
  say("AUC on development rows: scorecard %.4f", fast_auc(-s_dev, y))

  # PD: score -> development-calibrated PD -> EM prior on the scored year (no labels)
  p_dev_cal <- 1 / (1 + exp((sc_t - OFFSET) / FACTOR))   # odds_good = exp((score-offset)/factor)
  pi_dev <- mean(y); pd_tst <- shift_prior(p_dev_cal, pi_dev, em_prior(p_dev_cal, pi_dev))
  say("PD: development prior %.4f, EM prior on the scored year %.4f (realised %.4f, check only)",
      pi_dev, mean(pd_tst), mean(y_t))
  saveRDS(sc_t, file.path(OUT_DIR, sprintf("fi06_score_%s.rds", run)))
  saveRDS(pd_tst, file.path(OUT_DIR, sprintf("fi06_pd_%s.rds", run)))

  # 6. reason codes on the scored year
  if (run == "exante_main") {
    best <- tabs[, .(best = max(points)), by = characteristic]
    short <- sweep(-P_tst[pos, , drop = FALSE], 2, -best[match(vars, characteristic), best])   # best - points
    top <- t(apply(short, 1, function(r) { o <- order(-r); c(vars[o[1:3]], r[o[1:3]]) }))
    reasons <- data.table(id = kt$id, score = sc_t, r1 = top[, 1], r2 = top[, 2], r3 = top[, 3],
                          gap1 = as.numeric(top[, 4]), gap2 = as.numeric(top[, 5]), gap3 = as.numeric(top[, 6]))
    saveRDS(reasons, file.path(OUT_DIR, "fi06_reasons_main.rds"))
    flag <- rank(sc_t, ties.method = "first") <= floor(K_REASON / 100 * length(sc_t))
    tally <- reasons[flag, .N, by = r1][order(-N)][, share := N / sum(N)]
    tally[, label := ifelse(r1 %in% names(NUMERIC), NUMERIC[r1], CATEG[r1])]
    say("\nreason code 1 among the %d%% lowest scores (%s borrowers):", K_REASON, format(sum(flag), big.mark = ","))
    for (i in seq_len(nrow(tally))) say("   %5.1f%%  %s", 100 * tally$share[i], tally$label[i])
    na_share <- reasons[flag, mean(r1 %in% NOT_ACTIONABLE)]
    say("share whose first reason is something the borrower cannot change (area history, document type, variety, calendar): %.1f%%",
        100 * na_share)
    fwrite(tally, file.path(OUT_DIR, "fi06_reason_tally_main.csv"))

    md <- c(sprintf("# Ex-ante points scorecard (built on 2020-2021, %d characteristics)", nv), "",
            sprintf("600 points at good:bad odds 50:1; %d points double the odds. Higher = safer.", PDO), "",
            "| Characteristic | Range / level | Share | Bad rate | Points |", "|---|---|---|---|---|")
    for (i in seq_len(nrow(tabs)))
      md <- c(md, sprintf("| %s | %s | %.1f%% | %.2f%% | %d |", tabs$label[i], tabs$range[i],
                          100 * tabs$share[i], 100 * tabs$bad_rate[i], as.integer(tabs$points[i])))
    writeLines(md, file.path(OUT_DIR, "fi06_scorecard_main.md"))

    # every borrower-year scored by the main card, for the stability checks of
    # fi_09: development years carry their out-of-fold area history
    best_v <- best[match(vars, characteristic), best]
    first_reason <- function(P) vars[max.col(-sweep(P, 2, best_v), ties.method = "first")]
    all_sc <- rbind(data.table(Year = dev$Year, ID = dev$ID, Y = dev$Y, score = s_dev, r1 = first_reason(P_dev)),
                    data.table(Year = tst$Year, ID = tst$ID, Y = tst$Y, score = s_tst, r1 = first_reason(P_tst)))
    saveRDS(all_sc, file.path(OUT_DIR, "fi06_all_scores_main.rds"))
    saveRDS(list(vars = vars, spec = spec[vars], tabs = tabs, offset = OFFSET, factor = FACTOR),
            file.path(OUT_DIR, "fi06_spec_main.rds"))
    say("saved scores and first reasons for all %s borrower-years (fi06_all_scores_main.rds)",
        format(nrow(all_sc), big.mark = ","))
  }
}
say("\ndone. Paste %s and fi_out/fi06_scorecard_main.md into the chat.", LOG)

###############################################################################
#  fi_08_transparency.R  --  Study 3 (Financial Innovation), step 8
#
#  RQ3, the price of transparency, in baht. Three ex-ante models, from the
#  least to the most readable:
#     XGB        gradient-boosted trees              (fi_02 corrected PD)
#     LR         continuous logistic regression      (fi_02 corrected PD)
#     SCORECARD  integer points, 11 characteristics  (fi_06 PD)
#  plus the bank's own grade (no model).
#
#  For the scorecard the money-track rule is chosen exactly as fi_03 chose it
#  for XGB and LR: on the 2021 rolling run, the rule (R1 / R2 / R3) with the
#  highest mean exposure capture over K = 5..35%. Nothing is chosen on 2022.
#
#  Policies compared at the feasible capacities K = 13 / 19 / 38% (fi_04):
#     money-only  : selected rule
#     count-only  : corrected PD
#     mixed V2    : (1-s) money track + s count track on small loans
#                   (Outb <= P40), s = 0.3 and s = 0.5 (fi_05 s*)
#  Reported: recall, small-loan recall, exposure capture, EAR_FN (THB).
#  Price of transparency = EAR(model) - EAR(XGB) at the same policy and K, with
#  a Poisson bootstrap over ADM3 (B = 1000) at K = 19%.
#
#  Output: fi_out/fi08_transparency.csv, fi08_contrasts.csv, fi08_log.txt
###############################################################################

suppressPackageStartupMessages(library(data.table))

RAW_FILE <- "1_data_before_out.txt"
RUNS <- list(
  exante_rolling = list(dir = file.path("factorial7_out", "rolling_2020_to_2021", "exante", "pred"), year = 2L),
  exante_main    = list(dir = file.path("factorial7_out", "exante", "pred"),                         year = 3L)
)
MODELS <- c("XGB", "LR", "SCORECARD")
K_SET  <- c(13, 19, 38); SEL_K <- 5:35
SMALL_Q <- 0.40; LGD0 <- 1; B <- 1000; SEED <- 20261026
OUT_DIR <- "fi_out"

LOG <- file.path(OUT_DIR, "fi08_log.txt"); cat("", file = LOG)
say <- function(...) { s <- sprintf(...); cat(s, "\n", sep = ""); cat(s, "\n", sep = "", file = LOG, append = TRUE) }
bn  <- function(x) sprintf("%.2f bn", x / 1e9)
say("fi_08_transparency  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

sel <- fread(file.path(OUT_DIR, "fi03_selection.csv"))[selected == TRUE]
chosen <- c(XGB = sel[cell == "XGB__F_SP", rule], LR = sel[cell == "LR__F_SP", rule])

raw <- fread(RAW_FILE, sep = "|", showProgress = TRUE,
             select = c("Year", "ID", "Y", "Out", "Outb", "Int", "Previous_Default"))
build <- function(run) {
  R  <- RUNS[[run]]
  kt <- as.data.table(readRDS(file.path(R$dir, "keys_test.rds"))); setnames(kt, tolower(names(kt)))
  kt[, ord := .I]
  d <- merge(kt, raw[Year == R$year], by.x = "id", by.y = "ID", all.x = TRUE, sort = FALSE)
  setorder(d, ord)
  if (d[is.na(Out) | Y != y, .N] > 0) stop("join check failed for ", run)
  d[, pd_XGB := readRDS(file.path(OUT_DIR, sprintf("fi02_pd_%s_XGB__F_SP.rds", run)))]
  d[, pd_LR  := readRDS(file.path(OUT_DIR, sprintf("fi02_pd_%s_LR__F_SP.rds", run)))]
  d[, pd_SCORECARD := readRDS(file.path(OUT_DIR, sprintf("fi06_pd_%s.rds", run)))]
  d[, small := Outb <= quantile(Outb, SMALL_Q)]
  d
}
mscore <- function(d, m, rule) {
  p <- d[[paste0("pd_", m)]]
  switch(rule, R1_PD = p, R2_EXP = p * d$Outb, R3_EL = p * d$Outb * (LGD0 + d$Int))
}
policy_flag <- function(d, m, rule, k, s, tie) {
  N <- nrow(d); nK <- floor(k / 100 * N); f <- logical(N)
  o_m <- order(-mscore(d, m, rule), tie); o_p <- order(-d[[paste0("pd_", m)]], tie)
  n_c <- round(s * nK); n_m <- nK - n_c
  if (n_m > 0) f[o_m[seq_len(n_m)]] <- TRUE
  if (n_c > 0) { cand <- o_p[(!f & d$small)[o_p]]; f[cand[seq_len(min(n_c, length(cand)))]] <- TRUE }
  f
}
summ <- function(d, f) {
  y <- d$y == 1
  data.table(recall = sum(y & f) / sum(y), small_recall = sum(y & f & d$small) / sum(y & d$small),
             cap = sum(d$Out[y & f]) / sum(d$Out[y]), EAR = sum(d$Out[y]) - sum(d$Out[y & f]))
}

set.seed(SEED)
dat <- lapply(names(RUNS), build); names(dat) <- names(RUNS)
ties <- lapply(dat, function(d) runif(nrow(d)))

## ---------------------- scorecard money rule, chosen on 2021 like fi_03
d21 <- dat$exante_rolling
cap_curve <- function(d, sc, tie) {
  o <- order(-sc, tie); e <- d$Out[o] * (d$y[o] == 1); ce <- cumsum(e) / sum(e)
  ce[floor(SEL_K / 100 * nrow(d))]
}
sc_sel <- sapply(c("R1_PD", "R2_EXP", "R3_EL"), function(r) mean(cap_curve(d21, mscore(d21, "SCORECARD", r), ties$exante_rolling)))
chosen["SCORECARD"] <- names(which.max(sc_sel))
say("scorecard money rule chosen on 2021: %s (mean exposure capture %s)", chosen[["SCORECARD"]],
    paste(sprintf("%s %.4f", names(sc_sel), sc_sel), collapse = ", "))
say("money rules: %s", paste(names(chosen), chosen, sep = " -> ", collapse = ", "))

## ---------------------------------------------------- policies x models
POL <- list(money_only = list(rule = "money", s = 0), count_only = list(rule = "R1_PD", s = 0),
            mixed_0.3 = list(rule = "money", s = 0.3), mixed_0.5 = list(rule = "money", s = 0.5))
res <- list(); keep <- list()
for (run in names(RUNS)) {
  d <- dat[[run]]; tie <- ties[[run]]
  o_t <- order(-d$Int, -(d$Previous_Default == 1), -d$Outb, tie)
  for (k in K_SET) {
    f <- logical(nrow(d)); f[o_t[seq_len(floor(k / 100 * nrow(d)))]] <- TRUE
    res[[length(res) + 1]] <- cbind(run = run, model = "bank_grade", policy = "bank_grade", K = k, summ(d, f))
    if (run == "exante_main" && k == 19) keep[["bank_grade"]] <- f
    for (m in MODELS) for (pn in names(POL)) {
      P <- POL[[pn]]; rule <- if (P$rule == "money") chosen[[m]] else P$rule
      f <- policy_flag(d, m, rule, k, P$s, tie)
      res[[length(res) + 1]] <- cbind(run = run, model = m, policy = pn, K = k, summ(d, f))
      if (run == "exante_main" && k == 19) keep[[paste(m, pn)]] <- f
    }
  }
}
res <- rbindlist(res)
fwrite(res, file.path(OUT_DIR, "fi08_transparency.csv"))
for (rr in names(RUNS)) for (k in K_SET) {
  say("\n-- %s  K = %d%%", rr, k)
  s_ <- res[res$run == rr & res$K == k][order(policy, model)]
  for (i in seq_len(nrow(s_)))
    say("   %-11s %-10s recall %.3f  small-loan recall %.3f  capture %.3f  EAR %s",
        s_$policy[i], s_$model[i], s_$recall[i], s_$small_recall[i], s_$cap[i], bn(s_$EAR[i]))
}

## -------------------------------------------- bootstrap, 2022, K = 19%
say("\n== price of transparency, 2022, K = 19%% (95%% bootstrap over ADM3; EAR in bn THB, + = more left exposed) ==")
d <- dat$exante_main; cl <- as.integer(factor(d$adm3)); G <- max(cl)
W <- matrix(rpois(G * B, 1), nrow = G); yy <- d$y == 1
agg <- function(v) { g <- numeric(G); a <- tapply(v, cl, sum); g[as.integer(names(a))] <- a; colSums(W * g) }
Eb <- agg(d$Out * yy); Pb <- agg(as.numeric(yy)); Sb <- agg(as.numeric(yy & d$small))
ear <- function(f) Eb - agg(d$Out * (yy & f)); rec <- function(f) agg(as.numeric(yy & f)) / Pb
q <- function(v, sc = 1e9, fmt = "%+.2f") sprintf(paste0("[", fmt, ", ", fmt, "]"), quantile(v, .025) / sc, quantile(v, .975) / sc)
ct <- list()
for (pn in names(POL)) {
  for (pair in list(c("SCORECARD", "XGB"), c("LR", "XGB"), c("SCORECARD", "LR"))) {
    a <- keep[[paste(pair[1], pn)]]; b <- keep[[paste(pair[2], pn)]]
    de <- ear(a) - ear(b); dr <- rec(a) - rec(b)
    say("   %-11s %-9s - %-4s  EAR %s bn  recall %s pp", pn, pair[1], pair[2], q(de), q(dr, 0.01, "%+.1f"))
    ct[[length(ct) + 1]] <- data.table(policy = pn, a = pair[1], b = pair[2],
                                       dEAR_lo = quantile(de, .025), dEAR_hi = quantile(de, .975),
                                       drec_lo = quantile(dr, .025), drec_hi = quantile(dr, .975))
  }
  a <- keep[[paste("SCORECARD", pn)]]; de <- ear(a) - ear(keep[["bank_grade"]])
  say("   %-11s SCORECARD - bank grade  EAR %s bn", pn, q(de))
  ct[[length(ct) + 1]] <- data.table(policy = pn, a = "SCORECARD", b = "bank_grade",
                                     dEAR_lo = quantile(de, .025), dEAR_hi = quantile(de, .975),
                                     drec_lo = NA_real_, drec_hi = NA_real_)
}
fwrite(rbindlist(ct), file.path(OUT_DIR, "fi08_contrasts.csv"))
say("\ndone. Paste %s into the chat.", LOG)

###############################################################################
#  fi_03_capacity_frontier.R  --  Study 3 (Financial Innovation), step 3
#
#  Decision point (revised 19 Oct 2026): the START of the season, when the
#  bank sets each customer's grade (1 May) and renews the loan. The outcome is
#  the NPL status on 31 December, so the primary model is the EX-ANTE refit
#  (no year-end features, fi_01_exante.bat), and it is the ONLY model used.
#
#  Question (RQ1): the bank can follow up K% of the portfolio during the
#  season. Whom should it pick, and how much money is left exposed among the
#  defaulters it does not reach?
#
#  Ranking rules (the top K% are followed up):
#    R0_TIER  the bank's own grade, no model: contractual-rate tier (9.5%
#             first), then previous-year default, then the opening balance Outb
#    R1_PD    corrected PD                                       (counts heads)
#    R2_EXP   corrected PD x Outb                                (expected exposure)
#    R3_EL    corrected PD x Outb x (LGD + Int)                  (principal + interest)
#  Rankings use Outb, the balance KNOWN at the decision point. Outcomes are
#  measured with Out on 31 December, the exposure that actually defaulted
#  (un-capped file). R1 orders exactly as the raw score does.
#
#  Per set x run x model x rule x K (1..40%):
#    recall, exp_capture (Out reached / Out of all defaulters),
#    EAR_FN  (Out of defaulters not reached, THB), int_FN (Int x Out of those),
#    marg_exp (Out reached per extra follow-up between K-1 and K, THB)
#
#  SELECTION uses the ex-ante ROLLING run only (fit 2020 -> score 2021): per
#  model, the rule with the highest mean exp_capture over K = 5..35% is written
#  to fi03_selection.csv before any 2022 number is computed.
#
#  2022 uncertainty: Poisson bootstrap over sub-districts (ADM3), B = 1000,
#  follow-up lists held fixed; paired contrasts use the same weights.
#
#  Output: fi_out/fi03_frontier.csv, fi03_selection.csv, fi03_boot.csv,
#          fi03_contrasts.csv, fi03_tau_star_volume.csv, fi03_log.txt
###############################################################################

suppressPackageStartupMessages(library(data.table))

# Money columns from the UN-CAPPED file: 3_fin3y20_22.txt carries Out
# winsorised at 1,016,776.44 THB (Gong, 19 Oct 2026).
RAW_FILE <- "1_data_before_out.txt"
RUNS <- list(
  exante_rolling = list(dir = file.path("factorial7_out", "rolling_2020_to_2021", "exante", "pred"), year = 2L),
  exante_main    = list(dir = file.path("factorial7_out", "exante", "pred"),                         year = 3L)
)
# Study 3 uses the ex-ante model ONLY (Gong, 19 Oct 2026). The published F+SP
# model (year-end features) is not run through this script: the price of
# deciding at the start of the season is one number (XGB 2022 AUC 0.9077 ->
# 0.8970), cited in the text, not a set of tables. Leaving it out also keeps
# FI from reprinting quantities that belong to the Forecasting and ICMLC papers.
SEL_RUN <- "exante_rolling"
REP_RUN <- "exante_main"
CMP_RUN <- "full_main"
CELLS   <- c("XGB__F_SP", "LR__F_SP")
RULES   <- c("R0_TIER", "R1_PD", "R2_EXP", "R3_EL")
K_GRID  <- 1:40
SEL_K   <- 5:35
BOOT_K  <- c(5, 10, 13, 15, 19, 20, 25, 30, 35, 38, 40)   # 13/19/38 = feasible capacity (fi_04)
B       <- 1000
LGD     <- 1
MRR     <- 0.06575
TAU_STAR <- MRR / (MRR + LGD)
SEED    <- 20261019
OUT_DIR <- "fi_out"

dir.create(OUT_DIR, showWarnings = FALSE)
LOG <- file.path(OUT_DIR, "fi03_log.txt")
cat("", file = LOG)
say <- function(...) {
  s <- sprintf(...); cat(s, "\n", sep = ""); cat(s, "\n", sep = "", file = LOG, append = TRUE)
}
bn <- function(x) sprintf("%.2f bn", x / 1e9)
say("fi_03_capacity_frontier  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

## --------------------------------------------------------------- data
raw <- fread(RAW_FILE, sep = "|", showProgress = TRUE,
             select = c("Year", "ID", "Y", "Out", "Outb", "Int", "Previous_Default"))
say("money file %s: %s rows, max Out %s THB", RAW_FILE,
    format(nrow(raw), big.mark = ","), format(round(max(raw$Out)), big.mark = ","))

build <- function(run) {
  R  <- RUNS[[run]]
  kt <- as.data.table(readRDS(file.path(R$dir, "keys_test.rds")))
  setnames(kt, tolower(names(kt)))
  kt[, ord := .I]
  d <- merge(kt, raw[Year == R$year], by.x = "id", by.y = "ID", all.x = TRUE, sort = FALSE)
  setorder(d, ord)
  if (d[is.na(Out), .N] > 0) stop("unmatched rows in ", run)
  if (d[Y != y, .N] > 0) stop("outcome in ", RAW_FILE, " disagrees with keys_test in ", run)
  for (cell in CELLS) {
    f <- file.path(OUT_DIR, sprintf("fi02_pd_%s_%s.rds", run, cell))
    if (!file.exists(f)) stop("missing ", f, " -- run fi_02 first")
    d[[paste0("pd_", cell)]] <- readRDS(f)
  }
  d
}

rank_by <- function(d, rule, cell, tie) {
  p <- d[[paste0("pd_", cell)]]
  o <- switch(rule,
    R0_TIER = order(-d$Int, -(d$Previous_Default == 1), -d$Outb, tie),
    R1_PD   = order(-p, tie),
    R2_EXP  = order(-(p * d$Outb), tie),
    R3_EL   = order(-(p * d$Outb * (LGD + d$Int)), tie))
  r <- integer(nrow(d)); r[o] <- seq_along(o)
  r
}

frontier_one <- function(d, r, run, cell, rule) {
  N  <- nrow(d)
  o  <- order(r)
  y  <- d$y[o]; ex <- d$Out[o] * y; it <- d$Int[o] * d$Out[o] * y
  cy <- cumsum(y); ce <- cumsum(ex); ci <- cumsum(it)
  P  <- sum(y); E <- sum(ex); I <- sum(it)
  nK <- floor(K_GRID / 100 * N)
  out <- data.table(run = run, cell = if (rule == "R0_TIER") "none" else cell,
                    rule = rule, K = K_GRID, n_follow = nK,
                    tp = cy[nK], recall = cy[nK] / P,
                    exp_reached = ce[nK], exp_capture = ce[nK] / E,
                    EAR_FN = E - ce[nK], int_FN = I - ci[nK],
                    defaulters = P, exposure_def = E)
  out[, marg_exp := c(exp_reached[1], diff(exp_reached)) / c(n_follow[1], diff(n_follow))]
  out
}

## ------------------------------------------------------ frontiers
set.seed(SEED)
avail <- names(RUNS)[vapply(RUNS, function(R) file.exists(file.path(R$dir, "keys_test.rds")), logical(1))]
if (!all(c(SEL_RUN, REP_RUN) %in% avail))
  stop("the ex-ante runs are missing -- run fi_01_exante.bat, then fi_02, then this")
dat <- list(); fr <- list(); ranks <- list()
for (run in avail) {
  d <- build(run); dat[[run]] <- d
  tie <- runif(nrow(d))
  say("\n== %s: %s borrowers, %s defaulters holding %s on 31 Dec ==", run,
      format(nrow(d), big.mark = ","), format(sum(d$y), big.mark = ","),
      bn(sum(d$Out * d$y)))
  for (cell in CELLS) for (rule in RULES) {
    if (rule == "R0_TIER" && cell != CELLS[1]) next
    r <- rank_by(d, rule, cell, tie)
    ranks[[paste(run, cell, rule)]] <- r
    fr[[length(fr) + 1]] <- frontier_one(d, r, run, cell, rule)
  }
}
fr <- rbindlist(fr)
fwrite(fr, file.path(OUT_DIR, "fi03_frontier.csv"))

## ------------------------------------------- selection: ex-ante 2021 ONLY
sel <- fr[run == SEL_RUN & K %in% SEL_K & rule != "R0_TIER",
          .(mean_exp_capture = mean(exp_capture), mean_recall = mean(recall)),
          by = .(cell, rule)][order(cell, -mean_exp_capture)]
sel[, selected := seq_len(.N) == 1L, by = cell]
fwrite(sel, file.path(OUT_DIR, "fi03_selection.csv"))
say("\n== selection on %s (mean over K = %d..%d%%) ==", SEL_RUN, min(SEL_K), max(SEL_K))
for (i in seq_len(nrow(sel)))
  say("  %-10s %-7s exposure capture %.4f  recall %.4f %s", sel$cell[i], sel$rule[i],
      sel$mean_exp_capture[i], sel$mean_recall[i], if (sel$selected[i]) "<- selected" else "")
chosen <- sel[selected == TRUE, setNames(rule, cell)]

## ------------------------------------------- 2022, reported once
for (rr in intersect(c(REP_RUN, CMP_RUN), avail)) {
  say("\n== 2022, %s: exposure left among defaulters not followed up ==", rr)
  show <- fr[fr$run == rr & fr$K %in% BOOT_K]
  for (k in BOOT_K) {
    say("K = %2d%%", k)
    s <- show[K == k][order(cell, rule)]
    for (i in seq_len(nrow(s)))
      say("   %-10s %-7s recall %.4f  exposure capture %.4f  EAR_FN %s  interest on it %s  marginal %s THB/follow-up%s",
          s$cell[i], s$rule[i], s$recall[i], s$exp_capture[i], bn(s$EAR_FN[i]),
          bn(s$int_FN[i]), format(round(s$marg_exp[i]), big.mark = ","),
          if (s$cell[i] %in% names(chosen) && chosen[[s$cell[i]]] == s$rule[i]) "  [selected]" else "")
  }
}

tv <- rbindlist(lapply(avail, function(run) rbindlist(lapply(CELLS, function(cell) {
  p <- dat[[run]][[paste0("pd_", cell)]]
  data.table(run = run, cell = cell, share_flagged_at_tau_star = mean(p >= TAU_STAR))
}))))
fwrite(tv, file.path(OUT_DIR, "fi03_tau_star_volume.csv"))
say("\nfollow-up volume implied by corrected PD >= tau* = %.4f (ICMLC rule):", TAU_STAR)
for (i in seq_len(nrow(tv)))
  say("   %-15s %-10s %.1f%% of the portfolio", tv$run[i], tv$cell[i], 100 * tv$share_flagged_at_tau_star[i])

## --------------------------------------------- bootstrap on 2022 clusters
say("\n== bootstrap: %d Poisson draws over ADM3, follow-up lists fixed ==", B)
d  <- dat[[REP_RUN]]
if (CMP_RUN %in% avail) stopifnot(identical(d$id, dat[[CMP_RUN]]$id))
cl <- as.integer(factor(d$adm3)); G <- max(cl)
W  <- matrix(rpois(G * B, 1), nrow = G)
dc <- d[, .(P = sum(y), E = sum(Out * y)), by = .(cl = cl)][order(cl)]
Pg <- numeric(G); Eg <- numeric(G); Pg[dc$cl] <- dc$P; Eg[dc$cl] <- dc$E
Pb <- colSums(W * Pg); Eb <- colSums(W * Eg)
N  <- nrow(d); dy <- d$y == 1

boot_keys <- grep(paste0("^(", REP_RUN, "|", CMP_RUN, ") "), names(ranks), value = TRUE)
bt <- list(); draws <- list()
q <- function(v) quantile(v, c(.025, .975), names = FALSE)
for (kk in boot_keys) {
  r <- ranks[[kk]]; parts <- strsplit(kk, " ")[[1]]
  for (k in BOOT_K) {
    hit <- dy & r <= floor(k / 100 * N)
    a <- data.table(cl = cl[hit], ex = d$Out[hit])[, .(tp = .N, ex = sum(ex)), by = cl]
    tg <- numeric(G); eg <- numeric(G); tg[a$cl] <- a$tp; eg[a$cl] <- a$ex
    tb <- colSums(W * tg); ebb <- colSums(W * eg)
    draws[[paste(kk, k)]] <- list(recall = tb / Pb, cap = ebb / Eb, ear = Eb - ebb)
    bt[[length(bt) + 1]] <- data.table(
      run = parts[1], cell = if (parts[3] == "R0_TIER") "none" else parts[2], rule = parts[3], K = k,
      recall_lo = q(tb / Pb)[1], recall_hi = q(tb / Pb)[2],
      cap_lo = q(ebb / Eb)[1], cap_hi = q(ebb / Eb)[2],
      EAR_lo = q(Eb - ebb)[1], EAR_hi = q(Eb - ebb)[2])
  }
}
bt <- rbindlist(bt)

pair <- function(a, b, k, lab) {
  A <- draws[[paste(a, k)]]; Bq <- draws[[paste(b, k)]]
  if (is.null(A) || is.null(Bq)) return(NULL)
  dcap <- A$cap - Bq$cap; dear <- A$ear - Bq$ear; drec <- A$recall - Bq$recall
  data.table(contrast = lab, K = k,
             d_recall_lo = q(drec)[1], d_recall_hi = q(drec)[2],
             d_cap_lo = q(dcap)[1], d_cap_hi = q(dcap)[2],
             d_EAR_lo = q(dear)[1], d_EAR_hi = q(dear)[2])
}
k_ <- function(run, cell, rule) paste(run, cell, rule)
pc <- list()
for (k in BOOT_K) {
  for (cell in CELLS) {
    sr <- chosen[[cell]]
    pc[[length(pc) + 1]] <- pair(k_(REP_RUN, cell, sr), k_(REP_RUN, CELLS[1], "R0_TIER"),
                                 k, sprintf("%s %s - bank grade", cell, sr))
    pc[[length(pc) + 1]] <- pair(k_(REP_RUN, cell, "R2_EXP"), k_(REP_RUN, cell, "R1_PD"),
                                 k, sprintf("%s R2_EXP - R1_PD", cell))
    pc[[length(pc) + 1]] <- pair(k_(CMP_RUN, cell, sr), k_(REP_RUN, cell, sr),
                                 k, sprintf("%s %s full - ex-ante", cell, sr))
  }
  pc[[length(pc) + 1]] <- pair(k_(REP_RUN, CELLS[1], chosen[[CELLS[1]]]),
                               k_(REP_RUN, CELLS[2], chosen[[CELLS[2]]]),
                               k, "ex-ante XGB - LR (selected rules)")
}
pc <- rbindlist(pc)
fwrite(bt, file.path(OUT_DIR, "fi03_boot.csv"))
fwrite(pc, file.path(OUT_DIR, "fi03_contrasts.csv"))
say("paired contrasts on 2022 (95%% bootstrap interval; EAR in bn THB, negative = less exposure left):")
for (i in seq_len(nrow(pc)))
  say("   K=%2d%%  %-38s recall [%+.4f, %+.4f]  capture [%+.4f, %+.4f]  EAR [%+.2f, %+.2f] bn",
      pc$K[i], pc$contrast[i], pc$d_recall_lo[i], pc$d_recall_hi[i],
      pc$d_cap_lo[i], pc$d_cap_hi[i], pc$d_EAR_lo[i] / 1e9, pc$d_EAR_hi[i] / 1e9)

say("\ndone. Paste %s into the chat.", LOG)

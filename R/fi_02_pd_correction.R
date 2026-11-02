###############################################################################
#  fi_02_pd_correction.R  --  Study 3 (Financial Innovation), step 2
#
#  Turns each model's raw score into a corrected probability of default (PD)
#  with the rule recommended by the Study 2 paper (ICMLC #57), so that FI's
#  money-weighted ranking (PD x exposure) multiplies a probability rather than
#  a score that still carries its training weight:
#
#    stage 1  intercept shift: one constant added to the log-odds, chosen so
#             that the mean prediction on the HELD-OUT development rows
#             (keys_dev$in_val) equals the default rate observed there;
#    stage 2  prior shift to the Saerens-Latinne-Decaestecker EM estimate of
#             the deployment default rate, computed from the deployment
#             scores WITHOUT their labels.
#
#  Both stages use only what a lender has on 31 December: development-year
#  labels and the current year's unlabelled scores. The realised default rate
#  of the scored year is printed afterwards as a check and is used for nothing.
#
#  Runs (<set>_<run>):
#    *_rolling  fit 2020 -> score 2021   (SELECTION set: FI chooses things here)
#    *_main     fit 2020+2021 -> score 2022   (reported once, nothing chosen)
#    set = exante (primary) or full (published F+SP, comparator)
#
#  Functions are copied from study2_calibration.R (same EM_MAX, EM_TOL) so the
#  corrected PD is the one the Study 2 paper reports, not a re-implementation.
#
#  Output: fi_out/fi02_pd_<run>_<cell>.rds   corrected PD, keys_test row order
#          fi_out/fi02_summary.csv, fi_out/fi02_log.txt
###############################################################################

suppressPackageStartupMessages(library(data.table))

# Two feature sets (19 Oct 2026). "exante" = refit without the year-end
# features (fi_01_exante.bat); it is FI's primary model because the outcome is
# the NPL status on 31 December, the same date as the year-end balances.
# "full" = the published F+SP fits, kept as the comparator. A set whose folder
# does not exist yet is skipped with a message.
RUNS <- list(
  exante_rolling = file.path("factorial7_out", "rolling_2020_to_2021", "exante", "pred"),
  exante_main    = file.path("factorial7_out", "exante", "pred")
)
# Study 3 uses the ex-ante model only (Gong, 19 Oct 2026); the full F+SP fits
# belong to the Forecasting and ICMLC papers and are not recalibrated here.
CELLS  <- c("XGB__F_SP", "LR__F_SP")
OUT_DIR <- "fi_out"
EM_MAX <- 500
EM_TOL <- 1e-10

dir.create(OUT_DIR, showWarnings = FALSE)
LOG <- file.path(OUT_DIR, "fi02_log.txt")
cat("", file = LOG)
say <- function(...) {
  s <- sprintf(...); cat(s, "\n", sep = ""); cat(s, "\n", sep = "", file = LOG, append = TRUE)
}
say("fi_02_pd_correction  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

## ---------------------------------------------- from study2_calibration.R
clip   <- function(p, e = 1e-12) pmin(pmax(p, e), 1 - e)
logit  <- function(p) log(clip(p) / (1 - clip(p)))
sigm   <- function(z) 1 / (1 + exp(-z))
odds   <- function(p) clip(p) / (1 - clip(p))
unodds <- function(o) o / (1 + o)
shift_prior <- function(p, pi_src, pi_tgt) {
  r <- (pi_tgt / (1 - pi_tgt)) / (pi_src / (1 - pi_src))
  unodds(odds(p) * r)
}
em_prior <- function(p, pi_src, max_it = EM_MAX, tol = EM_TOL) {
  pi_hat <- pi_src
  for (i in seq_len(max_it)) {
    q      <- shift_prior(p, pi_src, pi_hat)
    pi_new <- mean(q)
    if (!is.finite(pi_new)) return(list(pi = NA_real_, it = i, ok = FALSE))
    if (abs(pi_new - pi_hat) < tol) return(list(pi = pi_new, it = i, ok = TRUE))
    pi_hat <- pi_new
  }
  list(pi = pi_hat, it = max_it, ok = FALSE)
}
intercept_map <- function(pv, yv) {
  tgt <- mean(yv)
  f   <- function(c) mean(sigm(logit(pv) + c)) - tgt
  c   <- tryCatch(uniroot(f, c(-20, 20))$root, error = function(e) NA_real_)
  if (is.na(c)) stop("intercept search failed")
  list(c = c, f = function(pt) sigm(logit(pt) + c))
}
fast_auc <- function(score, y) {
  n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  r  <- rank(score, ties.method = "average")
  (sum(as.numeric(r[y == 1])) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

## ------------------------------------------------------------------ run
summ <- list()
for (run in names(RUNS)) {
  pd_dir <- RUNS[[run]]
  if (!file.exists(file.path(pd_dir, "keys_test.rds"))) {
    say("\n== %s ==  %s not found -- skipped (run fi_01_exante.bat first?)", run, pd_dir)
    next
  }
  kd <- as.data.table(readRDS(file.path(pd_dir, "keys_dev.rds")))
  kt <- as.data.table(readRDS(file.path(pd_dir, "keys_test.rds")))
  setnames(kd, tolower(names(kd))); setnames(kt, tolower(names(kt)))
  in_val <- as.logical(kd$in_val)
  pi_dev <- mean(kd$y[in_val])
  say("\n== %s ==  dev rows %s (held out %s, default %.4f%%)  scored rows %s",
      run, format(nrow(kd), big.mark = ","), format(sum(in_val), big.mark = ","),
      100 * pi_dev, format(nrow(kt), big.mark = ","))
  for (cell in CELLS) {
    p_dev <- as.numeric(readRDS(file.path(pd_dir, sprintf("dev_%s.rds", cell))))
    p_tst <- as.numeric(readRDS(file.path(pd_dir, sprintf("pred_%s.rds", cell))))
    stopifnot(length(p_dev) == nrow(kd), length(p_tst) == nrow(kt))

    m1  <- intercept_map(p_dev[in_val], kd$y[in_val])       # stage 1
    p1  <- m1$f(p_tst)
    em  <- em_prior(p1, pi_dev)                              # stage 2, unlabelled
    if (!em$ok) warning(run, " ", cell, ": EM did not converge")
    p2  <- shift_prior(p1, pi_dev, em$pi)

    saveRDS(p2, file.path(OUT_DIR, sprintf("fi02_pd_%s_%s.rds", run, cell)))

    realised <- mean(kt$y)                                   # CHECK ONLY
    s <- data.table(run = run, cell = cell, n = length(p2),
                    pi_dev = pi_dev, stage1_shift = m1$c,
                    mean_raw = mean(p_tst), mean_stage1 = mean(p1),
                    em_prior = em$pi, em_iter = em$it, mean_pd = mean(p2),
                    realised_rate_check = realised,
                    pd_ratio_check = mean(p2) / realised,
                    auc_raw = fast_auc(p_tst, kt$y), auc_pd = fast_auc(p2, kt$y))
    summ[[length(summ) + 1]] <- s
    say("%-10s stage-1 shift %+.4f | mean raw %.4f -> stage 1 %.4f -> EM prior %.4f (%d it) | realised %.4f, PD ratio %.3f | AUC %.6f -> %.6f",
        cell, s$stage1_shift, s$mean_raw, s$mean_stage1, s$em_prior, s$em_iter,
        s$realised_rate_check, s$pd_ratio_check, s$auc_raw, s$auc_pd)
  }
}
summ <- rbindlist(summ)
fwrite(summ, file.path(OUT_DIR, "fi02_summary.csv"))
say("\nAUC must be unchanged by the correction (both stages are monotone).")
say("realised_rate_check and pd_ratio_check are reported, never used.")
say("done. Paste %s into the chat.", LOG)

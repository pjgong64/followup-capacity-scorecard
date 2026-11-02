###############################################################################
#  fi_05_mixed_policy.R  --  Study 3 (Financial Innovation), step 5
#
#  Why (fi_04, 26 Oct 2026): ranking by PD x exposure reaches 84% of the money
#  at risk but follows up almost none of the smallest borrowers -- at K = 20%
#  only 0.1% of the bottom loan-size quintile, whose 40,364 defaulters are 22%
#  of all 2022 defaulters. For a financial-inclusion reader that is the first
#  objection. This script prices the remedy.
#
#  Mixed policy: of the K% the bank can follow up,
#     (1 - s) go to the MONEY track  -- rule selected in fi_03 (XGB R3, LR R2)
#     s       go to the COUNT track  -- corrected PD, among those not yet chosen
#  Two variants of the count track:
#     V1  open    : any borrower
#     V2  small   : only borrowers whose opening balance Outb is at or below the
#                   40th percentile of the scored year (bottom two quintiles).
#                   Outb is known on the decision date; no outcome is used.
#  s in {0, .1, .2, .3, .4, .5, 1}; K in {13, 19, 38} (feasible capacity for 3,
#  2 and 1 visits per borrower, fi_04) plus 10/20/30.
#
#  Reported per model x variant x K x s, on both years:
#     recall, exposure capture, EAR_FN; small-loan defaulters reached and the
#     small-loan recall; cost of inclusion = extra EAR per extra small-loan
#     defaulter reached, relative to s = 0 (THB per defaulter).
#
#  Recommended s (a policy choice, made on 2021 ONLY): the smallest s at which
#  the mixed policy reaches at least as many small-loan defaulters (relative)
#  as the bank's own grade (R0_TIER) does at the same K. Its 2022 outcome is
#  then reported, with a Poisson bootstrap over ADM3 (B = 1000) for the
#  contrast against s = 0 and against the bank grade.
#
#  Output: fi_out/fi05_mixed.csv, fi05_recommended.csv, fi05_log.txt
###############################################################################

suppressPackageStartupMessages(library(data.table))

RAW_FILE <- "1_data_before_out.txt"
RUNS <- list(
  exante_rolling = list(dir = file.path("factorial7_out", "rolling_2020_to_2021", "exante", "pred"), year = 2L),
  exante_main    = list(dir = file.path("factorial7_out", "exante", "pred"),                         year = 3L)
)
CELLS  <- c("XGB__F_SP", "LR__F_SP")
K_SET  <- c(10, 13, 19, 20, 30, 38)
S_SET  <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1)
SMALL_Q <- 0.40
LGD0   <- 1
B      <- 1000
SEED   <- 20261026
OUT_DIR <- "fi_out"

`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
LOG <- file.path(OUT_DIR, "fi05_log.txt"); cat("", file = LOG)
say <- function(...) { s <- sprintf(...); cat(s, "\n", sep = ""); cat(s, "\n", sep = "", file = LOG, append = TRUE) }
bn  <- function(x) sprintf("%.2f bn", x / 1e9)
say("fi_05_mixed_policy  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

sel <- fread(file.path(OUT_DIR, "fi03_selection.csv"))[selected == TRUE]
chosen <- setNames(sel$rule, sel$cell)
say("money track = rule selected on 2021 by fi_03: %s",
    paste(names(chosen), chosen, sep = " -> ", collapse = ", "))

raw <- fread(RAW_FILE, sep = "|", showProgress = TRUE,
             select = c("Year", "ID", "Y", "Out", "Outb", "Int", "Previous_Default"))
build <- function(run) {
  R  <- RUNS[[run]]
  kt <- as.data.table(readRDS(file.path(R$dir, "keys_test.rds"))); setnames(kt, tolower(names(kt)))
  kt[, ord := .I]
  d <- merge(kt, raw[Year == R$year], by.x = "id", by.y = "ID", all.x = TRUE, sort = FALSE)
  setorder(d, ord)
  if (d[is.na(Out) | Y != y, .N] > 0) stop("join check failed for ", run)
  for (cell in CELLS) d[[paste0("pd_", cell)]] <- readRDS(file.path(OUT_DIR, sprintf("fi02_pd_%s_%s.rds", run, cell)))
  d[, small := Outb <= quantile(Outb, SMALL_Q)]
  d
}
money_score <- function(d, cell) {
  p <- d[[paste0("pd_", cell)]]
  switch(chosen[[cell]], R2_EXP = p * d$Outb, R3_EL = p * d$Outb * (LGD0 + d$Int), R1_PD = p)
}

# indicator of who is followed up under (K, s, variant)
pick <- function(d, cell, k, s, variant, o_money, o_pd) {
  N  <- nrow(d); nK <- floor(k / 100 * N)
  n_count <- round(s * nK); n_money <- nK - n_count
  f <- logical(N)
  if (n_money > 0) f[o_money[seq_len(n_money)]] <- TRUE
  if (n_count > 0) {
    elig <- !f
    if (variant == "V2_small") elig <- elig & d$small
    cand <- o_pd[elig[o_pd]]
    if (length(cand) < n_count) {                 # small pool exhausted: top up from money track
      f[cand] <- TRUE
      rest <- o_money[!f[o_money]]
      f[rest[seq_len(n_count - length(cand))]] <- TRUE
    } else f[cand[seq_len(n_count)]] <- TRUE
  }
  f
}
summ <- function(d, f) {
  y <- d$y == 1
  data.table(followed = sum(f), recall = sum(y & f) / sum(y),
             cap = sum(d$Out[y & f]) / sum(d$Out[y]),
             EAR = sum(d$Out[y]) - sum(d$Out[y & f]),
             small_def_reached = sum(y & f & d$small),
             small_recall = sum(y & f & d$small) / sum(y & d$small),
             small_followed_share = sum(f & d$small) / sum(d$small))
}

set.seed(SEED)
dat <- lapply(names(RUNS), build); names(dat) <- names(RUNS)
res <- list(); flags <- list()
for (run in names(RUNS)) {
  d <- dat[[run]]; tie <- runif(nrow(d))
  say("\n== %s: small loans = Outb <= %s THB (%s borrowers, %s defaulters holding %s) ==", run,
      format(round(quantile(d$Outb, SMALL_Q)), big.mark = ","), format(sum(d$small), big.mark = ","),
      format(sum(d$small & d$y == 1), big.mark = ","), bn(sum(d$Out[d$small & d$y == 1])))
  o_tier <- order(-d$Int, -(d$Previous_Default == 1), -d$Outb, tie)
  for (k in K_SET) {                              # bank grade reference
    f <- logical(nrow(d)); f[o_tier[seq_len(floor(k / 100 * nrow(d)))]] <- TRUE
    res[[length(res) + 1]] <- cbind(run = run, cell = "none", variant = "bank_grade", K = k, s = NA_real_, summ(d, f))
  }
  for (cell in CELLS) {
    o_money <- order(-money_score(d, cell), tie)
    o_pd    <- order(-d[[paste0("pd_", cell)]], tie)
    for (variant in c("V1_open", "V2_small")) for (k in K_SET) for (s in S_SET) {
      if (variant == "V2_small" && s == 0) next   # identical to V1 at s = 0
      f <- pick(d, cell, k, s, variant, o_money, o_pd)
      res[[length(res) + 1]] <- cbind(run = run, cell = cell, variant = if (s == 0) "money_only" else variant,
                                      K = k, s = s, summ(d, f))
      if (run == "exante_main" && k == 19) flags[[paste(cell, variant, k, s)]] <- f
    }
  }
}
res <- rbindlist(res)
base <- res[variant == "money_only", .(run, cell, K, EAR0 = EAR, small0 = small_def_reached, rec0 = recall)]
res <- merge(res, base, by = c("run", "cell", "K"), all.x = TRUE)
res[, `:=`(extra_EAR = EAR - EAR0, extra_small = small_def_reached - small0,
           extra_heads = (recall - rec0))]
res[, thb_per_small := fifelse(extra_small > 0, extra_EAR / extra_small, NA_real_)]
fwrite(res, file.path(OUT_DIR, "fi05_mixed.csv"))

## ------------------------------------------------- frontier, printed
for (rr in names(RUNS)) for (cc in CELLS) for (k in c(13, 19, 38)) {
  bg <- res[res$run == rr & res$cell == "none" & res$K == k]
  say("\n-- %s %s K=%d%%  (bank grade: recall %.3f, small-loan recall %.3f, EAR %s)", rr, cc, k,
      bg$recall, bg$small_recall, bn(bg$EAR))
  s_ <- res[res$run == rr & res$cell == cc & res$K == k][order(variant, s)]
  for (i in seq_len(nrow(s_)))
    say("   %-10s s=%.1f  recall %.3f  capture %.3f  EAR %s  small reached %s (recall %.3f)  +EAR %s  THB per extra small defaulter %s",
        s_$variant[i], s_$s[i], s_$recall[i], s_$cap[i], bn(s_$EAR[i]),
        format(s_$small_def_reached[i], big.mark = ","), s_$small_recall[i], bn(s_$extra_EAR[i]),
        ifelse(is.na(s_$thb_per_small[i]), "-", format(round(s_$thb_per_small[i]), big.mark = ",")))
}

## ------------------------------------- recommended s, chosen on 2021
rec <- list()
for (cc in CELLS) for (vv in c("V1_open", "V2_small")) for (k in K_SET) {
  r21 <- res[res$run == "exante_rolling" & res$cell == cc & res$variant %in% c("money_only", vv) & res$K == k][order(s)]
  target <- res[res$run == "exante_rolling" & res$cell == "none" & res$K == k]$small_recall
  ok <- r21[r21$small_recall >= target]
  s_star <- if (nrow(ok)) min(ok$s) else NA_real_
  r22 <- if (is.na(s_star)) NULL else res[res$run == "exante_main" & res$cell == cc & res$K == k &
                                          res$variant == (if (s_star == 0) "money_only" else vv) &
                                          !is.na(res$s) & abs(res$s - s_star) < 1e-9]
  bg22 <- res[res$run == "exante_main" & res$cell == "none" & res$K == k]
  m022 <- res[res$run == "exante_main" & res$cell == cc & res$K == k & res$variant == "money_only"]
  rec[[length(rec) + 1]] <- data.table(cell = cc, variant = vv, K = k, s_star_2021 = s_star,
    target_small_recall_2021 = target,
    recall_2022 = r22$recall %||% NA_real_, small_recall_2022 = r22$small_recall %||% NA_real_,
    EAR_2022 = r22$EAR %||% NA_real_, EAR_money_only_2022 = m022$EAR, EAR_bank_2022 = bg22$EAR,
    small_recall_bank_2022 = bg22$small_recall)
}
rec <- rbindlist(rec)
fwrite(rec, file.path(OUT_DIR, "fi05_recommended.csv"))
say("\n== recommended share s* (smallest s matching the bank grade's small-loan recall, chosen on 2021) ==")
for (i in seq_len(nrow(rec)))
  say("   %-10s %-9s K=%2d%%  s*=%s  2022: recall %.3f  small recall %.3f (bank %.3f)  EAR %s (money-only %s, bank %s)",
      rec$cell[i], rec$variant[i], rec$K[i], ifelse(is.na(rec$s_star_2021[i]), "none", sprintf("%.1f", rec$s_star_2021[i])),
      rec$recall_2022[i], rec$small_recall_2022[i], rec$small_recall_bank_2022[i],
      bn(rec$EAR_2022[i]), bn(rec$EAR_money_only_2022[i]), bn(rec$EAR_bank_2022[i]))

## ----------------------------------------- bootstrap at K = 19 on 2022
say("\n== bootstrap (2022, K = 19%%): recommended mixed policy vs money-only and vs bank grade ==")
d <- dat$exante_main; cl <- as.integer(factor(d$adm3)); G <- max(cl)
W <- matrix(rpois(G * B, 1), nrow = G)
yy <- d$y == 1
agg <- function(v) { g <- numeric(G); a <- tapply(v, cl, sum); g[as.integer(names(a))] <- a; colSums(W * g) }
Eb <- agg(d$Out * yy); Sb <- agg(as.numeric(yy & d$small))
o_tier <- order(-d$Int, -(d$Previous_Default == 1), -d$Outb, runif(nrow(d)))
f_bank <- logical(nrow(d)); f_bank[o_tier[seq_len(floor(0.19 * nrow(d)))]] <- TRUE
q <- function(v) sprintf("[%+.2f, %+.2f]", quantile(v, .025) / 1e9, quantile(v, .975) / 1e9)
qn <- function(v) sprintf("[%+.0f, %+.0f]", quantile(v, .025), quantile(v, .975))
for (cc in CELLS) for (vv in c("V1_open", "V2_small")) {
  ss <- rec[rec$cell == cc & rec$variant == vv & rec$K == 19]$s_star_2021
  if (is.na(ss)) { say("   %s %s: no s reaches the target on 2021", cc, vv); next }
  f_mix <- flags[[paste(cc, if (ss == 0) "V1_open" else vv, 19, ss)]]
  f_m0  <- flags[[paste(cc, "V1_open", 19, 0)]]
  ear <- function(f) Eb - agg(d$Out * (yy & f)); sm <- function(f) agg(as.numeric(yy & f & d$small))
  say("   %s %s s*=%.1f  vs money-only: EAR %s bn, small defaulters reached %s | vs bank grade: EAR %s bn, small reached %s",
      cc, vv, ss, q(ear(f_mix) - ear(f_m0)), qn(sm(f_mix) - sm(f_m0)),
      q(ear(f_mix) - ear(f_bank)), qn(sm(f_mix) - sm(f_bank)))
}
say("\ndone. Paste %s into the chat.", LOG)

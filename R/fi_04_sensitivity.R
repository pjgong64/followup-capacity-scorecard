###############################################################################
#  fi_04_sensitivity.R  --  Study 3 (Financial Innovation), step 4
#
#  Ex-ante models only. Reads fi02 PDs and the rule selected in fi03 (2021).
#
#  A. Loan-size groups (financial-inclusion check). Borrowers are split into
#     quintiles of the opening balance Outb (known at the decision point). For
#     each rule at K = 10/20/30%: share of the group followed up, recall and
#     exposure capture within the group. A money-weighted list is expected to
#     favour larger loans; this measures by how much.
#  B. Large-loan robustness. The primary analysis keeps every borrower (Gong,
#     19 Oct 2026: the largest balances are genuine individual borrowers, and
#     reaching one of them is worth a great deal). As a sensitivity only:
#       B1 drop the top 0.1% by Outb and re-rank;
#       B2 cap EAD at 1,016,776 THB (the cap used in 3_fin3y20_22.txt).
#     Reported: does the ordering of the rules, and the size of their gaps,
#     survive?
#  C. LGD and MRR. tau* = MRR / (MRR + LGD) and the follow-up volume it implies
#     on the corrected PD, over LGD in {0.2..1.0} x MRR in {4.575..8.575%};
#     expected principal loss left = LGD x EAR_FN, plus contractual interest.
#  D. Break-even follow-up cost. A follow-up of a defaulter saves a fraction
#     theta of the exposure reached (unknown; swept 0.05..0.30). The marginal
#     follow-up at volume K is worth theta x LGD x marg_exp(K) baht. For a
#     follow-up cost c, the recommended K stops at the first K whose marginal value
#     falls below c. K is CHOSEN ON 2021 (rolling) and its outcome REPORTED
#     ON 2022 -- the same selection rule as fi_03.
#
#  Output: fi_out/fi04_*.csv, fi_out/fi04_log.txt
###############################################################################

suppressPackageStartupMessages(library(data.table))

RAW_FILE <- "1_data_before_out.txt"
RUNS <- list(
  exante_rolling = list(dir = file.path("factorial7_out", "rolling_2020_to_2021", "exante", "pred"), year = 2L),
  exante_main    = list(dir = file.path("factorial7_out", "exante", "pred"),                         year = 3L)
)
CELLS  <- c("XGB__F_SP", "LR__F_SP")
K_GRID <- 1:40
K_SHOW <- c(10, 20, 30)
LGD0   <- 1
MRR0   <- 0.06575
LGD_GRID <- c(0.2, 0.4, 0.6, 0.8, 1.0)
MRR_GRID <- MRR0 + c(-0.02, -0.01, 0, 0.01, 0.02)
THETA  <- c(0.05, 0.10, 0.20, 0.30)
# THB per follow-up. BASE = 500 (Gong, 20 Oct 2026, BAAC operating figures):
# officer salary ~1,300 + allowance 300 + fuel 400 = 2,000 THB per field day,
# at a standard 4 follow-ups per day. 250-1,000 brackets 2-8 cases a day.
COST   <- c(250, 500, 750, 1000, 2000, 5000)
COST_BASE <- 500
CASES_PER_DAY <- 4
# Staff (Gong, 20 Oct 2026): ~3,000 field officers nationwide for ~4.5 million
# borrowers; this panel is ~1/3 of them, so ~1,000 officer-equivalents serve it
# under a pro-rata assumption (stated as an assumption in the paper).
OFFICERS <- 3000 * (1496721 / 4.5e6)
# Field time (Gong, 20 Oct 2026): ~3 days a week on follow-up, ~48 working
# weeks -> ~144 field days per officer per year. A borrower may need more than
# one visit, so the feasible share of the portfolio is shown for 1-3 visits.
FIELD_DAYS <- 3 * 48
VISITS <- 1:3
CAP    <- 1016776.44
SEED   <- 20261019
OUT_DIR <- "fi_out"

LOG <- file.path(OUT_DIR, "fi04_log.txt"); cat("", file = LOG)
say <- function(...) { s <- sprintf(...); cat(s, "\n", sep = ""); cat(s, "\n", sep = "", file = LOG, append = TRUE) }
bn  <- function(x) sprintf("%.2f bn", x / 1e9)
say("fi_04_sensitivity  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

sel_f <- file.path(OUT_DIR, "fi03_selection.csv")
if (!file.exists(sel_f)) stop("run fi_03 first (", sel_f, " missing)")
sel <- fread(sel_f)[selected == TRUE]
chosen <- setNames(sel$rule, sel$cell)
say("rules selected on 2021 by fi_03: %s", paste(names(chosen), chosen, sep = " -> ", collapse = ", "))

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
  d
}
score_of <- function(d, rule, cell, lgd = LGD0) {
  p <- d[[paste0("pd_", cell)]]
  switch(rule, R1_PD = p, R2_EXP = p * d$Outb, R3_EL = p * d$Outb * (lgd + d$Int))
}
rank_of <- function(d, rule, cell, tie, lgd = LGD0) {
  o <- if (rule == "R0_TIER") order(-d$Int, -(d$Previous_Default == 1), -d$Outb, tie)
       else order(-score_of(d, rule, cell, lgd), tie)
  r <- integer(nrow(d)); r[o] <- seq_along(o); r
}
curve <- function(d, r, ead) {                     # cumulative over K_GRID
  o <- order(r); y <- d$y[o]; e <- ead[o] * y; it <- (d$Int * ead)[o] * y
  nK <- floor(K_GRID / 100 * nrow(d))
  data.table(K = K_GRID, n = nK, recall = cumsum(y)[nK] / sum(y),
             cap = cumsum(e)[nK] / sum(e), EAR = sum(e) - cumsum(e)[nK],
             INT = sum(it) - cumsum(it)[nK],
             marg = c(cumsum(e)[nK][1], diff(cumsum(e)[nK])) / c(nK[1], diff(nK)))
}

set.seed(SEED)
dat <- lapply(names(RUNS), build); names(dat) <- names(RUNS)
ties <- lapply(dat, function(d) runif(nrow(d)))
RULES_ALL <- c("R0_TIER", "R1_PD", "R2_EXP", "R3_EL")

## ------------------------------------------------------------- A. size groups
say("\n== A. 2022 by quintile of opening balance Outb ==")
d <- dat$exante_main; tie <- ties$exante_main
d[, q := cut(frank(Outb, ties.method = "first"), 5, labels = paste0("Q", 1:5))]
qinfo <- d[, .(n = .N, outb_min = min(Outb), outb_max = max(Outb), defaulters = sum(y),
               exposure_def = sum(Out * y)), by = q][order(q)]
for (i in seq_len(nrow(qinfo)))
  say("  %s  Outb %s..%s THB  n=%s  defaulters %s  their Out %s", qinfo$q[i],
      format(round(qinfo$outb_min[i]), big.mark = ","), format(round(qinfo$outb_max[i]), big.mark = ","),
      format(qinfo$n[i], big.mark = ","), format(qinfo$defaulters[i], big.mark = ","), bn(qinfo$exposure_def[i]))
A <- list()
for (cell in CELLS) for (rule in unique(c("R1_PD", chosen[[cell]], "R0_TIER"))) {
  if (rule == "R0_TIER" && cell != CELLS[1]) next
  r <- rank_of(d, rule, cell, tie)
  for (k in K_SHOW) {
    f <- r <= floor(k / 100 * nrow(d))
    a <- d[, .(followed = mean(f[.I]), recall = sum(y & f[.I]) / sum(y),
               cap = sum(Out * (y & f[.I])) / sum(Out * y)), by = q]
    A[[length(A) + 1]] <- cbind(cell = if (rule == "R0_TIER") "none" else cell, rule = rule, K = k, a)
  }
}
A <- rbindlist(A)[order(cell, rule, K, q)]
fwrite(A, file.path(OUT_DIR, "fi04_size_groups.csv"))
for (k in K_SHOW) {
  say("K = %d%%  (share followed up / recall within group)", k)
  for (key in unique(A[K == k, paste(cell, rule)])) {
    s <- A[K == k & paste(cell, rule) == key]
    say("   %-18s %s", key, paste(sprintf("%s %4.1f%%/%4.1f%%", s$q, 100 * s$followed, 100 * s$recall), collapse = "  "))
  }
}

## ---------------------------------------------- B. large-loan robustness
say("\n== B. large-loan robustness (2022) ==")
B <- list()
variants <- list(
  primary   = list(keep = rep(TRUE, nrow(d)), ead = d$Out),
  drop_top  = list(keep = d$Outb <= quantile(d$Outb, 0.999), ead = d$Out),
  cap_ead   = list(keep = rep(TRUE, nrow(d)), ead = pmin(d$Out, CAP)))
for (v in names(variants)) {
  V <- variants[[v]]; dd <- d[V$keep]; ee <- V$ead[V$keep]; tt <- tie[V$keep]
  for (cell in CELLS) for (rule in RULES_ALL) {
    if (rule == "R0_TIER" && cell != CELLS[1]) next
    cv <- curve(dd, rank_of(dd, rule, cell, tt), ee)[K %in% K_SHOW]
    B[[length(B) + 1]] <- cbind(variant = v, cell = if (rule == "R0_TIER") "none" else cell, rule = rule, cv)
  }
}
B <- rbindlist(B)
fwrite(B, file.path(OUT_DIR, "fi04_large_loans.csv"))
say("variant drop_top removes %s borrowers with Outb > %s THB", format(sum(!variants$drop_top$keep), big.mark = ","),
    format(round(quantile(d$Outb, 0.999)), big.mark = ","))
for (v in names(variants)) for (k in K_SHOW) {
  s <- B[variant == v & K == k][order(cell, rule)]
  say("  %-9s K=%d%%  %s", v, k,
      paste(sprintf("%s/%s cap %.3f EAR %s", s$cell, s$rule, s$cap, bn(s$EAR)), collapse = " | "))
}

## ------------------------------------------------------- C. LGD x MRR
say("\n== C. LGD x MRR: tau*, implied follow-up volume, loss left at that volume (2022) ==")
C <- list()
for (cell in CELLS) {
  p <- d[[paste0("pd_", cell)]]
  for (lgd in LGD_GRID) {
    rule <- chosen[[cell]]
    cv <- curve(d, rank_of(d, rule, cell, tie, lgd), d$Out)
    for (mrr in MRR_GRID) {
      ts <- mrr / (mrr + lgd); vol <- mean(p >= ts)
      k <- max(1L, min(40L, round(100 * vol)))
      row <- cv[K == k]
      C[[length(C) + 1]] <- data.table(cell = cell, rule = rule, LGD = lgd, MRR = mrr, tau_star = ts,
                                       volume = vol, K_used = k, recall = row$recall, cap = row$cap,
                                       principal_loss_left = lgd * row$EAR, interest_left = row$INT)
    }
  }
}
C <- rbindlist(C)
fwrite(C, file.path(OUT_DIR, "fi04_lgd_mrr.csv"))
for (cc in CELLS) {
  say("  %s (%s): follow-up volume implied by tau* [rows LGD, columns MRR %s]", cc, chosen[[cc]],
      paste(sprintf("%.3f%%", 100 * MRR_GRID), collapse = " "))
  for (lgd in LGD_GRID) {
    s <- C[which(C[["cell"]] == cc & C[["LGD"]] == lgd)][order(MRR)]
    say("     LGD %.1f  %s   | principal loss left at MRR 6.575%%: %s", lgd,
        paste(sprintf("%5.1f%%", 100 * s$volume), collapse = " "),
        bn(s[["principal_loss_left"]][abs(s[["MRR"]] - MRR0) < 1e-9]))
  }
}
say("  (volumes above 40%% are evaluated at K = 40%%, the end of the frontier)")

## ----------------------------------------- D. break-even follow-up cost
say("\n== D. recommended K for a follow-up cost c and save-rate theta: chosen on 2021, reported on 2022 ==")
D <- list()
for (cell in CELLS) {
  rule <- chosen[[cell]]
  cv21 <- curve(dat$exante_rolling, rank_of(dat$exante_rolling, rule, cell, ties$exante_rolling), dat$exante_rolling$Out)
  cv22 <- curve(d, rank_of(d, rule, cell, tie), d$Out)
  for (th in THETA) for (c in COST) {
    # stop at the FIRST volume whose marginal follow-up is worth less than c
    below <- which(th * LGD0 * cv21$marg < c)
    k <- if (!length(below)) max(cv21$K) else if (below[1] == 1L) 0L else cv21$K[below[1] - 1L]
    r22 <- if (k > 0) cv22[K == k] else data.table(recall = 0, cap = 0, EAR = sum(d$Out * d$y), marg = NA_real_)
    D[[length(D) + 1]] <- data.table(cell = cell, rule = rule, theta = th, cost = c, K_chosen_2021 = k,
                                     recall_2022 = r22$recall, cap_2022 = r22$cap, EAR_2022 = r22$EAR,
                                     saved_2022 = th * LGD0 * (sum(d$Out * d$y) - r22$EAR),
                                     spend_2022 = c * floor(k / 100 * nrow(d)),
                                     officer_days = floor(k / 100 * nrow(d)) / CASES_PER_DAY)
  }
}
D <- rbindlist(D)
D[, net_2022 := saved_2022 - spend_2022]
fwrite(D, file.path(OUT_DIR, "fi04_breakeven.csv"))
for (cc in CELLS) for (th in THETA) {
  s <- D[which(D[["cell"]] == cc & D[["theta"]] == th)][order(cost)]
  say("  %s theta=%.2f  %s", cc, th,
      paste(sprintf("c=%s: K=%d%% net %s", format(s$cost, big.mark = ","), s$K_chosen_2021, bn(s$net_2022)), collapse = " | "))
}
say("\nnet = theta x LGD x exposure reached - cost x follow-ups, on 2022, with K fixed from 2021.")
say("\nworkload at %d follow-ups per officer-day (base cost %s THB per follow-up):", CASES_PER_DAY, COST_BASE)
for (k in c(5, 10, 20, 30, 40))
  say("   K = %2d%%  %s follow-ups = %s officer-days = %.0f days per officer (of ~%.0f), %s THB",
      k, format(floor(k / 100 * nrow(d)), big.mark = ","),
      format(round(floor(k / 100 * nrow(d)) / CASES_PER_DAY), big.mark = ","),
      floor(k / 100 * nrow(d)) / CASES_PER_DAY / OFFICERS, OFFICERS,
      format(COST_BASE * floor(k / 100 * nrow(d)), big.mark = ",", scientific = FALSE))
say("\nfeasible follow-up share of the portfolio: %.0f officers x %d field days x %d per day / visits per borrower",
    OFFICERS, FIELD_DAYS, CASES_PER_DAY)
feas <- data.table(visits = VISITS, borrowers = OFFICERS * FIELD_DAYS * CASES_PER_DAY / VISITS)
feas[, K_feasible := 100 * borrowers / nrow(d)]
for (i in seq_len(nrow(feas)))
  say("   %d visit(s) per borrower: %s borrowers = K %.1f%%", feas$visits[i],
      format(round(feas$borrowers[i]), big.mark = ","), feas$K_feasible[i])
fwrite(feas, file.path(OUT_DIR, "fi04_feasible_K.csv"))
for (cc in CELLS) {
  rule <- chosen[[cc]]
  cv22 <- curve(d, rank_of(d, rule, cc, tie), d$Out)
  for (i in seq_len(nrow(feas))) {
    k <- max(1L, min(40L, floor(feas$K_feasible[i])))
    r <- cv22[K == k]
    say("   %s %s at feasible K = %d%% (%d visit): recall %.3f, exposure capture %.3f, EAR left %s",
        cc, rule, k, feas$visits[i], r$recall, r$cap, bn(r$EAR))
  }
  be <- COST_BASE / (LGD0 * cv22$marg)            # theta needed for the marginal follow-up to pay
  say("   %s %s: save-rate theta at which the marginal follow-up pays for itself (2022): %s", cc, rule,
      paste(sprintf("K%d %.1f%%", K_SHOW, 100 * be[K_SHOW]), collapse = "  "))
}
say("done. Paste %s into the chat.", LOG)

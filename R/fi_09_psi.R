###############################################################################
#  fi_09_psi.R  --  Study 3 (Financial Innovation), step 9
#
#  RQ4: can the bank see a season going wrong before the outcomes arrive?
#  The standard monitoring kit of a scorecard (Siddiqi, 2006), on the main
#  ex-ante card of fi_06, computed WITHOUT outcomes:
#
#    PSI  population stability index of the score, year t against year t-1,
#         on ten bins fixed at the deciles of year t-1:
#              PSI = sum (a_i - e_i) ln(a_i / e_i)
#    CSI  the same for every characteristic on the card (its own bins for
#         categorical ones, deciles of year t-1 for numeric ones), plus the
#         mean points it contributes, so a reader sees WHICH characteristic
#         moved the score and in which direction.
#    Reason drift: the mix of first reason codes among the 19% lowest scores.
#  Conventional reading (fixed in advance, not tuned): PSI < 0.10 stable,
#  0.10-0.25 watch, > 0.25 act.
#
#  Outcomes are shown alongside ONLY as a check of whether the alarm and the
#  realised default rate moved together; nothing is selected on them.
#  Caveat stated in the log: the 2020 and 2021 scores come from the card built
#  on those years, so 2020->2021 is in-sample for the card's points (it is not
#  for the population the PSI measures).
#
#  Needs fi_06 rerun with the version that writes fi06_all_scores_main.rds.
#  Output: fi_out/fi09_psi.csv, fi09_csi.csv, fi09_reasons.csv, fi09_log.txt
###############################################################################

suppressPackageStartupMessages(library(data.table))

RAW_FILE <- "1_data_before_out.txt"
OUT_DIR  <- "fi_out"
K_REASON <- 19
YEAR_LAB <- c(`1` = 2020L, `2` = 2021L, `3` = 2022L)

LOG <- file.path(OUT_DIR, "fi09_log.txt"); cat("", file = LOG)
say <- function(...) { s <- sprintf(...); cat(s, "\n", sep = ""); cat(s, "\n", sep = "", file = LOG, append = TRUE) }
say("fi_09_psi  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

f_sc <- file.path(OUT_DIR, "fi06_all_scores_main.rds"); f_sp <- file.path(OUT_DIR, "fi06_spec_main.rds")
if (!file.exists(f_sc) || !file.exists(f_sp))
  stop("rerun fi_06_scorecard.bat first: this step needs fi06_all_scores_main.rds and fi06_spec_main.rds")
sc <- as.data.table(readRDS(f_sc)); sp <- readRDS(f_sp)
vars <- sp$vars; tabs <- as.data.table(sp$tabs)

psi <- function(base, now, cuts = NULL, cat = FALSE) {
  if (cat) { lv <- union(unique(base), unique(now)); e <- table(factor(base, lv)); a <- table(factor(now, lv)) }
  else {
    if (is.null(cuts)) cuts <- unique(quantile(base, seq(0.1, 0.9, 0.1), names = FALSE, na.rm = TRUE))
    e <- table(factor(findInterval(base, cuts), 0:length(cuts)))
    a <- table(factor(findInterval(now, cuts), 0:length(cuts)))
  }
  e <- (as.numeric(e) + 0.5) / sum(as.numeric(e) + 0.5); a <- (as.numeric(a) + 0.5) / sum(as.numeric(a) + 0.5)
  sum((a - e) * log(a / e))
}
verdict <- function(x) ifelse(x < 0.10, "stable", ifelse(x <= 0.25, "watch", "act"))

## ------------------------------------------------------------ score PSI
say("\n== score PSI (ten bins at the deciles of the earlier year) ==")
P <- rbindlist(lapply(list(c(1, 2), c(2, 3), c(1, 3)), function(p) {
  b <- sc[Year == p[1], score]; n <- sc[Year == p[2], score]
  data.table(from = YEAR_LAB[[as.character(p[1])]], to = YEAR_LAB[[as.character(p[2])]],
             PSI = psi(b, n), mean_from = mean(b), mean_to = mean(n),
             dr_from = sc[Year == p[1], mean(Y)], dr_to = sc[Year == p[2], mean(Y)])
}))
P[, reading := verdict(PSI)]
for (i in seq_len(nrow(P)))
  say("   %d -> %d  PSI %.4f (%s)  mean score %.1f -> %.1f  | check only: default rate %.2f%% -> %.2f%%",
      P$from[i], P$to[i], P$PSI[i], P$reading[i], P$mean_from[i], P$mean_to[i], 100 * P$dr_from[i], 100 * P$dr_to[i])
fwrite(P, file.path(OUT_DIR, "fi09_psi.csv"))
say("   (2020 and 2021 are the card's own development years; 2022 is out of sample)")

## ---------------------------------------------- characteristic CSI
raw <- fread(RAW_FILE, sep = "|", showProgress = TRUE,
             select = c("Year", "ID", intersect(c(vars, "eInc", "eCost", "Area", "AreaR"), names(fread(RAW_FILE, sep = "|", nrows = 0)))))
if ("Profit_buffer" %in% vars) raw[, Profit_buffer := fifelse(eInc > 0, (eInc - eCost) / eInc, NA_real_)]
if ("Land_util" %in% vars) raw[, Land_util := fifelse(AreaR > 0, Area / AreaR, NA_real_)]
pts_of <- function(v, x) {
  S <- sp$spec[[v]]
  b <- if (S$type == "num") { bb <- paste0("b", findInterval(x, S$cuts, left.open = TRUE) + 1L); bb[is.na(x)] <- "missing"; bb }
       else { bb <- ifelse(as.character(x) %in% S$keep, as.character(x), "other"); bb[is.na(x)] <- "missing"; bb }
  setNames(tabs[characteristic == v, points], tabs[characteristic == v, bin])[b]
}
say("\n== characteristic stability (CSI) and mean points contributed ==")
C <- list()
for (v in vars) {
  if (v == "Area_default_hist") {                       # area history lives only in the score file
    say("   %-20s (sub-district default history: rebuilt each year from earlier outcomes; shown via reasons)", v); next
  }
  S <- sp$spec[[v]]
  for (p in list(c(1, 2), c(2, 3))) {
    b <- raw[Year == p[1]][[v]]; n <- raw[Year == p[2]][[v]]
    cs <- if (S$type == "cat") psi(as.character(b), as.character(n), cat = TRUE) else psi(b, n)
    C[[length(C) + 1]] <- data.table(characteristic = v, from = YEAR_LAB[[as.character(p[1])]],
                                     to = YEAR_LAB[[as.character(p[2])]], CSI = cs,
                                     points_from = mean(pts_of(v, b), na.rm = TRUE),
                                     points_to = mean(pts_of(v, n), na.rm = TRUE))
  }
}
C <- rbindlist(C); C[, `:=`(reading = verdict(CSI), d_points = points_to - points_from)]
fwrite(C, file.path(OUT_DIR, "fi09_csi.csv"))
say("  NOTE: in 2020 no borrower has a previous year in the panel, so Previous_Default and")
say("  Has_Credit_History jump by construction from 2020 to 2021. 2021 -> 2022 is the real test.")
for (pr in c("2020->2021", "2021->2022")) {
  s_ <- C[paste0(from, "->", to) == pr][order(-CSI)]
  say("  %s", pr)
  for (i in seq_len(nrow(s_)))
    say("     %-20s CSI %.4f (%s)  mean points %.1f -> %.1f (%+.1f)", s_$characteristic[i], s_$CSI[i],
        s_$reading[i], s_$points_from[i], s_$points_to[i], s_$d_points[i])
}

## ------------------------------------------------------ reason drift
say("\n== first reason code among the %d%% lowest scores, by year ==", K_REASON)
R <- sc[, { f <- rank(score, ties.method = "first") <= floor(K_REASON / 100 * .N); .SD[f, .N, by = r1] }, by = Year]
R[, share := N / sum(N), by = Year]
Rw <- dcast(R, r1 ~ Year, value.var = "share", fill = 0)
setnames(Rw, as.character(1:3), paste0("y", YEAR_LAB[as.character(1:3)]), skip_absent = TRUE)
fwrite(Rw, file.path(OUT_DIR, "fi09_reasons.csv"))
for (i in seq_len(nrow(Rw)))
  say("   %-20s %s", Rw$r1[i], paste(sprintf("%s %5.1f%%", names(Rw)[-1], 100 * unlist(Rw[i, -1])), collapse = "  "))
say("\ndone. Paste %s into the chat.", LOG)

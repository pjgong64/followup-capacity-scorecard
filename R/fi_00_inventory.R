###############################################################################
#  fi_00_inventory.R  --  Study 3 (Financial Innovation), step 0
#
#  READ-ONLY. Fits nothing, tunes nothing, selects nothing.
#
#  What it checks, and why each check exists:
#    A. The score vectors FI will use exist and reproduce their AUCs:
#         main run  (fit 2020+2021 -> score 2022): XGB | F+SP, LR | F+SP
#         rolling   (fit 2020      -> score 2021): XGB | F+SP, LR | F+SP
#       The rolling pair is the SELECTION set: every FI choice is made there.
#    B. The raw money columns (Outb, Out, CLb, CL, Int) join to the score rows
#       one-to-one on (ID, Year), and the outcome in the raw file agrees with
#       the outcome in keys_*.rds row by row. If either fails, nothing
#       downstream can be trusted, so the script stops.
#    C. Descriptive profile of exposure by year and outcome: the quantities
#       EAR (exposure at risk) will be built from.
#    D. Interest-rate tiers by year and outcome (Int is the strongest single
#       predictor in the model; its timing has to be confirmed before FI uses
#       it as a reason code or as a per-borrower forgone-interest rate).
#
#  The 2022 rows are READ here only to describe them and to check alignment.
#  No number computed on 2022 is used to choose anything.
#
#  Output: fi_out/fi00_log.txt and fi_out/fi00_*.csv
#  Run:    fi_00_inventory.bat  (or Rscript fi_00_inventory.R from this folder)
###############################################################################

suppressPackageStartupMessages(library(data.table))

RAW_FILE  <- "3_fin3y20_22.txt"            # raw, un-normalised, THB
MAIN_PRED <- file.path("factorial7_out", "pred")
ROLL_PRED <- file.path("factorial7_out", "rolling_2020_to_2021", "pred")
OUT_DIR   <- "fi_out"
CELLS     <- c("XGB__F_SP", "LR__F_SP")
YEAR_LAB  <- c(`1` = 2020L, `2` = 2021L, `3` = 2022L)

# AUCs the score vectors on disk are expected to reproduce (tolerance 1e-6:
# these are stored vectors, not refits, so they must match exactly).
EXPECT <- list(
  main    = c(XGB__F_SP = 0.90765704862945,  LR__F_SP = 0.894327453556953),
  rolling = c(XGB__F_SP = 0.897957612634277, LR__F_SP = 0.892829078726812)
)

dir.create(OUT_DIR, showWarnings = FALSE)
LOG <- file.path(OUT_DIR, "fi00_log.txt")
cat("", file = LOG)
say <- function(...) {
  s <- sprintf(...)
  cat(s, "\n", sep = "")
  cat(s, "\n", sep = "", file = LOG, append = TRUE)
}
fmt <- function(x, d = 0) format(round(x, d), big.mark = ",", nsmall = d,
                                 scientific = FALSE)

say("fi_00_inventory  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
say("working directory: %s", normalizePath("."))

fast_auc <- function(score, y) {
  n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  r  <- rank(score, ties.method = "average")
  (sum(as.numeric(r[y == 1])) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

## ---------------------------------------------------------------- A. scores
load_run <- function(dir, tag) {
  kf <- file.path(dir, "keys_test.rds")
  if (!file.exists(kf)) stop("missing ", kf)
  k <- as.data.table(readRDS(kf))
  setnames(k, tolower(names(k)))
  for (cell in CELLS) {
    pf <- file.path(dir, sprintf("pred_%s.rds", cell))
    if (!file.exists(pf)) stop("missing ", pf)
    v <- as.numeric(readRDS(pf))
    if (length(v) != nrow(k))
      stop(sprintf("%s: %d scores but %d keys", pf, length(v), nrow(k)))
    k[[cell]] <- v
  }
  k[, run := tag]
  k
}

say("\n== A. score vectors ==")
runs <- list(main = load_run(MAIN_PRED, "main"),
             rolling = load_run(ROLL_PRED, "rolling"))
aucA <- rbindlist(lapply(names(runs), function(r) {
  k <- runs[[r]]
  rbindlist(lapply(CELLS, function(cell) {
    a <- fast_auc(k[[cell]], k$y)
    data.table(run = r, cell = cell, n = nrow(k), defaults = sum(k$y),
               default_rate = mean(k$y), auc = a,
               expected = EXPECT[[r]][[cell]],
               diff = a - EXPECT[[r]][[cell]])
  }))
}))
for (i in seq_len(nrow(aucA)))
  say("%-8s %-10s n=%s  defaults=%s (%.3f%%)  AUC=%.6f  expected=%.6f  diff=%+.1e  %s",
      aucA$run[i], aucA$cell[i], fmt(aucA$n[i]), fmt(aucA$defaults[i]),
      100 * aucA$default_rate[i], aucA$auc[i], aucA$expected[i], aucA$diff[i],
      if (abs(aucA$diff[i]) < 1e-6) "OK" else "MISMATCH")
fwrite(aucA, file.path(OUT_DIR, "fi00_auc_check.csv"))
if (any(abs(aucA$diff) >= 1e-6))
  warning("an AUC does not reproduce -- read fi00_auc_check.csv before going on")

## ---------------------------------------------------------- B. money columns
say("\n== B. joining the raw money columns ==")
if (!file.exists(RAW_FILE)) stop("missing ", RAW_FILE)
raw <- fread(RAW_FILE, sep = "|", showProgress = TRUE,
             select = c("Year", "ID", "ADM3", "Y", "Outb", "Out",
                        "CLb", "CL", "Int"))
say("raw file: %s rows", fmt(nrow(raw)))
dup <- raw[, .N, by = .(Year, ID)][N > 1]
if (nrow(dup)) stop(nrow(dup), " duplicated (ID, Year) pairs in ", RAW_FILE)
say("(ID, Year) is unique in the raw file: OK")

# keys_test of the main run is 2022 (Year code 3); of the rolling run, 2021 (2)
YEAR_OF <- c(main = 3L, rolling = 2L)
joined <- list()
for (r in names(runs)) {
  k <- copy(runs[[r]])
  k[, Year := YEAR_OF[[r]]]
  k[, ord := .I]
  j <- merge(k, raw, by.x = c("id", "Year"), by.y = c("ID", "Year"),
             all.x = TRUE, sort = FALSE)
  setorder(j, ord)
  n_miss <- j[is.na(Out), .N]
  n_bad  <- j[!is.na(Y) & Y != y, .N]
  n_adm  <- j[!is.na(ADM3) & ADM3 != adm3, .N]
  say("%-8s (%d) rows=%s  unmatched=%s  outcome mismatch=%s  ADM3 mismatch=%s",
      r, YEAR_LAB[[as.character(YEAR_OF[[r]])]], fmt(nrow(j)), fmt(n_miss),
      fmt(n_bad), fmt(n_adm))
  if (n_miss > 0 || n_bad > 0)
    stop("alignment failed for the ", r, " run -- do not continue")
  joined[[r]] <- j
}
say("alignment: OK for both runs")

## ------------------------------------------------- C. exposure profile (THB)
say("\n== C. exposure by year and outcome (THB) ==")
prof <- raw[, .(n = .N,
                out_total   = sum(as.numeric(Out)),
                out_mean    = mean(Out),
                out_median  = as.numeric(median(Out)),
                out_p90     = as.numeric(quantile(Out, 0.90)),
                out_p99     = as.numeric(quantile(Out, 0.99)),
                out_zero    = mean(Out == 0),
                outb_mean   = mean(Outb),
                out_ge_outb = mean(Out >= Outb),
                cl_mean     = mean(CL),
                out_gt_cl   = mean(Out > CL),
                int_mean    = mean(Int)),
            by = .(Year, Y)][order(Year, Y)]
prof[, year := YEAR_LAB[as.character(Year)]]
for (i in seq_len(nrow(prof)))
  say("%d Y=%d n=%s  total Out=%s bn  mean=%s  median=%s  p99=%s  Out==0 %.2f%%  Out>=Outb %.1f%%  Out>CL %.2f%%",
      prof$year[i], prof$Y[i], fmt(prof$n[i]), fmt(prof$out_total[i] / 1e9, 2),
      fmt(prof$out_mean[i]), fmt(prof$out_median[i]), fmt(prof$out_p99[i]),
      100 * prof$out_zero[i], 100 * prof$out_ge_outb[i], 100 * prof$out_gt_cl[i])
fwrite(prof, file.path(OUT_DIR, "fi00_exposure_profile.csv"))

# Share of portfolio exposure held by defaulters: the ceiling on what any
# alert list can capture, and the denominator of exposure capture.
cap <- raw[, .(exposure_total = sum(as.numeric(Out)),
               exposure_def   = sum(as.numeric(Out[Y == 1])),
               n_def = sum(Y == 1), n = .N), by = Year][order(Year)]
cap[, `:=`(year = YEAR_LAB[as.character(Year)],
           share_exposure_def = exposure_def / exposure_total,
           share_count_def    = n_def / n)]
for (i in seq_len(nrow(cap)))
  say("%d  defaulters hold %s bn of %s bn (%.2f%% of exposure, %.2f%% of borrowers)",
      cap$year[i], fmt(cap$exposure_def[i] / 1e9, 2),
      fmt(cap$exposure_total[i] / 1e9, 2),
      100 * cap$share_exposure_def[i], 100 * cap$share_count_def[i])
fwrite(cap, file.path(OUT_DIR, "fi00_exposure_capacity.csv"))

# Defaulters whose year-end principal is zero would carry EAD = 0 under EAD=Out.
zero_def <- raw[Y == 1 & Out == 0, .(n = .N, outb_mean = mean(Outb),
                                     outb_zero = mean(Outb == 0)), by = Year]
say("\ndefaulters with Out == 0 (EAD would be zero):")
for (i in seq_len(nrow(zero_def)))
  say("  %d  n=%s  their mean Outb=%s  Outb also zero=%.1f%%",
      YEAR_LAB[as.character(zero_def$Year[i])], fmt(zero_def$n[i]),
      fmt(zero_def$outb_mean[i]), 100 * zero_def$outb_zero[i])
fwrite(zero_def, file.path(OUT_DIR, "fi00_zero_out_defaulters.csv"))

## ------------------------------------------------------- D. interest tiers
say("\n== D. contractual interest rate tiers ==")
tiers <- raw[, .(n = .N, defaults = sum(Y)), by = .(Year, Int)][order(Year, Int)]
tiers[, `:=`(year = YEAR_LAB[as.character(Year)], default_rate = defaults / n)]
tiers[, share_of_year := n / sum(n), by = Year]
for (i in seq_len(nrow(tiers)))
  say("%d  Int=%.4f  n=%s (%.1f%% of year)  default rate %.2f%%",
      tiers$year[i], tiers$Int[i], fmt(tiers$n[i]),
      100 * tiers$share_of_year[i], 100 * tiers$default_rate[i])
fwrite(tiers, file.path(OUT_DIR, "fi00_interest_tiers.csv"))

# Does a borrower's tier move between consecutive years, and does a move
# up coincide with the outcome? Needed to judge whether Int is set before the
# season (pricing) or after it (penalty).
w <- dcast(raw[, .(ID, Year, Int, Y)], ID ~ Year, value.var = c("Int", "Y"))
mv <- rbindlist(list(
  w[!is.na(Int_1) & !is.na(Int_2),
    .(pair = "2020->2021", n = .N,
      same = mean(Int_1 == Int_2),
      up_to_095 = mean(Int_2 == 0.095 & Int_1 != 0.095),
      dr_if_up = mean(Y_2[Int_2 == 0.095 & Int_1 != 0.095]),
      dr_prev_default_and_095 = mean(Int_2[Y_1 == 1] == 0.095))],
  w[!is.na(Int_2) & !is.na(Int_3),
    .(pair = "2021->2022", n = .N,
      same = mean(Int_2 == Int_3),
      up_to_095 = mean(Int_3 == 0.095 & Int_2 != 0.095),
      dr_if_up = mean(Y_3[Int_3 == 0.095 & Int_2 != 0.095]),
      dr_prev_default_and_095 = mean(Int_3[Y_2 == 1] == 0.095))]))
say("\ntier movement between years:")
for (i in seq_len(nrow(mv)))
  say("  %s  n=%s  same tier %.1f%%  moved up to 9.5%% %.2f%%  (their default rate %.1f%%)  share of last year's defaulters now at 9.5%% %.1f%%",
      mv$pair[i], fmt(mv$n[i]), 100 * mv$same[i], 100 * mv$up_to_095[i],
      100 * mv$dr_if_up[i], 100 * mv$dr_prev_default_and_095[i])
fwrite(mv, file.path(OUT_DIR, "fi00_interest_movement.csv"))

## ------------------------------------- E. proxy for the March-due group
# The bank's account (Gong, 17 Oct): most contracts fall due in December, but
# about 20% fall due in March because the farmer stores the rice to sell then;
# those are mostly harvested in Nov-Dec (MH 11/12) and grow jasmine rice
# (IDRV = 1). There is no due-date column, so the question here is only
# whether MH in {11,12} & IDRV == 1 is a usable proxy: if it marks far more
# than ~20% of borrowers it is too broad to separate the two groups.
say("\n== E. proxy for the March-due (stored-rice) group: MH in {11,12} & IDRV == 1 ==")
px <- fread(RAW_FILE, sep = "|", showProgress = FALSE,
            select = c("Year", "Y", "MH", "IDRV", "Previous_Default"))
px[, proxy := MH %in% c(11L, 12L) & IDRV == 1L]
pe <- px[, .(n = .N, default_rate = mean(Y),
             prev_default_share = mean(Previous_Default == 1)),
         by = .(Year, proxy)][order(Year, -proxy)]
pe[, share_of_year := n / sum(n), by = Year]
pe[, year := YEAR_LAB[as.character(Year)]]
for (i in seq_len(nrow(pe)))
  say("%d  proxy=%-5s  n=%s (%.1f%% of year)  default rate %.2f%%  previous default %.1f%%",
      pe$year[i], pe$proxy[i], fmt(pe$n[i]), 100 * pe$share_of_year[i],
      100 * pe$default_rate[i], 100 * pe$prev_default_share[i])
# the two ingredients separately, so a broad proxy can be narrowed
say("share by MH (all years): %s",
    paste(px[, .N, by = MH][order(MH)][, sprintf("%d:%.1f%%", MH, 100 * N / sum(N))],
          collapse = "  "))
say("share by IDRV (all years): %s",
    paste(px[, .N, by = IDRV][order(IDRV)][, sprintf("%d:%.1f%%", IDRV, 100 * N / sum(N))],
          collapse = "  "))
fwrite(pe, file.path(OUT_DIR, "fi00_march_due_proxy.csv"))

say("\ndone. Paste the whole of %s back into the chat.", LOG)

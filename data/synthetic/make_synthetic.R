###############################################################################
#  make_synthetic.R -- a fully artificial panel with the schema the fi_ scripts
#  read, so that fi_02 to fi_10 run end to end without the restricted records.
#
#  Writes into ../../work/ (relative to this file's folder):
#    1_data_before_out.txt                         borrower-year file, "|"-separated
#    factorial7_out/exante/pred/                   main run (fit 2020-21, score 2022)
#    factorial7_out/rolling_2020_to_2021/exante/pred/   selection run (fit 2020, score 2021)
#  Each pred folder holds keys_test.rds, keys_dev.rds (with in_val) and the
#  score vectors pred_<cell>.rds / dev_<cell>.rds for cells XGB__F_SP, LR__F_SP,
#  in the format written by fi_01_run_exante*.R.
#
#  No value comes from a real borrower. The default model below is invented and
#  only roughly mimics the direction of the real effects, so every number the
#  pipeline prints on these data is meaningless; the point is that it runs.
###############################################################################
suppressPackageStartupMessages(library(data.table)); set.seed(20261102)
N <- as.integer(Sys.getenv("SYNTH_N", "20000"))
OUT <- file.path("..", "..", "work"); dir.create(OUT, showWarnings = FALSE)
raw <- rbindlist(lapply(1:3, function(yr) {
  d <- data.table(Year = yr, ID = seq_len(N), ADM3 = sample(100101:100160, N, TRUE))
  d[, Int := sample(c(.065, .0725, .08, .0875, .095), N, TRUE, prob = c(.65, .07, .07, .12, .09))]
  d[, Previous_Default := if (yr == 1) -1L else sample(0:1, N, TRUE, prob = c(.93, .07))]
  d[, Has_Credit_History := as.integer(yr > 1)]
  d[, Outb := round(rlnorm(N, 12, 1))]; d[, CLb := round(Outb * runif(N, 1, 1.5))]; d[, Depb := round(rlnorm(N, 9, 1.5))]
  d[, `:=`(nLC = sample(1:5, N, TRUE), nIS = sample(1:3, N, TRUE), nP = sample(1:6, N, TRUE), Area = runif(N, 2, 60))]
  d[, AreaR := Area * runif(N, 1, 1.5)]
  d[, `:=`(nRV = sample(1:3, N, TRUE), IDRV = sample(1:5, N, TRUE), MP = sample(4:8, N, TRUE), MH = sample(10:12, N, TRUE), CD = sample(100:200, N, TRUE))]
  d[, `:=`(eCostrai = runif(N, 3000, 5600), eYierai = runif(N, 0.28, 0.40), ePrcton = runif(N, 9000, 12000), IDDT = sample(c(68, 44, 27, 23), N, TRUE))]
  d[, `:=`(eCost = eCostrai * Area, eYie = eYierai * Area)]; d[, eInc := eYie * ePrcton]
  d[, lp := -3.6 + 60 * (Int - .065) + 2.5 * (Previous_Default == 1) + 0.4 * (log(Outb) - 12) - .5 * (log(Depb) - 9) + c(0, .4, .9)[yr]]
  d[, Y := rbinom(N, 1, plogis(lp))]
  d[, Out := pmax(0, round(Outb * runif(N, 0.6, 1.1)))]
  d[, s_true := lp]; d
}))
fwrite(raw[, !c("lp", "s_true")], file.path(OUT, "1_data_before_out.txt"), sep = "|")
mk <- function(dir, dev_years, test_year) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  kt <- raw[Year == test_year][sample(.N)]; kd <- raw[Year %in% dev_years][sample(.N)]
  saveRDS(data.table(y = kt$Y, adm3 = kt$ADM3, id = kt$ID), file.path(dir, "keys_test.rds"))
  saveRDS(data.table(y = kd$Y, adm3 = kd$ADM3, id = kd$ID, in_val = runif(nrow(kd)) < .15), file.path(dir, "keys_dev.rds"))
  for (cc in c("XGB__F_SP", "LR__F_SP")) {
    noise <- if (cc == "XGB__F_SP") 0.6 else 0.8          # weighted-model scores: shifted up, noisier for LR
    saveRDS(plogis(kt$s_true + 2.8 + rnorm(nrow(kt), 0, noise)), file.path(dir, sprintf("pred_%s.rds", cc)))
    saveRDS(plogis(kd$s_true + 2.8 + rnorm(nrow(kd), 0, noise)), file.path(dir, sprintf("dev_%s.rds", cc)))
  }
}
mk(file.path(OUT, "factorial7_out", "exante", "pred"), 1:2, 3)
mk(file.path(OUT, "factorial7_out", "rolling_2020_to_2021", "exante", "pred"), 1, 2)
dir.create(file.path(OUT, "fi_out"), showWarnings = FALSE)
cat(sprintf("synthetic panel: %s borrowers x 3 years -> %s\n", format(N, big.mark = ","), normalizePath(OUT)))

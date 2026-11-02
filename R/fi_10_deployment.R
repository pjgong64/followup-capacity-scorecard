###############################################################################
#  fi_10_deployment.R  --  Study 3 (Financial Innovation), step 10
#
#  Section 5 (Deploying the Scorecard) of the FI manuscript, reproducible:
#    A. gains table by 20-point score band, 2021 and 2022 (Table 9)
#    B. points-to-odds alignment per year (realised PDO and 50:1 anchor)
#    C. label-free realignment Delta = PDO/ln2 * ln(odds_dev/odds_new)
#    D. strategy table: one score cutoff per loan-size band, set on 2021 from
#       the mixed policy (s = 0.5) at K = 13/19/38, applied unchanged to 2022
#       (Tables 10-11)
#    E. reason codes among borrowers flagged by the K = 19 table (Section 5.3)
#  Nothing here is selected on 2022 outcomes; 2022 is evaluated once.
#  Needs: fi_out/fi06_all_scores_main.rds, fi06_spec_main.rds,
#         fi06_reasons_main.rds, and 1_data_before_out.txt
#  Output: fi_out/fi10_*.csv and fi_out/fi10_log.txt
###############################################################################
suppressPackageStartupMessages(library(data.table))
RAW_FILE <- "1_data_before_out.txt"; OUT_DIR <- "fi_out"
EM_PRIOR_2022 <- 0.1110      # label-free prior of the scorecard on 2022, from fi06_log
DEV_PRIOR     <- 0.0570      # development prior (2020-2021), from fi06_log
S_MIX <- 0.5; SMALL_Q <- 0.40; K_SET <- c(13, 19, 38); SEED <- 20261026
EDGES <- c(-Inf, 50e3, 160e3, 300e3, 500e3, 1e6, Inf)
LAB <- c("<= 50,000", "50,001-160,000", "160,001-300,000", "300,001-500,000",
         "500,001-1,000,000", "> 1,000,000")

LOG <- file.path(OUT_DIR, "fi10_log.txt"); cat("", file = LOG)
say <- function(...) { s <- sprintf(...); cat(s, "\n", sep = ""); cat(s, "\n", sep = "", file = LOG, append = TRUE) }
say("fi_10_deployment  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

sp  <- readRDS(file.path(OUT_DIR, "fi06_spec_main.rds")); OFF <- sp$offset; FAC <- sp$factor
sc  <- as.data.table(readRDS(file.path(OUT_DIR, "fi06_all_scores_main.rds"))); setnames(sc, "Y", "Ys")
raw <- fread(RAW_FILE, sep = "|", select = c("Year", "ID", "Outb", "Out", "Int", "Previous_Default", "Y"))
x <- merge(raw, sc, by = c("Year", "ID"))
stopifnot(nrow(x) == nrow(sc), all(x$Y == x$Ys)); x[, Ys := NULL]
x[, p := 1 / (1 + exp((score - OFF) / FAC))]
set.seed(SEED); x[, tie := runif(.N)]
say("scaling: offset %.4f factor %.4f (PDO %.1f)", OFF, FAC, FAC * log(2))

## A. gains table
x[, band := cut(score, c(-Inf, seq(500, 660, 20), Inf), right = FALSE)]
G <- x[Year >= 2, .(n = .N, bad = sum(Y), exp_card = mean(p)), by = .(Year, band)][order(Year, band)]
G[, `:=`(share = n / sum(n), bad_rate = bad / n, cum_bad = cumsum(bad) / sum(bad)), by = Year]
fwrite(G, file.path(OUT_DIR, "fi10_gains.csv"))
say("\n== A. gains table (bad rate 2021 | 2022) ==")
for (b in levels(x$band)) say("   %-11s %6.2f%% | %6.2f%%  cum 2022 %5.1f%%", b,
  100 * G[Year == 2 & band == b, bad_rate], 100 * G[Year == 3 & band == b, bad_rate], 100 * G[Year == 3 & band == b, cum_bad])

## B. alignment
say("\n== B. realised points-to-odds alignment ==")
al <- function(d) { b <- coef(glm(Y ~ score, binomial, data = d)); c(PDO = log(2) / -b[[2]], s50 = (log(1 / 50) - b[[1]]) / b[[2]]) }
for (lab in c("dev 2020-21", "2021", "2022")) {
  d <- switch(lab, "dev 2020-21" = x[Year <= 2], "2021" = x[Year == 2], "2022" = x[Year == 3])
  a <- al(d); say("   %-11s PDO %.1f  score at 50:1 %.1f", lab, a["PDO"], a["s50"]) }

## C. realignment
D <- FAC * log(((1 - DEV_PRIOR) / DEV_PRIOR) / ((1 - EM_PRIOR_2022) / EM_PRIOR_2022))
x[Year == 3, p_re := 1 / (1 + exp((score - D - OFF) / FAC))]
say("\n== C. label-free realignment: Delta = %.1f points ==", D)
say("   2022 mean PD: card %.2f%%  realigned %.2f%%  realised %.2f%% (check only)",
    100 * x[Year == 3, mean(p)], 100 * x[Year == 3, mean(p_re)], 100 * x[Year == 3, mean(Y)])
fwrite(x[Year == 3, .(exp_card = mean(p), exp_realigned = mean(p_re), realised = mean(Y)), by = band][order(band)],
       file.path(OUT_DIR, "fi10_realign.csv"))

## D. strategy table
d21 <- x[Year == 2]; d22 <- x[Year == 3]
for (d in list(d21, d22)) { d[, small := Outb <= quantile(Outb, SMALL_Q)]; d[, sb := cut(Outb, EDGES, labels = LAB)] }
mixed <- function(d, s, k) { N <- nrow(d); nK <- floor(k / 100 * N); f <- logical(N)
  o_m <- order(-(d$p * d$Outb), d$tie); o_p <- order(-d$p, d$tie)
  n_c <- round(s * nK); n_m <- nK - n_c; if (n_m > 0) f[o_m[seq_len(n_m)]] <- TRUE
  cand <- o_p[(!f & d$small)[o_p]]; f[cand[seq_len(n_c)]] <- TRUE; f }
bank <- function(d, k) { f <- logical(nrow(d))
  o <- order(-d$Int, -(d$Previous_Default == 1), -d$Outb, d$tie); f[o[seq_len(floor(k / 100 * nrow(d)))]] <- TRUE; f }
summ <- function(d, f, pol, k, yr) { y <- d$Y == 1
  data.table(K = k, year = yr, policy = pol, share = mean(f), recall = sum(y & f) / sum(y),
             small_recall = sum(y & f & d$small) / sum(y & d$small), EAR_bn = (sum(d$Out[y]) - sum(d$Out[y & f])) / 1e9) }
TAB <- data.table(band = LAB); RES <- list()
for (k in K_SET) {
  f21 <- mixed(d21, S_MIX, k)
  cc <- sapply(LAB, function(b) { s <- d21[sb == b, score]; t <- sum(f21[d21$sb == b]); if (t == 0) return(NA)
    u <- sort(unique(s)); cnt <- findInterval(u, sort(s)); u[which.min(abs(cnt - t))] })
  TAB[[paste0("K", k)]] <- cc
  ap <- function(d) { c0 <- cc[match(as.character(d$sb), LAB)]; !is.na(c0) & d$score <= c0 }
  RES[[length(RES) + 1]] <- rbind(summ(d21, bank(d21, k), "bank grade", k, 2021), summ(d21, ap(d21), "strategy table", k, 2021),
                                  summ(d22, bank(d22, k), "bank grade", k, 2022), summ(d22, ap(d22), "strategy table", k, 2022))
}
RES <- rbindlist(RES); fwrite(TAB, file.path(OUT_DIR, "fi10_strategy_table.csv")); fwrite(RES, file.path(OUT_DIR, "fi10_strategy_eval.csv"))
say("\n== D. strategy table (visit if score <= cutoff), set on 2021 ==")
for (i in seq_len(nrow(TAB))) say("   %-18s %s", TAB$band[i], paste(sprintf("K%d %3d", K_SET, unlist(TAB[i, -1])), collapse = "  "))
say("   evaluation:")
for (i in seq_len(nrow(RES))) say("   K %2d %d %-15s share %.3f recall %.3f small %.3f EAR %.2f bn",
  RES$K[i], RES$year[i], RES$policy[i], RES$share[i], RES$recall[i], RES$small_recall[i], RES$EAR_bn[i])

## E. reason codes among the K = 19 table's list in 2022
r <- as.data.table(readRDS(file.path(OUT_DIR, "fi06_reasons_main.rds")))
c19 <- TAB$K19[match(as.character(d22$sb), LAB)]; d22[, flag := !is.na(c19) & score <= c19]
z <- merge(d22[flag == TRUE, .(ID, sb)], r, by.x = "ID", by.y = "id")
say("\n== E. leading reason among %s flagged borrowers (2022, K = 19 table) ==", format(nrow(z), big.mark = ","))
R1 <- z[, .N, by = r1][, share := N / sum(N)][order(-N)]
for (i in seq_len(nrow(R1))) say("   %-20s %5.1f%%", R1$r1[i], 100 * R1$share[i])
say("   median shortfall on reasons 1-3: %.0f / %.0f / %.0f points", median(z$gap1), median(z$gap2), median(z$gap3))
fwrite(dcast(z[, .N, by = .(sb, r1)][, share := N / sum(N), by = sb], sb ~ r1, value.var = "share", fill = 0),
       file.path(OUT_DIR, "fi10_reasons_by_size.csv"))
say("\ndone. Paste %s into the chat.", LOG)

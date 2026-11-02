###############################################################################
#  fi_10_figure1.R -- Figure 1 of the FI manuscript (capacity frontier, 2022)
#  (a) exposure capture and (b) recall against follow-up capacity K, for the
#  bank grade, XGB and LR ranked by PD, and the money-weighted lists selected on 2021
#  (XGB: R3_EL, LR: R2_EXP). Reads fi_out/fi03_frontier.csv.
#  Output: fi_out/fi10_figure1.png (300 dpi) and fi_out/fi10_figure1.pdf
###############################################################################
suppressPackageStartupMessages(library(data.table))
d <- fread(file.path("fi_out", "fi03_frontier.csv"))[run == "exante_main"]
S <- list(list("none", "R0_TIER", "Bank grade", "#8a8a85", 1),
          list("XGB__F_SP", "R1_PD", "XGB, PD", "#2a78d6", 2),
          list("XGB__F_SP", "R3_EL", "XGB, money-weighted", "#2a78d6", 1),
          list("LR__F_SP", "R1_PD", "LR, PD", "#eb6834", 2),
          list("LR__F_SP", "R2_EXP", "LR, money-weighted", "#eb6834", 1))
draw <- function() {
  layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE), heights = c(1, 0.13))
  par(mar = c(4.2, 4.4, 2.6, 1), mgp = c(2.6, 0.7, 0), las = 1, cex = 0.85, bty = "l")
  for (j in 1:2) {
    col <- c("exp_capture", "recall")[j]
    plot(NA, xlim = c(1, 40), ylim = c(0, 1), xlab = "Follow-up capacity K (% of portfolio)",
         ylab = c("Share of 2022 defaulted principal", "Share of 2022 defaulters")[j], xaxs = "i", yaxs = "i")
    abline(h = seq(0.2, 1, 0.2), col = "#ecebe7"); abline(v = c(13, 19, 38), col = "#d9d8d3")
    text(c(13.4, 19.4), 0.03, c("13%", "19%"), adj = c(0, 0), col = "#6b6a65", cex = 0.85)
    text(37.6, 0.03, "38%", adj = c(1, 0), col = "#6b6a65", cex = 0.85)
    for (s in S) { z <- d[cell == s[[1]] & rule == s[[2]]][order(K)]; lines(z$K, z[[col]], col = s[[4]], lwd = 2.2, lty = s[[5]]) }
    title(main = c("(a) Exposure capture", "(b) Recall")[j], adj = 0, font.main = 1, cex.main = 1)
  }
  par(mar = c(0, 0, 0, 0)); plot.new()
  legend("center", ncol = 5, bty = "n", text.width = NA, legend = sapply(S, `[[`, 3), col = sapply(S, `[[`, 4),
         lty = sapply(S, `[[`, 5), lwd = 2.2, cex = 0.9)
}
png(file.path("fi_out", "fi10_figure1.png"), width = 7.2, height = 3.6, units = "in", res = 300); draw(); invisible(dev.off())
pdf(file.path("fi_out", "fi10_figure1.pdf"), width = 7.2, height = 3.6); draw(); invisible(dev.off())
cat("Figure 1 written to fi_out/fi10_figure1.png and .pdf\n")

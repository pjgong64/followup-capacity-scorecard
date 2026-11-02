# =============================================================================
#  fi_01_run_exante.R  --  Study 3 (FI) ex-ante refit. COPY of 12_Factorial7_run.R; the published
#  runner is untouched. Differences from 12_Factorial7_run.R, and nothing else:
#    1. EXANTE_EXCLUDE (below) is removed from FIN: every feature built from a
#       year-end (31 December) value, the same date the outcome is assessed
#       (Gong, 19 Oct 2026). What remains is known at the start of the season:
#       contractual rate (customer grade, set 1 May from the previous year),
#       previous-year default, beginning-of-year balances (Outb, CLb, Depb),
#       plot / variety / calendar, and the start-of-season production estimates.
#    2. Output goes to <out_dir>/exante so no existing result is overwritten.
#  Main: fit 2020+2021, score 2022 (factorial7_out/exante).
# =============================================================================
# =============================================================================
#  12_Factorial7_run.R   —   7 feature sets x 5 model families
#  ---------------------------------------------------------------------------
#  Question:  what is Earth observation worth, on its own and alongside the two
#             things a lender already has (financial behaviour, spatial history)?
#
#  Arms      F · F+EO · F+SP · F+SP+EO · EO · SP · EO+SP
#  Families  LR, RF (h2o) · XGB (xgboost) · MLP, ST-CRAAN (keras)
#
#  Design points that make the table readable, and that are easy to get wrong:
#
#   1. SP is carved OUT of FIN. TE_ADM3RL is the sub-district default history,
#      i.e. the 0.149 AUC "persistent component". If it stays inside FIN then
#      the F arm already contains spatial history and F vs F+SP measures nothing.
#
#   2. Class imbalance is handled in every family, by that family's own
#      mechanism. A missing class weight in one arm produced a spurious 41.34pp
#      gap once already.
#
#   3. Comparison is at MATCHED ALERT VOLUME, never at a fixed 0.5 threshold.
#      A fixed cut inverted the XGBoost result once already.
#
#   4. Hyper-parameters are tuned on the EO-bearing arm and reused unchanged on
#      the arms without EO. The comparison is therefore biased IN FAVOUR of EO,
#      so "EO adds nothing" is a conservative conclusion.
#
#   5. The EO tensor is reshaped with an explicit column map and a stopifnot.
#      reticulate::array_reshape is C-order; R's dim<- is Fortran-order. Getting
#      this wrong scrambles month against channel silently.
#
#   6. Falsification check: EO+SP is a predictor resolved to (ADM3, Year), so it
#      must not exceed the 0.662 leave-one-out ceiling. If it does, the ceiling
#      or the pipeline is wrong, and that has to be resolved before either paper
#      goes out.
#
#  Usage:   Rscript 12_Factorial7_run.R            # runs / resumes everything
#           Rscript 12_Factorial7_run.R LR XGB     # only these families
#  Resume:  results are appended per cell; finished cells are skipped.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

t_start <- Sys.time()
source("12_Factorial7_config.R")

# The runner and the config move together. A stale config is the single most
# likely cause of "every cell FAILED": the fit functions read CFG$hp, and if it
# is absent every hyper-parameter silently becomes NULL, which each library
# then rejects at fit time -- inside try(), so it is recorded as FAILED rather
# than stopping the run.
.need_cfg <- c("hp", "arms", "dev_years", "test_years", "sp_cols", "eo_prefix")
.missing  <- .need_cfg[!.need_cfg %in% names(CFG)]
if (length(.missing))
  stop("12_Factorial7_config.R is out of date -- missing CFG$",
       paste(.missing, collapse = ", CFG$"),
       "\n  The config and the runner must be from the same version.",
       "\n  Copy the matching 12_Factorial7_config.R over the old one.")
for (.f in c("xgb", "lr", "rf", "mlp", "stcraan"))
  if (is.null(CFG$hp[[.f]]))
    stop("CFG$hp$", .f, " is missing from the config. Every ", toupper(.f),
         " cell would fail with a NULL hyper-parameter.")
set.seed(CFG$seed)

# ---- Study 2 / H2a: the class-weight sweep ----------------------------------
# alpha=<r>   sets the positive-to-negative weight ratio explicitly and sends
#             every output -- results, log, cache and saved scores -- to its own
#             sub-directory, so a sweep cannot disturb the factorial. With no
#             alpha= on the command line nothing changes: class_w() falls back to
#             N0/N1, the ratio it has always used, and writes where it always has.
# arms=<a,b>  restricts the arm grid, so a sweep can be run on one feature set
#             rather than the whole factorial. Seven values of alpha across five
#             families and every arm is a great deal of GPU time for a question
#             that one feature set answers.
.argv <- commandArgs(trailingOnly = TRUE)

ALPHA_RATIO <- NA_real_
.a <- grep("^alpha=", .argv, value = TRUE)
if (length(.a)) {
  ALPHA_RATIO <- suppressWarnings(as.numeric(sub("^alpha=", "", .a[1])))
  if (!is.finite(ALPHA_RATIO) || ALPHA_RATIO <= 0)
    stop("alpha= needs a positive number, got: ", .a[1])
  CFG$out_dir <- file.path(
    CFG$out_dir,
    paste0("alpha_", gsub("[.]", "p", format(ALPHA_RATIO, trim = TRUE,
                                             scientific = FALSE))))
  cat(sprintf("alpha sweep: positive/negative weight ratio %.4f\n  output -> %s\n",
              ALPHA_RATIO, CFG$out_dir))
}

# seed=<n>    refits under a different seed and sends the run to its own
#             sub-directory, so a set of repeats can be collected side by side.
#             This is how the repeat-fit variability of Section 4 is measured:
#             the published run is one draw, and the spread across draws is what
#             says whether a small difference in AUC means anything. Note that
#             the spread is NOT only seed variation -- h2o DRF across threads,
#             xgboost's hist tie-breaking and GPU training are not bitwise
#             reproducible even at a fixed seed, and a repeat samples all of it.
.s <- grep("^seed=", .argv, value = TRUE)
if (length(.s)) {
  .sv <- suppressWarnings(as.integer(sub("^seed=", "", .s[1])))
  if (is.na(.sv)) stop("seed= needs an integer, got: ", .s[1])
  CFG$seed <- .sv
  CFG$out_dir <- file.path(CFG$out_dir, paste0("repeat_seed", .sv))
  set.seed(CFG$seed)
  cat(sprintf("repeat fit: seed %d\n  output -> %s\n", .sv, CFG$out_dir))
}

SEL_ARMS <- NULL
.m <- grep("^arms=", .argv, value = TRUE)
if (length(.m)) {
  SEL_ARMS <- trimws(strsplit(sub("^arms=", "", .m[1]), ",")[[1]])
  .bad <- setdiff(SEL_ARMS, names(CFG$arms))
  if (length(.bad))
    stop("unknown arm: ", paste(.bad, collapse = ", "),
         "\n  known arms: ", paste(names(CFG$arms), collapse = ", "))
  cat("arms this run: ", paste(SEL_ARMS, collapse = ", "), "\n", sep = "")
}

# ---- FI ex-ante: drop every feature measured on 31 December ----------------
EXANTE_EXCLUDE <- c(
  "norm_log_Dep", "norm_log_Out", "norm_log_CL",
  "norm_win_Credit_Utilization_Rate", "norm_win_Liquidity_Buffer",
  "norm_win_Debt_to_Income", "norm_win_Bes", "norm_win_Proxy_DSR",
  "norm_win_Repayment_Rate", "norm_log_Debt_per_Rai",
  "norm_win_Est_DSCR10", "norm_win_Est_DSCR20",
  "norm_win_Debt_Reduction", "norm_win_Debt_Momentum",
  "norm_win_Saving_Growth", "norm_win_Saving_Momentum")
CFG$fin_exclude_extra <- unique(c(CFG$fin_exclude_extra, EXANTE_EXCLUDE))
CFG$out_dir <- file.path(CFG$out_dir, "exante")
cat(sprintf("FI ex-ante run: %d year-end features removed from FIN\n  output -> %s\n",
            length(EXANTE_EXCLUDE), CFG$out_dir))
dir.create(CFG$out_dir, showWarnings = FALSE, recursive = TRUE)
RES_FILE <- file.path(CFG$out_dir, "factorial7_results.csv")
.n_feat_now <- NA_integer_        # set per cell, recorded with the result

# The reporting helpers live in 12_Factorial7_report.R so there is a single
# implementation of the AUC matrix. Load them through a function that can be
# called again later: running this script a block at a time in RStudio can skip
# this line entirely, and then the between-cell table fails with
# "could not find function auc_matrix" after a cell has already been fitted.
ensure_report_helpers <- function() {
  if (exists("auc_matrix", mode = "function")) return(invisible(TRUE))
  f <- "12_Factorial7_report.R"
  if (!file.exists(f)) { say("cannot find %s -- the progress table is skipped", f)
                         return(invisible(FALSE)) }
  .defs_only <<- TRUE
  on.exit(.defs_only <<- FALSE, add = TRUE)
  source(f)
  invisible(exists("auc_matrix", mode = "function"))
}
.defs_only <- FALSE
ensure_report_helpers()

# ---------------------------------------------------------------------------
#  Writing results when the file might be locked
#  ---------------------------------------------------------------------------
#  On Windows an open Excel window holds an exclusive lock, and fwrite() then
#  fails. Losing a cell that took twenty minutes to fit because a spreadsheet
#  was open is not acceptable, so a blocked write retries briefly and then goes
#  to its own shard file. Shards are first-class: every reader below merges
#  them, so a run is never wrong, only untidy, and the next unlocked startup
#  folds them back in.
# ---------------------------------------------------------------------------
shard_files <- function()
  list.files(CFG$out_dir, pattern = "^factorial7_results\\.pending-.*\\.csv$",
             full.names = TRUE)

# colClasses is load-bearing, not tidiness. One of the arms is literally named
# "F", and fread() auto-detects a column of "T"/"F" as LOGICAL -- so a shard
# holding only F-arm rows comes back with arm = FALSE, the F cell never matches
# as done, and it reruns forever while showing up as an "FALSE" row in the
# matrix. Force the identifier columns to character on every read.
read_csv_safe <- function(f)
  tryCatch(fread(f, colClasses = list(character = c("family", "arm", "note"))),
           error = function(e) NULL)

read_results <- function() {
  fs <- c(if (file.exists(RES_FILE)) RES_FILE, shard_files())
  if (!length(fs)) return(NULL)
  d <- rbindlist(lapply(fs, read_csv_safe), use.names = TRUE, fill = TRUE)
  if (!nrow(d)) return(NULL)
  d[, family := as.character(family)][, arm := as.character(arm)]
  d[]
}

safe_write_row <- function(row_dt) {
  for (attempt in 1:5) {
    ok <- tryCatch({
      fwrite(row_dt, RES_FILE, append = file.exists(RES_FILE),
             col.names = !file.exists(RES_FILE))
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(invisible(TRUE))
    if (attempt == 1)
      say("  results file is locked (Excel?) -- retrying for a few seconds")
    Sys.sleep(2)
  }
  shard <- file.path(CFG$out_dir,
                     sprintf("factorial7_results.pending-%s.csv",
                             format(Sys.time(), "%Y%m%d-%H%M%OS3")))
  fwrite(row_dt, shard)
  say("  *** %s is locked. This cell was saved to %s instead.", RES_FILE, shard)
  say("      Nothing is lost: every reader merges the shards. Close the file")
  say("      and the next startup folds them back into the main results.")
  invisible(FALSE)
}

merge_shards <- function() {
  sh <- shard_files()
  if (!length(sh)) return(invisible())
  d <- read_results()
  ok <- tryCatch({ fwrite(d, RES_FILE); TRUE }, error = function(e) FALSE)
  if (!ok) {
    say("%d pending shard(s) present but %s is still locked -- leaving them in",
        length(sh), RES_FILE)
    say("  place. They are read as part of the results, so nothing is missing.")
    return(invisible())
  }
  unlink(sh)
  say("merged %d pending shard(s) into %s", length(sh), RES_FILE)
}
LOG_FILE <- file.path(CFG$out_dir, "factorial7_log.txt")

say <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", sprintf(...))
  cat(msg, "\n"); cat(msg, "\n", file = LOG_FILE, append = TRUE)
}

args_in <- commandArgs(trailingOnly = TRUE)
# "auto" / "-y" / "--auto" anywhere on the command line disables the prompt.
flags        <- tolower(args_in) %in%
                c("auto", "-y", "--auto", "redo", "--redo", "-r") |
                grepl("^(alpha|arms|seed)=", args_in)
sel_families <- args_in[!flags]
if (!length(sel_families)) sel_families <- CFG$families
bad <- setdiff(sel_families, CFG$families)
if (length(bad))
  stop("unknown family: ", paste(bad, collapse = ", "),
       "\n  known families: ", paste(CFG$families, collapse = ", "),
       "\n  flags: \"auto\" = no prompt between cells,",
       " \"redo\" = rerun even cells that already succeeded")
stopifnot(all(sel_families %in% CFG$families))

say("families this run: %s", paste(sel_families, collapse = ", "))
# Which R is this, and where does it look for packages? A mismatch between the
# Rscript on the command line and the R that RStudio uses is the usual reason a
# package that is plainly installed reads as missing here.
say("%s", R.version.string)
say("library paths: %s", paste(.libPaths(), collapse = " ; "))

# ---------------------------------------------------------------------------
#  Which families can actually run on this machine?
#  Checked ONCE, before any fitting. Without this a missing TensorFlow produces
#  eleven identical failures, one per cell, each after its own data prep -- and
#  eleven FAILED rows that look like eleven problems.
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
#  Bind reticulate to the right Python BEFORE anything touches it. The binding
#  is fixed for the whole session at the first Python call, so this has to run
#  before the availability check below -- afterwards it is too late and
#  use_condaenv() silently has no effect.
# ---------------------------------------------------------------------------
bind_python <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) return(invisible())
  if (!is.null(CFG$python_env)) {
    # Resolve the NAME to a folder on disk first. use_condaenv() would hand the
    # name to conda.exe, which is not on PATH in a double-clicked cmd window --
    # that is the "Unable to find conda binary" error, and it says nothing about
    # whether the environment exists. find_conda_python() looks for the folder
    # instead, so the same env binds from cmd and from RStudio alike.
    # use_condaenv() stays as the fallback for a layout the search misses.
    # exists() guard: find_conda_python() is defined in the config, so an older
    # config beside a newer runner would abort here with "could not find
    # function" instead of simply falling back to the conda-name route.
    path <- if (exists("find_conda_python"))
              tryCatch(find_conda_python(CFG$python_env), error = function(e) NA_character_)
            else NA_character_
    ok <- tryCatch({
      if (!is.na(path)) {
        reticulate::use_python(path, required = TRUE)
        say("python: %s -> %s", CFG$python_env, path)
      } else {
        reticulate::use_condaenv(CFG$python_env, required = TRUE)
        say("python: requested %s", CFG$python_env)
      }
      TRUE
    }, error = function(e) { say("could not bind Python: %s", conditionMessage(e)); FALSE })
    if (!ok) say("  (CFG$python_env = '%s' -- see the environment list below)", CFG$python_env)
  }
  cfgp <- tryCatch(reticulate::py_config(), error = function(e) NULL)
  if (!is.null(cfgp)) say("python: using %s", cfgp$python)
  invisible()
}

# When tensorflow is missing, say WHERE it looked and where it does exist.
# "not installed" is wrong and misleading when the package is installed in a
# different environment, which is the common case on a machine with rgee.
python_hint <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) return(invisible())
  cfgp <- tryCatch(reticulate::py_config(), error = function(e) NULL)
  if (!is.null(cfgp)) say("  reticulate is bound to: %s", cfgp$python)
  # conda_list() needs conda.exe. conda_envs() reads the folders directly, so
  # it still answers on a machine where conda is not on PATH -- which is the
  # machine most likely to be asking.
  envs <- if (exists("conda_envs")) tryCatch(conda_envs(), error = function(e) NULL) else NULL
  if (is.null(envs) || !nrow(envs)) {
    say("  no conda environments found on disk under the usual install roots.")
  } else {
    say("  environments found on disk (tf = tensorflow is in site-packages):")
    for (i in seq_len(nrow(envs)))
      say("    %-3s %-24s %s", if (envs$has_tf[i]) "tf" else "", envs$name[i], envs$python[i])
    pick <- if (any(envs$has_tf)) envs$python[envs$has_tf][1] else envs$python[1]
    say("  Put the FULL PATH in the config -- it needs no conda binary:")
    say("    CFG$python_env <- \"%s\"", pick)
  }
  say("  In that environment, tensorflow must be importable from R:")
  say("    reticulate::py_module_available(\"tensorflow\")   should be TRUE")
  invisible()
}

family_ready <- function(fam) {
  need_pkg <- switch(fam, LR = "h2o", RF = "h2o", XGB = "xgboost", "keras")
  # requireNamespace() returns FALSE for two completely different situations,
  # and reporting both as "not installed" sends you looking in the wrong place:
  #   (a) the package really is absent from every .libPaths() entry -- usually
  #       because Rscript is a DIFFERENT R version from the one RStudio uses,
  #       so it reads win-library/<other version>;
  #   (b) the package is there but its .onLoad hook throws -- which is what
  #       keras does when it cannot reach a working Python.
  if (!need_pkg %in% rownames(installed.packages()))
    return(sprintf("'%s' is not installed in any library this R sees: %s",
                   need_pkg, paste(.libPaths(), collapse = " ; ")))
  load_err <- tryCatch({ loadNamespace(need_pkg); NA_character_ },
                       error = function(e) conditionMessage(e))
  if (!is.na(load_err))
    return(sprintf("'%s' is installed at %s but failed to load: %s",
                   need_pkg, dirname(find.package(need_pkg)),
                   gsub("[\r\n]+", " ", substr(load_err, 1, 200))))
  if (fam %in% c("MLP", "STCRAAN")) {
    # Probe the module only. Do NOT touch keras here: py_require_legacy_keras()
    # has to be the first thing that decides which keras the R API talks to, and
    # that happens in keras_start(). Importing keras during a readiness check
    # would settle the choice early, with Keras 3.
    ok <- tryCatch({
      requireNamespace("reticulate", quietly = TRUE) &&
        reticulate::py_module_available("tensorflow")
    }, error = function(e) FALSE)
    if (!isTRUE(ok))
      return(sprintf("TensorFlow not importable from the bound Python%s",
                     if (is.null(CFG$python_env)) " (CFG$python_env is NULL)"
                     else sprintf(" (CFG$python_env = '%s')", CFG$python_env)))
  }
  NA_character_
}

cat("\n---------------- family availability ----------------\n")
if (any(sel_families %in% c("MLP", "STCRAAN"))) bind_python()
fam_why <- vapply(sel_families, family_ready, character(1))
for (f in sel_families)
  cat(sprintf("  %-8s %s\n", f,
              if (is.na(fam_why[[f]])) "ready" else paste("SKIP --", fam_why[[f]])))
skip_fam <- sel_families[!is.na(fam_why)]
if (length(skip_fam)) {
  cat("\n  These families are skipped entirely -- no rows are written for them,\n")
  cat("  so they are NOT recorded as failures and will run normally once the\n")
  cat("  dependency is resolved.\n")
  if (any(skip_fam %in% c("MLP", "STCRAAN"))) {
    cat("\n  TensorFlow: if it IS installed, reticulate is bound to the wrong\n")
    cat("  Python. Set CFG$python_env in the config -- details:\n")
    python_hint()
  }
  sel_families <- setdiff(sel_families, skip_fam)
}
cat("\n")
if (!length(sel_families))
  stop("no family on the command line can run on this machine (see above).")


# =============================================================================
#  1. LOAD AND RESOLVE THE SCHEMA
# =============================================================================

if (!file.exists(CFG$data_file))
  stop("data file not found: ", CFG$data_file, "\nworking directory: ", getwd())

# ---- separator ---------------------------------------------------------------
# Getting this wrong reads the whole line as one column and then EVERY column
# lookup fails at once, which looks like a naming problem but is not.
SEP_CANDIDATES <- c(pipe = "|", tab = "\t", comma = ",", semicolon = ";", space = " ")
hdr_line <- readLines(CFG$data_file, n = 1L, warn = FALSE)
field_counts <- vapply(SEP_CANDIDATES,
                       function(s) length(strsplit(hdr_line, s, fixed = TRUE)[[1]]), integer(1))
sep_use <- CFG$sep
if (is.null(sep_use)) {
  sep_use <- SEP_CANDIDATES[[which.max(field_counts)]]
  say("separator auto-detected: %s (%d fields in the header)",
      names(SEP_CANDIDATES)[which.max(field_counts)], max(field_counts))
}

say("reading %s (sep = %s)", CFG$data_file, encodeString(sep_use, quote = "\""))
dt <- fread(CFG$data_file, sep = sep_use, showProgress = TRUE)
say("loaded %s rows x %s cols", format(nrow(dt), big.mark = ","), ncol(dt))

if (ncol(dt) < 5) {
  stop("the file read as ", ncol(dt), " column(s) — the separator is wrong.\n",
       "  header splits into: ",
       paste(sprintf("%s=%d", names(field_counts), field_counts), collapse = "  "), "\n",
       "  configured CFG$sep = ", encodeString(sep_use, quote = "\""),
       "; the header suggests ", names(SEP_CANDIDATES)[which.max(field_counts)],
       "\n  set CFG$sep accordingly, or CFG$sep <- NULL to auto-detect.")
}

need <- c(CFG$col_id, CFG$col_year, CFG$col_adm3, CFG$col_target)
miss <- setdiff(need, names(dt))
if (length(miss)) {
  stop("missing key column(s): ", paste(miss, collapse = ", "),
       "\n  id candidates    : ", paste(head(grep("^(cid|id|cust|borrow)", names(dt), ignore.case = TRUE, value = TRUE), 6), collapse = ", "),
       "\n  year candidates  : ", paste(head(grep("year|yr", names(dt), ignore.case = TRUE, value = TRUE), 6), collapse = ", "),
       "\n  adm3 candidates  : ", paste(head(grep("adm3|tambon|subdist", names(dt), ignore.case = TRUE, value = TRUE), 6), collapse = ", "),
       "\n  target candidates: ", paste(head(grep("default|npl|target|label|bad", names(dt), ignore.case = TRUE, value = TRUE), 6), collapse = ", "),
       "\n\nRun  Rscript 12_Factorial7_schema.R  for the full column list.")
}

# ---- EO columns ------------------------------------------------------------
# The month suffix is resolved against the file rather than assumed. Matching is
# by exact prefix, so "norm_NDVI" can never pick up "norm_SAI_NDVI".
eo_pref <- CFG$eo_prefix
if (isTRUE(CFG$include_anomaly)) eo_pref <- c(eo_pref, CFG$eo_prefix_anomaly)

MONTH_PATTERNS <- list(
  "<base><m>"   = function(b, m) paste0(b, m),
  "<base>_<m>"  = function(b, m) paste0(b, "_", m),
  "<base>.<m>"  = function(b, m) paste0(b, ".", m),
  "<base>M<m>"  = function(b, m) paste0(b, "M", m),
  "<base>_M<m>" = function(b, m) paste0(b, "_M", m),
  "<base><mm>"  = function(b, m) paste0(b, sprintf("%02d", m)),
  "<base>_<mm>" = function(b, m) paste0(b, "_", sprintf("%02d", m))
)

detect_month_pattern <- function(bases, months, nm) {
  if (!is.null(CFG$month_pattern)) {
    if (!CFG$month_pattern %in% names(MONTH_PATTERNS))
      stop("CFG$month_pattern must be one of: ", paste(names(MONTH_PATTERNS), collapse = ", "))
    return(CFG$month_pattern)
  }
  for (v in names(MONTH_PATTERNS)) {
    f <- MONTH_PATTERNS[[v]]
    if (all(unlist(lapply(bases, function(b) f(b, months))) %in% nm)) return(v)
  }
  NA_character_
}

eo_diagnose <- function(bases, months, nm) {
  msg <- c("Could not resolve the EO columns.", "")
  for (i in seq_along(bases)) {
    b <- bases[[i]]
    hits <- nm[startsWith(nm, b)]
    sfx  <- substring(hits, nchar(b) + 1L)
    sfx  <- sort(unique(sfx[grepl("^[^A-Za-z]*[0-9]+$", sfx)]))
    msg <- c(msg, sprintf("  %-12s base '%s': %d columns start with it%s",
                          names(bases)[i], b, length(hits),
                          if (length(sfx)) sprintf(", suffixes {%s}",
                            paste(head(sfx, 14), collapse = ",")) else ""))
  }
  env_like <- grep("NDVI|NDWI|LST|Precip|Rain|SMAP|NTL|VHI|Heat|SAI", nm,
                   ignore.case = TRUE, value = TRUE)
  msg <- c(msg, "",
    sprintf("  %d columns in the file look environmental. Distinct bases:", length(env_like)),
    paste0("    ", paste(sort(unique(sub("[^A-Za-z]*[0-9]+$", "", env_like))), collapse = "\n    ")),
    "", "  Run  Rscript 12_Factorial7_schema.R  for the full picture.")
  paste(msg, collapse = "\n")
}

pat <- detect_month_pattern(eo_pref, CFG$months, names(dt))
if (is.na(pat)) stop(eo_diagnose(eo_pref, CFG$months, names(dt)))
say("EO month pattern: %s", pat)

build_eo_cols <- function(prefixes, months, pattern = pat) {
  # channel-major: all months of channel 1, then channel 2, ...
  f <- MONTH_PATTERNS[[pattern]]
  unlist(lapply(prefixes, function(p) f(p, months)), use.names = FALSE)
}
EO_COLS <- build_eo_cols(eo_pref, CFG$months)
stopifnot(all(EO_COLS %in% names(dt)))
N_CH <- length(eo_pref); N_MO <- length(CFG$months)
stopifnot(length(EO_COLS) == N_CH * N_MO)
say("EO: %d channels x %d months = %d columns", N_CH, N_MO, length(EO_COLS))

# ---- SP columns -------------------------------------------------------------
SP_COLS <- intersect(CFG$sp_cols, names(dt))
sp_missing <- setdiff(CFG$sp_cols, names(dt))
if (length(sp_missing)) {
  stop("SP columns not found: ", paste(sp_missing, collapse = ", "),
       "\n  candidates in this file: ",
       paste(head(grep("TE_|ADM|prov|region|tambon|geo", names(dt), ignore.case = TRUE, value = TRUE), 12), collapse = ", "),
       "\nSP must name the spatial-identity encodings (e.g. TE_ADM3RL)")
}
say("SP: %d columns -> %s", length(SP_COLS), paste(SP_COLS, collapse = ", "))

# ---- which SP candidates are actually areal? --------------------------------
# "Areal" means constant across every borrower in the same (ADM3, Year) cell.
# A column that varies inside a cell carries borrower-level information; if it
# sits in SP then the EO+SP arm is no longer bounded by the 0.662 leave-one-out
# ceiling and the falsification test is void. Decided by measurement, not by the
# column name.
SP_NONAREAL <- character(0)
if (isTRUE(CFG$check_areal)) {
  # one column per EO channel, not just the two ends: with 11 channels a single
  # non-areal channel is exactly the kind of thing that hides in the middle.
  eo_probe <- EO_COLS[seq(1, length(EO_COLS), by = N_MO)]
  probe <- c(SP_COLS, eo_probe)
  u <- dt[, lapply(.SD, uniqueN), by = c(CFG$col_adm3, CFG$col_year), .SDcols = probe]
  prof <- data.table(
    column = probe,
    max_per_cell  = vapply(probe, function(cc) max(u[[cc]]), numeric(1)),
    mean_per_cell = vapply(probe, function(cc) round(mean(u[[cc]]), 2), numeric(1)),
    pct_cells_gt1 = vapply(probe, function(cc) round(100 * mean(u[[cc]] > 1), 1), numeric(1)))
  prof[, verdict := ifelse(max_per_cell == 1, "areal", "borrower-level")]
  cat("\n--- distinct values within one (ADM3, Year) cell ---\n")
  print(prof, row.names = FALSE)

  bad_eo <- prof[column %in% EO_COLS & verdict != "areal", column]
  if (length(bad_eo))
    stop("EO column(s) vary within a cell: ", paste(bad_eo, collapse = ", "),
         "\nBoth papers rest on EO being resolved to the sub-district. ",
         "Investigate before going further.")

  SP_NONAREAL <- prof[column %in% SP_COLS & verdict != "areal", column]
  if (length(SP_NONAREAL)) {
    action <- if (is.null(CFG$sp_nonareal_action)) "fin" else CFG$sp_nonareal_action
    cat("\n")
    say("%d SP candidate(s) are not areal: %s",
        length(SP_NONAREAL), paste(SP_NONAREAL, collapse = ", "))
    if (action == "stop") {
      stop("CFG$sp_nonareal_action is 'stop'. Set it to 'fin' or 'drop', or edit CFG$sp_cols.")
    } else if (action == "drop") {
      say("  -> dropped from the study entirely (CFG$sp_nonareal_action = 'drop')")
      CFG$fin_exclude_extra <- unique(c(CFG$fin_exclude_extra, SP_NONAREAL))
    } else if (action == "split") {
      # spatial-but-sub-cell columns STAY in SP; only non-spatial ones go to FIN.
      fine <- intersect(SP_NONAREAL, CFG$sp_fine_cols)
      other <- setdiff(SP_NONAREAL, CFG$sp_fine_cols)
      say("  -> split (CFG$sp_nonareal_action = 'split')")
      if (length(fine))
        say("     spatial, sub-cell -> STAY in SP: %s", paste(fine, collapse = ", "))
      if (length(other))
        say("     not spatial       -> FIN:        %s", paste(other, collapse = ", "))
      CFG$fin_exclude_extra <- unique(c(CFG$fin_exclude_extra, fine))
      SP_COLS <- setdiff(SP_COLS, other)
      SP_NONAREAL <- fine
    } else {
      say("  -> moved to FIN (CFG$sp_nonareal_action = 'fin'), so SP stays purely areal")
      say("     NOTE: F then contains some geographic flavour, which UNDERSTATES what")
      say("     SP adds in F -> F+SP. It does not affect F+SP -> F+SP+EO, the EO test.")
      SP_COLS <- setdiff(SP_COLS, SP_NONAREAL)
      SP_NONAREAL <- character(0)
    }
  }
  # SPA = the areal-only subset of SP. The 0.662 falsification test uses THIS,
  # never SP, because only SPA is constant within (ADM3, Year).
  SPA_COLS <<- setdiff(SP_COLS, SP_NONAREAL)
  if (!length(SPA_COLS))
    stop("no areal SP column survives — the EO+SPA falsification test cannot be built.\n",
         "Add a target encoding of ADM3 alone to CFG$sp_cols.")
  if (!length(SP_COLS))
    stop("no areal SP column survives — the SP and EO+SP arms cannot be built.\n",
         "Add a purely areal encoding (e.g. a target encoding of ADM3 alone) to CFG$sp_cols.")
  say("SP  (all spatial): %s", paste(SP_COLS, collapse = ", "))
  say("SPA (areal only,  bounded by %.3f): %s",
      CFG$anchor$areal_ceiling_test, paste(SPA_COLS, collapse = ", "))
  cat("\n")
}

if (!exists("SPA_COLS")) SPA_COLS <- SP_COLS   # check_areal off: SPA == SP

# ---- FIN = everything else that is numeric ---------------------------------
all_env <- unique(c(EO_COLS,
  tryCatch(build_eo_cols(c(CFG$eo_prefix, CFG$eo_prefix_anomaly), CFG$months),
           error = function(e) character(0))))
non_fin <- unique(c(need, all_env, SP_COLS, CFG$fin_exclude_extra))
.miss_ex <- setdiff(EXANTE_EXCLUDE, names(dt))
if (length(.miss_ex))
  stop("ex-ante exclusion names not in the data file: ", paste(.miss_ex, collapse = ", "))
.left_ex <- intersect(EXANTE_EXCLUDE, setdiff(names(dt), non_fin))
if (length(.left_ex)) stop("year-end features still in FIN: ", paste(.left_ex, collapse = ", "))
# SP_COLS has already had the non-areal ones removed, so they fall into FIN
cand <- setdiff(names(dt), non_fin)
is_num <- vapply(cand, function(cc) is.numeric(dt[[cc]]) || is.integer(dt[[cc]]), logical(1))
FIN_COLS <- cand[is_num]
dropped  <- cand[!is_num]

say("FIN: %d columns", length(FIN_COLS))
cat("\n--- FIN columns (confirm nothing spatial or environmental is here) ---\n")
print(FIN_COLS)
if (length(dropped)) {
  cat("\n--- dropped from FIN because non-numeric ---\n"); print(dropped)
}
# guard: a spatial encoding hiding in FIN would silently break the factorial
suspicious <- grep("ADM|TE_|prov|region|lat|lon|geo", FIN_COLS,
                   ignore.case = TRUE, value = TRUE)
# Columns already adjudicated by measurement, so they do not need re-flagging.
suspicious <- setdiff(suspicious, CFG$fin_known_nonspatial)
if (length(suspicious)) {
  cat("\n"); say("WARNING: these FIN columns look spatial -> move to CFG$sp_cols if so:")
  print(suspicious)
}
cat("\n")

# ---- splits -----------------------------------------------------------------
dt[, .row_dev := get(CFG$col_year) %in% CFG$dev_years]
dt[, .row_tst := get(CFG$col_year) %in% CFG$test_years]

# The whole factorial hangs off this split. If the configured years are not the
# values in the file, every arm is built on the wrong rows -- and with zero test
# rows the AUCs come back as NA rather than as an error, which is worse.
if (!sum(dt$.row_dev) || !sum(dt$.row_tst))
  stop("CFG$dev_years / CFG$test_years do not match the data.\n",
       "  values of ", CFG$col_year, " in the file: ",
       paste(sort(unique(dt[[CFG$col_year]])), collapse = ", "), "\n",
       "  CFG$dev_years  = ", paste(CFG$dev_years,  collapse = ", "),
       "  -> ", sum(dt$.row_dev), " rows\n",
       "  CFG$test_years = ", paste(CFG$test_years, collapse = ", "),
       "  -> ", sum(dt$.row_tst), " rows")
if (any(dt$.row_dev & dt$.row_tst))
  stop("a year appears in BOTH CFG$dev_years and CFG$test_years. ",
       "The out-of-time evaluation would not be out of time.")
say("split: %d dev rows (%s), %d test rows (%s)",
    sum(dt$.row_dev), paste(CFG$dev_years,  collapse = "/"),
    sum(dt$.row_tst), paste(CFG$test_years, collapse = "/"))
idx_dev <- which(dt$.row_dev)
idx_tst <- which(dt$.row_tst)
stopifnot(length(idx_dev) > 0, length(idx_tst) > 0)

# The target may arrive as numeric, character ("0"/"1") or factor. as.integer()
# on a FACTOR returns the level index, not the label, so a factor c("0","1")
# silently becomes 1/2 -- the 0 class disappears and every AUC breaks. Coerce
# through the label in that case.
as_label <- function(z) {
  if (is.factor(z))    return(suppressWarnings(as.integer(as.character(z))))
  if (is.character(z)) return(suppressWarnings(as.integer(z)))
  as.integer(z)
}
y_all <- as_label(dt[[CFG$col_target]])

# Stratified sampling, used for BOTH the smoke-test subsample and the fit/val
# split. Simple random sampling leaves the realised default rate free to drift,
# and the rate is not just cosmetic: class_w() is computed from it, so two runs
# at the same dev_fraction would be reweighted differently and their AUCs would
# not be comparable. Sampling each class at the same rate pins the prevalence.
strat_take <- function(idx, y, frac) {
  out <- integer(0)
  for (cls in sort(unique(y))) {
    in_cls <- idx[y == cls]
    k <- floor(length(in_cls) * frac)
    if (k < 1L) stop("stratified sample would take 0 rows of class ", cls,
                     " (only ", length(in_cls), " available at frac ", frac,
                     "). Raise CFG$dev_fraction.")
    # NOT sample(in_cls, k): when in_cls has length 1, sample() treats it as a
    # RANGE and draws from 1:in_cls, returning a row index that was never in the
    # stratum. Index into the vector instead, which has no such special case.
    out <- c(out, in_cls[sample.int(length(in_cls), k)])
  }
  sort(out)
}

if (CFG$dev_fraction < 1) {
  before <- mean(y_all[idx_dev])
  idx_dev <- strat_take(idx_dev, y_all[idx_dev], CFG$dev_fraction)
  say("SMOKE TEST: using %s of %s dev rows, stratified (default rate %.3f%% -> %.3f%%)",
      format(length(idx_dev), big.mark = ","),
      format(sum(dt$.row_dev), big.mark = ","),
      100 * before, 100 * mean(y_all[idx_dev]))
}
# the test set is never subsampled: AUC on a small test set quantizes

y_dev <- y_all[idx_dev]
y_tst <- y_all[idx_tst]

# AUC is undefined unless BOTH classes are present. Without this guard the run
# fits every cell for real, then reports test AUC as a bare NA -- which reads
# like a model failure when it is a data problem, and costs the whole grid.
check_y <- function(y, what) {
  n_na <- sum(is.na(y))
  tab  <- table(y, useNA = "no")
  say("%s: %s rows | %s | %d NA",
      what, format(length(y), big.mark = ","),
      paste(sprintf("%s=%s", names(tab), format(as.integer(tab), big.mark = ",")),
            collapse = "  "), n_na)
  if (n_na)
    stop(what, " contains ", n_na, " missing values in column '",
         CFG$col_target, "'. Every AUC computed on it would be wrong.")
  if (length(tab) < 2)
    stop(what, " has only ONE class (", paste(names(tab), collapse = ""), ").\n",
         "  AUC is undefined, so this arm can never produce a number.\n",
         "  Check that '", CFG$col_target, "' is populated for ",
         CFG$col_year, " in {", paste(CFG$test_years, collapse = ","), "}.\n",
         "  Values of ", CFG$col_target, " over the whole file: ",
         paste(utils::capture.output(print(table(y_all, useNA = "ifany"))),
               collapse = " | "))
}
check_y(y_dev, "dev")
check_y(y_tst, "test")
say("dev %s rows (%.2f%% default) | test %s rows (%.2f%% default)",
    format(length(idx_dev), big.mark = ","), 100 * mean(y_dev),
    format(length(idx_tst), big.mark = ","), 100 * mean(y_tst))

# ---- per-borrower test scores, kept for the significance tests --------------
# Every cell computes fit$tst, takes AUC and the alert metrics from it, and then
# drops it. A difference between two AUCs has no standard error unless both
# score vectors still exist, so each one is written to CFG$out_dir/pred/ and
# assembled later by assemble_test_scores.R. The keys go out once here, in the
# same row order the scores use (idx_tst), so the two line up without a join.
# The test set is never subsampled, so this order is stable across cells.
PRED_DIR <- file.path(CFG$out_dir, "pred")
dir.create(PRED_DIR, showWarnings = FALSE, recursive = TRUE)
saveRDS(data.table(y    = as.integer(y_tst),
                   adm3 = dt[[CFG$col_adm3]][idx_tst],
                   id   = dt[[CFG$col_id]][idx_tst]),
        file.path(PRED_DIR, "keys_test.rds"))
say("prediction keys -> %s (%s rows)",
    file.path(PRED_DIR, "keys_test.rds"), format(length(idx_tst), big.mark = ","))

# ---- one fit/val split, shared by every family ------------------------------
# Early stopping needs held-out data. Using the training set for it (which an
# earlier draft did for xgboost) never triggers, because training AUC only
# improves. The same split is reused everywhere so the families are comparable.
set.seed(CFG$seed)
# The validation split is stratified for the same reason, plus one of its own:
# early stopping watches val AUC, and at a small dev_fraction an unstratified
# 15% slice can end up with very few positives, which makes that AUC jumpy and
# stops training at an arbitrary epoch.
idx_val <- strat_take(idx_dev, y_all[idx_dev], 0.15)
idx_fit <- setdiff(idx_dev, idx_val)
y_fit <- y_all[idx_fit]
y_val <- y_all[idx_val]
say("default rate  fit %.3f%%  val %.3f%%  test %.3f%%",
    100 * mean(y_fit), 100 * mean(y_val), 100 * mean(y_tst))
say("fit %s | val %s | test %s",
    format(length(idx_fit), big.mark = ","),
    format(length(idx_val), big.mark = ","),
    format(length(idx_tst), big.mark = ","))

# ---- development keys, for the Study 2 calibration grid ---------------------
# in_val marks the 15% no model was fitted on. Platt and isotonic recalibration
# are fitted on those rows only: fitted on idx_fit they would learn the model's
# in-sample over-confidence and then apply that correction to 2022, which makes
# the calibration worse rather than better. Early stopping did watch these rows,
# so they are not perfectly clean either, but the optimism there comes from a
# stopping rule rather than from a fitted label.
saveRDS(data.table(y      = as.integer(y_all[idx_dev]),
                   adm3   = dt[[CFG$col_adm3]][idx_dev],
                   id     = dt[[CFG$col_id]][idx_dev],
                   in_val = idx_dev %in% idx_val),
        file.path(PRED_DIR, "keys_dev.rds"))
say("development keys -> %s (%s rows, %s held out)",
    file.path(PRED_DIR, "keys_dev.rds"),
    format(length(idx_dev), big.mark = ","),
    format(length(idx_val), big.mark = ","))

# Class reweighting, identical in every family.
#
# Only the RATIO w1/w0 moves the decision boundary. The scale is fixed so that
# mean(w) == 1 whatever the ratio, because for the neural families a larger mean
# weight is also a larger effective step size, and without this a sweep over the
# ratio would confound "heavier positives" with "faster training" -- the curve
# would then be partly a learning-rate curve and would not mean what it appears
# to mean.
#
# At the default ratio N0/N1 this returns exactly what it always did:
#   r = N0/N1  ->  w0 = 1/(2*p0) = N/(2*N0),  w1 = 1/(2*p1) = N/(2*N1)
# so a run without alpha= reproduces the factorial bit for bit.
class_w <- function(y) {
  n  <- length(y)
  n1 <- max(1, sum(y == 1)); n0 <- max(1, sum(y == 0))
  r  <- if (is.na(ALPHA_RATIO)) n0 / n1 else ALPHA_RATIO
  p1 <- n1 / n; p0 <- n0 / n
  w0 <- 1 / (p1 * r + p0)
  ifelse(y == 1, r * w0, w0)
}

# =============================================================================
#  2. EVALUATION
# =============================================================================

fast_auc <- function(score, y, what = "") {
  # exact Mann-Whitney AUC with tie correction; O(n log n), no subsampling
  if (length(score) != length(y)) {
    say("  AUC %s: %d scores but %d labels -- returning NA",
        what, length(score), length(y))
    return(NA_real_)
  }
  if (anyNA(score)) {
    say("  AUC %s: %d of %d predicted scores are NA -- returning NA",
        what, sum(is.na(score)), length(score))
    return(NA_real_)
  }
  # as.numeric is NOT cosmetic. sum() on a logical returns an INTEGER, and
  # n1 * n0 in integer arithmetic overflows past 2,147,483,647 -- R then returns
  # NA with only a warning. With 180,504 positives and 1,316,217 negatives the
  # product is 2.4e11, so every AUC on the full test set came back NA while the
  # same code was fine on a 60k smoke sample. Keep these as doubles.
  n1 <- as.numeric(sum(y == 1, na.rm = TRUE))
  n0 <- as.numeric(sum(y == 0, na.rm = TRUE))
  if (n1 == 0 || n0 == 0) {
    say("  AUC %s: labels are single-class (n1=%.0f, n0=%.0f) -- returning NA",
        what, n1, n0)
    return(NA_real_)
  }
  r <- rank(score, ties.method = "average")     # rank() returns doubles
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Self-test at load. The overflow above produced NA silently for months of
# compute; a two-microsecond check makes that impossible to repeat.
local({
  # Save and restore the RNG stream. A diagnostic that reseeds the generator
  # would silently change every sample() taken after it, so the run would not be
  # reproducible and the cause would be this check rather than the analysis.
  had <- exists(".Random.seed", envir = .GlobalEnv)
  old <- if (had) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(if (had) assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)

  n <- 3e6; set.seed(1)
  y <- as.integer(runif(n) < 0.12)
  a <- fast_auc(as.numeric(y) + rnorm(n, 0, 0.5), y, "self-test")
  if (!is.finite(a) || a < 0.5 || a > 1)
    stop("fast_auc self-test failed (got ", a, "). Do not trust any AUC.")
  say("fast_auc self-test OK at n = %s (AUC %.4f)", format(n, big.mark = ","), a)
})

eval_at_alerts <- function(score, y, rates) {
  rbindlist(lapply(rates, function(a) {
    thr  <- as.numeric(quantile(score, 1 - a, names = FALSE, type = 7))
    pred <- score >= thr
    tp <- sum(pred & y == 1L); fn <- sum(!pred & y == 1L); na <- sum(pred)
    data.table(alert_rate = a, n_alert = na,
               recall    = tp / max(1L, tp + fn),
               precision = tp / max(1L, na),
               fn        = fn)
  }))
}

record <- function(family, arm, auc_dev, auc_tst, alerts, secs, note = "") {
  wide <- as.list(setNames(
    c(alerts$recall, alerts$fn),
    c(sprintf("recall_a%02d", round(alerts$alert_rate * 100)),
      sprintf("fn_a%02d",     round(alerts$alert_rate * 100)))))
  row <- c(list(family = family, arm = arm,
                auc_dev = auc_dev, auc_test = auc_tst,
                n_dev = length(idx_dev), n_test = length(idx_tst),
                n_feat = .n_feat_now, dev_fraction = CFG$dev_fraction,
                secs = round(secs, 1), note = note,
                stamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")), wide)
  safe_write_row(as.data.table(row))
  say("  %-8s %-9s  dev %.5f  test %.5f  (%.0fs) %s",
      family, arm, auc_dev, auc_tst, secs, note)
}

# A cell counts as done ONLY if it produced a number. Treating a FAILED row as
# done was a real bug: one bad run wrote 28 failure rows, and every later run
# then skipped all 28 as "already finished", so the grid could never be retried
# without deleting the results file by hand.
# A finished cell only counts as done if it was produced under the SAME setup.
# Without this, switching CFG$dev_fraction from 0.02 to 1 would skip every cell
# already fitted on 2% of the data and quietly mix smoke-test numbers into the
# final table -- the worst kind of failure, because the output looks complete.
done_cells <- function(verbose = FALSE) {
  d <- read_results()
  if (is.null(d) || !nrow(d)) return(character(0))
  ok <- !is.na(d$auc_test) &
        (is.null(d$note) | is.na(d$note) | !grepl("^FAILED", as.character(d$note)))
  same <- rep(TRUE, nrow(d))
  if (!is.null(d$n_dev))  same <- same & (d$n_dev  == length(idx_dev))
  if (!is.null(d$n_test)) same <- same & (d$n_test == length(idx_tst))
  same[is.na(same)] <- FALSE
  if (verbose && any(ok & !same))
    say("%d finished cell(s) ignored: fitted on a different sample (n_dev %s, now %s)",
        sum(ok & !same),
        paste(unique(d$n_dev[ok & !same]), collapse = "/"), length(idx_dev))
  unique(paste(d$family, d$arm, sep = "|")[ok & same])
}

# Feature count for a cell that is otherwise 'done'; NA when unknown (rows
# written before this column existed).
done_nfeat <- function(key) {
  d <- read_results()
  if (is.null(d) || is.null(d$n_feat)) return(NA_integer_)
  i <- which(paste(d$family, d$arm, sep = "|") == key & !is.na(d$auc_test))
  if (!length(i)) return(NA_integer_)
  as.integer(d$n_feat[max(i)])
}

failed_cells <- function() {
  d <- read_results()
  if (is.null(d) || !nrow(d)) return(character(0))
  bad <- is.na(d$auc_test) |
         (!is.na(d$note) & grepl("^FAILED", as.character(d$note)))
  setdiff(unique(paste(d$family, d$arm, sep = "|")[bad]), done_cells())
}

# =============================================================================
#  3. FEATURE BLOCKS
# =============================================================================

block_cols <- function(blocks) {
  unlist(lapply(blocks, function(b) switch(b,
    FIN = FIN_COLS, SP = SP_COLS, SPA = SPA_COLS, EO = EO_COLS,
    stop("unknown block ", b))), use.names = FALSE)
}

# corrected tensor build: channel-major columns -> (N, months, channels) C-order
COL_MAP <- matrix(seq_along(EO_COLS), nrow = N_MO, ncol = N_CH)   # [month, channel]
SEQ_ORDER <- as.vector(t(COL_MAP))                                # month-major

make_tensor <- function(rows) {
  x <- as.matrix(dt[rows, ..EO_COLS])
  x <- x[, SEQ_ORDER, drop = FALSE]
  a <- reticulate::array_reshape(x, c(length(rows), N_MO, N_CH))
  # sample 1 must round-trip: a[1, month, channel] == original[1, COL_MAP[month, channel]]
  orig <- as.matrix(dt[rows[1], ..EO_COLS])
  stopifnot(all.equal(as.vector(a[1, , ]), as.vector(orig[1, COL_MAP])))
  a
}

# =============================================================================
#  4. MODEL FAMILIES
# =============================================================================

# ---------------------------------------------------------------- h2o -------
h2o_ready <- FALSE
h2o_start <- function() {
  if (h2o_ready) return(invisible())
  suppressPackageStartupMessages(library(h2o))
  h2o.init(max_mem_size = CFG$h2o_mem, nthreads = CFG$h2o_threads)
  if (!isTRUE(CFG$verbose)) h2o.no_progress()
  h2o_ready <<- TRUE
}

fit_h2o <- function(kind, cols) {
  h2o_start()
  mk <- function(rows, y) {
    d <- dt[rows, ..cols]
    set(d, j = ".y", value = factor(y))
    set(d, j = ".w", value = class_w(y))     # GLM has no balance_classes; both
    as.h2o(d)                                 # families use the same weights
  }
  h_fit <- mk(idx_fit, y_fit)
  h_val <- mk(idx_val, y_val)

  m <- if (kind == "LR") {
    hp <- CFG$hp$lr
    h2o.glm(x = cols, y = ".y", training_frame = h_fit, validation_frame = h_val,
            weights_column = ".w", family = "binomial",
            lambda_search = hp$lambda_search, nlambdas = hp$nlambdas,
            standardize = hp$standardize, seed = CFG$seed)
  } else {
    hp <- CFG$hp$rf
    h2o.randomForest(x = cols, y = ".y", training_frame = h_fit,
                     validation_frame = h_val, weights_column = ".w",
                     ntrees = hp$ntrees, max_depth = hp$max_depth,
                     min_rows = hp$min_rows, seed = CFG$seed,
                     stopping_rounds = hp$stopping_rounds,
                     stopping_metric = "AUC",
                     score_tree_interval = hp$score_tree_interval)
  }
  h2o.rm(h_fit); h2o.rm(h_val); gc()

  score <- function(rows, y) {
    d <- dt[rows, ..cols]; set(d, j = ".y", value = factor(y)); set(d, j = ".w", value = 1)
    h <- as.h2o(d); p <- as.data.frame(h2o.predict(m, h))[["p1"]]
    h2o.rm(h); gc(); p
  }
  out <- list(dev = score(idx_dev, y_dev), tst = score(idx_tst, y_tst))
  h2o.rm(m); gc()
  out
}

# ---------------------------------------------------------------- xgboost ---
fit_xgb <- function(cols) {
  suppressPackageStartupMessages(library(xgboost))
  d_fit <- xgb.DMatrix(as.matrix(dt[idx_fit, ..cols]), label = y_fit)
  d_val <- xgb.DMatrix(as.matrix(dt[idx_val, ..cols]), label = y_val)
  # scale_pos_weight IS the ratio, so it takes ALPHA_RATIO unchanged; the
  # default below is the same N0/N1 the factorial used.
  spw <- if (is.na(ALPHA_RATIO)) sum(y_fit == 0) / max(1, sum(y_fit == 1)) else ALPHA_RATIO
  hp  <- CFG$hp$xgb
  prm <- list(objective = "binary:logistic", eval_metric = "auc",
              eta = hp$eta, max_depth = hp$max_depth, subsample = hp$subsample,
              colsample_bytree = hp$colsample_bytree,
              min_child_weight = hp$min_child_weight,
              tree_method = "hist", scale_pos_weight = spw,
              nthread = CFG$xgb_nthread)
  set.seed(CFG$seed)
  m <- xgb.train(prm, d_fit, nrounds = hp$nrounds,
                 watchlist = list(fit = d_fit, val = d_val),
                 early_stopping_rounds = hp$early_stopping, maximize = TRUE,
                 verbose = if (isTRUE(CFG$verbose)) 1 else 0,
                 print_every_n = 10)
  say("    xgb best_iteration = %d (val auc %.5f)", m$best_iteration,
      m$evaluation_log$val_auc[m$best_iteration])
  out <- list(dev = predict(m, xgb.DMatrix(as.matrix(dt[idx_dev, ..cols]))),
              tst = predict(m, xgb.DMatrix(as.matrix(dt[idx_tst, ..cols]))))
  rm(d_fit, d_val, m); gc()
  out
}

# ---------------------------------------------------------------- keras -----
keras_ready <- FALSE
keras_start <- function() {
  if (keras_ready) return(invisible())
  # ORDER MATTERS, and each step is here for a reason:
  #   1. bind the conda env  -- reticulate fixes its interpreter at the first
  #      Python call, so this must come first or it is silently ignored.
  #   2. library(keras)
  #   3. py_require_legacy_keras() -- routes the R API to tf_keras (Keras 2)
  #      instead of Keras 3. The two are not compatible: with Keras 3 the
  #      multi-input model and sample_weight calls in fit_stcraan() fail or
  #      behave differently. Must be after library(keras) and before the model
  #      is built.
  #   4. library(tensorflow)
  bind_python()
  suppressPackageStartupMessages(library(keras))
  if (isTRUE(CFG$keras_legacy)) {
    ok <- tryCatch({ keras::py_require_legacy_keras(); TRUE },
                   error = function(e) {
                     say("py_require_legacy_keras() unavailable: %s",
                         conditionMessage(e)); FALSE })
    if (ok) say("keras: using legacy tf_keras (Keras 2) as requested")
  }
  suppressPackageStartupMessages(library(tensorflow))
  gpus <- tf$config$list_physical_devices("GPU")
  say("keras: %d GPU(s) visible", length(gpus))
  for (g in gpus) try(tf$config$experimental$set_memory_growth(g, TRUE), silent = TRUE)
  tensorflow::set_random_seed(CFG$seed)
  keras_ready <<- TRUE
}

# chunked prediction; pushing the whole tensor at once caused
# "Dst tensor is not initialized" on this data size
predict_chunked <- function(model, inputs, n) {
  chunk <- CFG$predict_chunk
  outs <- vector("list", ceiling(n / chunk))
  k <- 1L
  for (s in seq(1L, n, by = chunk)) {
    e <- min(s + chunk - 1L, n)
    sub <- lapply(inputs, function(z)
      if (length(dim(z)) == 3) z[s:e, , , drop = FALSE] else z[s:e, , drop = FALSE])
    outs[[k]] <- as.numeric(predict(model, if (length(sub) == 1) sub[[1]] else sub,
                                    verbose = if (isTRUE(CFG$verbose)) 1 else 0))
    k <- k + 1L
  }
  unlist(outs, use.names = FALSE)
}

# keras multi-output models reject class_weight; sample_weight always works,
# and it is the same reweighting the other families get.

fit_mlp <- function(cols) {
  keras_start()
  hp <- CFG$hp$mlp
  stopifnot(length(hp$units) == length(hp$dropout))
  x_fit <- as.matrix(dt[idx_fit, ..cols]); x_val <- as.matrix(dt[idx_val, ..cols])
  model <- keras_model_sequential() %>%
    layer_dense(hp$units[1], activation = "relu", input_shape = ncol(x_fit))
  for (i in seq_along(hp$units)) {
    if (i > 1) model <- model %>% layer_dense(hp$units[i], activation = "relu")
    if (hp$dropout[i] > 0)
      model <- model %>% layer_batch_normalization() %>%
               layer_dropout(hp$dropout[i])
  }
  model <- model %>% layer_dense(1, activation = "sigmoid")
  model %>% compile(optimizer = optimizer_adam(hp$lr), loss = "binary_crossentropy",
                    metrics = list(metric_auc(name = "auc")))
  model %>% fit(x_fit, y_fit, sample_weight = class_w(y_fit),
                validation_data = list(x_val, y_val, class_w(y_val)),
                epochs = CFG$keras_epochs, batch_size = CFG$keras_batch,
                verbose = if (isTRUE(CFG$verbose)) 1 else 2,
                callbacks = list(callback_early_stopping(
                  monitor = "val_auc", mode = "max",
                  patience = CFG$keras_patience, restore_best_weights = TRUE)))
  x_dev <- as.matrix(dt[idx_dev, ..cols]); x_tst <- as.matrix(dt[idx_tst, ..cols])
  out <- list(dev = predict_chunked(model, list(x_dev), nrow(x_dev)),
              tst = predict_chunked(model, list(x_tst), nrow(x_tst)))
  rm(x_fit, x_val, x_dev, x_tst); k_clear_session(); gc()
  out
}

# ST-CRAAN: static branch + recurrent EO branch.
# The L2 penalty is applied to the recurrent kernel and the bias as well as the
# input kernel. Regularising the input kernel ALONE let it decay to ~1e-7 of its
# Glorot scale while the recurrent kernel stayed healthy: the hidden state went
# constant across borrowers and the branch was silently dead.
# ST-CRAAN on every arm, including the ones with no EO.
#   use_eo = TRUE   static branch (if any) + GRU/attention branch  -- the model
#   use_eo = FALSE  static branch only, same head, same optimiser, same early
#                   stopping. Architecturally that is a dense net, and it is
#                   deliberately NOT the MLP family: it keeps ST-CRAAN's own
#                   hp (static_units, head_units, dropout, lr) so the column is
#                   internally comparable. That is the whole point -- it makes
#                   F -> F+EO measurable INSIDE ST-CRAAN, which is the cleanest
#                   statement of what EO adds to this architecture, with no
#                   cross-family representation difference to argue about.
fit_stcraan <- function(static_cols, use_eo = TRUE) {
  keras_start()
  has_static <- length(static_cols) > 0
  if (!has_static && !use_eo)
    stop("ST-CRAAN was given neither static columns nor EO -- nothing to fit.")
  s_fit <- if (has_static) as.matrix(dt[idx_fit, ..static_cols]) else NULL
  s_val <- if (has_static) as.matrix(dt[idx_val, ..static_cols]) else NULL
  t_fit <- if (use_eo) make_tensor(idx_fit) else NULL
  t_val <- if (use_eo) make_tensor(idx_val) else NULL

  hp  <- CFG$hp$stcraan
  stopifnot(length(hp$static_units) == length(hp$static_drop))
  reg <- regularizer_l2(hp$l2)
  in_seq <- NULL; seq_vec <- NULL
  if (use_eo) {
    # bidirectional doubles the feature width, and the attention repeat_vector
    # MUST match it. Hard-coding 128 here silently breaks the moment gru_units
    # changes, so derive it.
    n_feat <- 2L * as.integer(hp$gru_units)
    in_seq <- layer_input(shape = c(N_MO, N_CH), name = "seq")
    h <- in_seq %>%
      bidirectional(layer_gru(units = hp$gru_units, return_sequences = TRUE,
                              kernel_regularizer = reg,
                              recurrent_regularizer = reg,
                              bias_regularizer = reg)) %>%
      layer_layer_normalization()
    att <- h %>% layer_dense(1, activation = "tanh") %>%
      layer_flatten() %>% layer_activation("softmax") %>%
      layer_repeat_vector(n_feat) %>% layer_permute(c(2, 1))
    seq_vec <- layer_multiply(list(h, att)) %>%
      layer_lambda(function(z) tensorflow::tf$reduce_sum(z, axis = 1L))
  }

  if (has_static) {
    in_stat <- layer_input(shape = ncol(s_fit), name = "static")
    stat_vec <- in_stat
    for (i in seq_along(hp$static_units)) {
      stat_vec <- stat_vec %>% layer_dense(hp$static_units[i], activation = "relu")
      if (hp$static_drop[i] > 0)
        stat_vec <- stat_vec %>% layer_batch_normalization() %>%
                    layer_dropout(hp$static_drop[i])
    }
    fused  <- if (use_eo) layer_concatenate(list(stat_vec, seq_vec)) else stat_vec
    inputs <- if (use_eo) list(in_stat, in_seq) else list(in_stat)
  } else {
    fused  <- seq_vec
    inputs <- list(in_seq)
  }
  out <- fused %>%
    layer_dense(hp$head_units, activation = "relu") %>%
    layer_dropout(hp$head_drop) %>%
    layer_dense(1, activation = "sigmoid", name = "main")
  model <- keras_model(inputs = inputs, outputs = out)
  model %>% compile(optimizer = optimizer_adam(hp$lr), loss = "binary_crossentropy",
                    metrics = list(metric_auc(name = "auc")))

  # keras takes a bare array for a single-input model and a list for a
  # multi-input one. With use_eo = FALSE and a static branch there is exactly
  # one input again, so the unwrapping rule is "one input -> bare array",
  # not "has_static -> list".
  # packl() ALWAYS returns a list; pack() unwraps a single input to a bare
  # array. predict_chunked() does its own unwrapping and needs the list -- give
  # it a bare matrix and lapply() iterates the columns instead of the inputs.
  packl <- function(s, t) c(if (has_static) list(s), if (use_eo) list(t))
  pack  <- function(s, t) { z <- packl(s, t); if (length(z) == 1L) z[[1]] else z }
  x_fit <- pack(s_fit, t_fit)
  x_val <- pack(s_val, t_val)
  model %>% fit(x_fit, y_fit,
                sample_weight = class_w(y_fit),
                validation_data = list(x_val, y_val, class_w(y_val)),
                epochs = CFG$keras_epochs, batch_size = CFG$keras_batch,
                verbose = if (isTRUE(CFG$verbose)) 1 else 2,
                callbacks = list(callback_early_stopping(
                  monitor = "val_auc", mode = "max",
                  patience = CFG$keras_patience, restore_best_weights = TRUE)))

  # branch health, so a dead branch is never mistaken for "EO does not help"
  health <- if (!use_eo) "no-EO arm: static branch only" else ""
  if (use_eo) {
    gru <- NULL
    for (l in model$layers) if (grepl("bidirectional", l$name)) gru <- l
    if (!is.null(gru)) {
      ws <- lapply(gru$get_weights(), function(z) sqrt(sum(as.numeric(z)^2)))
      health <- sprintf("kernelL2=%.3g recurL2=%.3g", ws[[1]], ws[[2]])
    }
  }
  rm(s_fit, s_val, t_fit, t_val, x_fit, x_val); gc()
  s_dev <- if (has_static) as.matrix(dt[idx_dev, ..static_cols]) else NULL
  s_tst <- if (has_static) as.matrix(dt[idx_tst, ..static_cols]) else NULL
  t_dev <- if (use_eo) make_tensor(idx_dev) else NULL
  t_tst <- if (use_eo) make_tensor(idx_tst) else NULL
  n_dev_rows <- length(idx_dev); n_tst_rows <- length(idx_tst)
  res <- list(dev = predict_chunked(model, packl(s_dev, t_dev), n_dev_rows),
              tst = predict_chunked(model, packl(s_tst, t_tst), n_tst_rows),
              note = health)
  rm(s_dev, s_tst, t_dev, t_tst, x_dev, x_tst); k_clear_session(); gc()
  res
}

# =============================================================================
#  5. RUN THE GRID
# =============================================================================

REDO <- any(tolower(args_in) %in% c("redo", "--redo", "-r"))
merge_shards()        # fold in anything a previous locked run had to park
done <- if (REDO) character(0) else done_cells(verbose = TRUE)
local({
  d <- read_results()
  say("results file: %d row(s), %d usable (a real test AUC), %d finished cell(s)",
      if (is.null(d)) 0L else nrow(d),
      if (is.null(d)) 0L else sum(!is.na(d$auc_test)), length(done))
})
retry <- failed_cells()
say("resuming: %d cell(s) already finished%s", length(done),
    if (REDO) "  (--redo: ignoring the cache, everything reruns)" else "")
if (length(retry) && !REDO)
  say("retrying %d previously FAILED cell(s): %s", length(retry),
      paste(head(retry, 8), collapse = ", "))

# -----------------------------------------------------------------------------
#  Pause after each finished cell: Y continue, N stop, A run the rest unattended
# -----------------------------------------------------------------------------
AUTO <- !isTRUE(CFG$pause_between_runs) ||
        any(tolower(args_in) %in% c("auto", "-y", "--auto"))

# readline() returns "" immediately under Rscript, so read the real stdin --
# and open that connection ONCE. Opening and closing file("stdin") per prompt
# discards everything still buffered, so the second prompt sees EOF and the
# script silently flips to auto after one answer.
STDIN_CON <- NULL
read_one <- function() {
  if (interactive()) return(trimws(readline()))
  if (is.null(STDIN_CON))
    STDIN_CON <<- tryCatch(file("stdin", open = "r"), error = function(e) NULL)
  if (is.null(STDIN_CON)) return(NA_character_)
  z <- tryCatch(readLines(STDIN_CON, n = 1L, warn = FALSE),
                error = function(e) character(0))
  if (!length(z)) NA_character_ else trimws(z)
}

ask_continue <- function(key, secs, auc_tst, remaining) {
  if (AUTO) return(TRUE)
  cat(sprintf("\n  ---- %s done in %.1f min, test AUC %s. %d cell(s) left ----\n",
              key, secs / 60,
              if (is.na(auc_tst)) "NA" else sprintf("%.5f", auc_tst), remaining))
  # everything finished so far, not just this cell
  r <- read_results()
  if (!is.null(r) && nrow(r) && isTRUE(ensure_report_helpers())) {
    r <- r[!duplicated(paste(family, arm), fromLast = TRUE)]
    print(auc_matrix(r), row.names = FALSE)
    auc_matrix_legend()
  }
  cat("  (full report any time, in another terminal:",
      "Rscript 12_Factorial7_report.R )\n")
  repeat {
    cat("  continue?  [Y] next   [N] stop here   [A] all remaining, no more asking : ")
    flush.console()
    ans <- read_one()
    if (is.na(ans)) {                    # no terminal attached (nohup, pipe, cron)
      cat("\n  no console attached -- switching to auto.\n")
      AUTO <<- TRUE; return(TRUE)
    }
    ans <- toupper(ans)
    if (ans %in% c("", "Y")) return(TRUE)
    if (ans == "A") { AUTO <<- TRUE; cat("  auto: running the rest.\n"); return(TRUE) }
    if (ans == "N") return(FALSE)
    cat("  please answer Y, N or A.\n")
  }
}

stopped <- FALSE
total_cells <- sum(vapply(sel_families, function(f) {
  a <- names(CFG$arms)
  if (f == "STCRAAN") a <- intersect(a, CFG$stcraan_arms)
  if (!is.null(SEL_ARMS)) a <- intersect(a, SEL_ARMS)
  sum(!paste(f, a, sep = "|") %in% done)
}, integer(1)))
say("%d cell(s) to run this session%s", total_cells,
    if (AUTO) " (auto: no prompts)" else " (will pause after each)")
cells_done_now <- 0L

for (fam in sel_families) {
  if (stopped) break
  # Re-read what is finished at the top of every family. The results file is
  # small and this costs milliseconds, but it means re-running just this loop
  # in an interactive session (RStudio, source of a selection) still honours
  # what is already on disk instead of starting the grid again from scratch.
  done <- if (REDO) character(0) else done_cells()
  arms <- names(CFG$arms)
  if (fam == "STCRAAN") arms <- intersect(arms, CFG$stcraan_arms)
  if (!is.null(SEL_ARMS)) arms <- intersect(arms, SEL_ARMS)

  for (arm in arms) {
    if (stopped) break
    key <- paste(fam, arm, sep = "|")
    if (key %in% done) {
      nf_old <- done_nfeat(key); nf_new <- length(block_cols(CFG$arms[[arm]]))
      if (!is.na(nf_old) && nf_old != nf_new) {
        say("rerun %s: it was fitted on %d features, the config now gives %d",
            key, nf_old, nf_new)
      } else {
        say("skip %s (done)", key); next
      }
    }

    blocks <- CFG$arms[[arm]]
    cols   <- block_cols(blocks)
    .n_feat_now <<- length(cols)

    # Hard guard. Year/ID/ADM3/Y are dropped before training and must never
    # reach a model matrix: Y is the label, Year defines the dev/test split,
    # ADM3 is the unit the 0.662 ceiling is defined on, and ID is an arbitrary
    # code a tree would happily split on. Any one of them arriving here would
    # invalidate the arm silently rather than error.
    leaked <- intersect(cols, need)
    if (length(leaked))
      stop("key column(s) reached the feature set for ", fam, " ", arm, ": ",
           paste(leaked, collapse = ", "),
           "\nThese are dropped before training by design. Check CFG$sp_cols ",
           "and CFG$eo_prefix.")
    if (anyDuplicated(cols))
      stop("duplicate column(s) in arm ", arm, ": ",
           paste(unique(cols[duplicated(cols)]), collapse = ", "),
           "\nTwo blocks are claiming the same column; the block definitions ",
           "overlap and the arm is not what its name says.")
    # ST-CRAAN does not see EO as flat columns: they leave the static matrix and
    # enter as the (rows, months, channels) tensor. Reporting length(cols) for it
    # would overstate the flat input by the whole EO block.
    n_flat <- if (fam == "STCRAAN") length(setdiff(cols, EO_COLS)) else length(cols)
    say("running %-8s %-9s  (%d flat features: %s%s)", fam, arm, n_flat,
        paste(blocks, collapse = "+"),
        if (fam == "STCRAAN" && "EO" %in% blocks)
          sprintf(" + EO tensor %dx%d", N_MO, N_CH) else "")

    t0 <- Sys.time()
    fit <- try({
      if (fam %in% c("LR", "RF")) {
        fit_h2o(fam, cols)
      } else if (fam == "XGB") {
        fit_xgb(cols)
      } else if (fam == "MLP") {
        fit_mlp(cols)
      } else if (fam == "STCRAAN") {
        fit_stcraan(setdiff(cols, EO_COLS), use_eo = "EO" %in% blocks)
      }
    }, silent = TRUE)

    if (inherits(fit, "try-error")) {
      emsg <- gsub("[\r\n]+", " ", trimws(as.character(fit)))
      say("  FAILED %s: %s", key, emsg)
      # Write the reason into the results file. Recording only the word FAILED
      # loses the one thing needed to fix it the moment the console scrolls.
      cat(sprintf("[%s] %s | %s\n", format(Sys.time()), key, emsg),
          file = file.path(CFG$out_dir, "factorial7_errors.log"), append = TRUE)
      record(fam, arm, NA_real_, NA_real_,
             data.table(alert_rate = CFG$alert_rates,
                        recall = NA_real_, fn = NA_real_),
             as.numeric(difftime(Sys.time(), t0, units = "secs")),
             note = paste("FAILED:", substr(emsg, 1, 300)))
      cells_done_now <- cells_done_now + 1L
      if (!ask_continue(paste(key, "(FAILED)"),
                        as.numeric(difftime(Sys.time(), t0, units = "secs")),
                        NA_real_, total_cells - cells_done_now)) {
        stopped <- TRUE; break
      }
      next
    }

    secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    auc_t <- fast_auc(fit$tst, y_tst, "test")
    record(fam, arm, fast_auc(fit$dev, y_dev, "dev"), auc_t,
           eval_at_alerts(fit$tst, y_tst, CFG$alert_rates),
           secs, note = if (!is.null(fit$note)) fit$note else "")

    # Keep this cell's test scores. See the note beside keys_test.rds above.
    # The arm name is sanitised because it contains "+", which has no business
    # in a filename: F+SP+EO becomes F_SP_EO, and that is the form the contrast
    # table in auc_significance_tests.R expects.
    pred_file <- file.path(PRED_DIR,
                           sprintf("pred_%s__%s.rds", fam,
                                   gsub("[^A-Za-z0-9]", "_", arm)))
    saveRDS(as.numeric(fit$tst), pred_file)
    say("  scores -> %s", basename(pred_file))

    # The development scores go out too, for the Study 2 calibration grid: the
    # recalibration maps are fitted on the held-out development rows and then
    # applied to 2022, so both vectors have to survive the run.
    dev_file <- file.path(PRED_DIR,
                          sprintf("dev_%s__%s.rds", fam,
                                  gsub("[^A-Za-z0-9]", "_", arm)))
    saveRDS(as.numeric(fit$dev), dev_file)
    say("  dev scores -> %s", basename(dev_file))
    rm(fit); gc()

    cells_done_now <- cells_done_now + 1L
    if (!ask_continue(key, secs, auc_t, total_cells - cells_done_now)) {
      stopped <- TRUE; break
    }
  }
}

if (stopped) {
  cat("\n")
  say("stopped by request after %d cell(s). Everything finished is already in",
      cells_done_now)
  say("%s -- rerun the same command to pick up where this left off.", RES_FILE)
}

# =============================================================================
#  6. SUMMARY AND CONSISTENCY CHECKS
# =============================================================================
# One implementation, in 12_Factorial7_report.R, so the summary printed here and
# the one you get from a second terminal mid-run can never disagree.

.report_sourced <- TRUE
source("12_Factorial7_report.R")

say("done in %.1f min | results: %s",
    as.numeric(difftime(Sys.time(), t_start, units = "mins")), RES_FILE)
if (h2o_ready) try(h2o.shutdown(prompt = FALSE), silent = TRUE)

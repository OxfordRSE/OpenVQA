# Rscript utilities/audit-distribution-inputs.R [results-tree] [detail.csv]
# shortcut: assumes mixed beta scoring;
#   use runtime logging for other algorithms or bootstrap counts.
audit_distribution_inputs <- function(root) {
  if (!dir.exists(root)) stop("Results tree does not exist: ", root)
  files <- list.files(root, pattern = "_focal.csv$", recursive = TRUE, full.names = TRUE)
  rows <- list()
  for (file in files) {
    focal <- read.csv(file, stringsAsFactors = FALSE)
    required <- c("landcover", "stratum", "bm.vegetation", "dist", "q.method")
    if (!all(required %in% names(focal))) stop("Missing focal columns: ", file)
    focal <- focal[focal$dist %in% c("Bet", "gamma") & focal$q.method == "empirical", ]
    if (!nrow(focal)) next
    raw_file <- sub("_focal.csv$", "_raw.csv", file)
    raw <- read.csv(raw_file, stringsAsFactors = FALSE)
    required <- c("focalOrBenchmark", "landCover", "vegClass", "stratum", "EI")
    if (!all(required %in% names(raw))) stop("Missing raw columns: ", raw_file)
    directory <- dirname(file)
    for (i in seq_len(nrow(focal))) {
      result <- focal[i, ]
      f <- raw$EI[raw$focalOrBenchmark == "f" & raw$landCover == result$landcover &
        raw$stratum == result$stratum]
      b <- raw$EI[raw$focalOrBenchmark == "b" & raw$vegClass == result$bm.vegetation &
        raw$stratum == result$stratum]
      if (!length(f) || !length(b) || any(!is.finite(c(f, b)))) {
        stop("Missing or nonfinite sample: ", file, " / ", result$landcover, " / ", result$stratum)
      }
      if (result$dist == "Bet" && any(c(f, b) < 0 | c(f, b) > 1)) {
        stop("Beta sample outside [0,1]: ", file, " / ", result$landcover)
      }
      if (result$dist == "gamma" && any(c(f, b) < 0)) {
        stop("Gamma sample below zero: ", file, " / ", result$landcover)
      }
      # calculate_indicator_quality() skips fitting when both gamma samples are zero.
      if (result$dist == "gamma" && all(f == 0) && all(b == 0)) next
      for (role in c("focal", "benchmark")) {
        values <- if (role == "focal") f else b
        rows[[length(rows) + 1L]] <- data.frame(
          project = basename(dirname(dirname(directory))),
          assessment = basename(dirname(directory)), results_dir = directory,
          indicator = sub("_focal.csv$", "", basename(file)),
          landcover = result$landcover, stratum = result$stratum,
          role = role,
          sample_group = if (role == "focal") result$landcover else result$bm.vegetation,
          distribution = result$dist, n = length(values),
          zeros = sum(values == 0), ones = sum(values == 1),
          distinct_values = length(unique(values)),
          beta_boundary = result$dist == "Bet" && any(values == 0 | values == 1),
          beta_constant = result$dist == "Bet" && length(unique(values)) == 1L,
          gamma_zero = result$dist == "gamma" && any(values == 0)
        )
      }
    }
  }
  if (!length(rows)) stop("No eligible empirical beta/gamma samples found under: ", root)
  audit <- do.call(rbind, rows)
  audit$adjusted <- audit$beta_boundary | audit$beta_constant | audit$gamma_zero
  audit
}

summarise_distribution_audit <- function(audit) {
  keys <- c("results_dir", "indicator", "role", "sample_group", "stratum")
  samples <- audit[!duplicated(audit[keys]), ]
  flags <- c("beta_boundary", "beta_constant", "gamma_zero", "adjusted")
  outputs <- aggregate(
    audit[flags],
    audit[c("results_dir", "indicator", "landcover", "stratum", "distribution")], any
  )
  summary <- function(data, unit) {
    do.call(rbind, lapply(split(data, data$distribution), function(group) {
      data.frame(
        unit = unit, distribution = group$distribution[1], total = nrow(group),
        as.list(colSums(group[flags])), percent_adjusted = round(100 * mean(group$adjusted), 1)
      )
    }))
  }
  rbind(
    summary(samples, "unique input datasets"),
    summary(outputs, "indicator-quality outputs")
  )
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 2L) stop("Usage: audit-distribution-inputs.R [results-tree] [detail.csv]")
  root <- if (length(args)) args[1] else "tests/refactor/fixtures"
  audit <- audit_distribution_inputs(root)
  if (length(args) == 2L) {
    if (file.exists(args[2])) stop("Refusing to overwrite: ", args[2])
    write.csv(audit, args[2], row.names = FALSE)
  }
  cat(
    "Assumes mixed beta scoring",
    "Original samples only; fixed scoring and both-zero gamma bypass excluded.\n"
  )
  cat(
    "Shared benchmarks count once per assessment/indicator/vegetation/",
    "stratum in unique dataset totals.\n"
  )
  print(summarise_distribution_audit(audit), row.names = FALSE)
}

#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
})

source(file.path("pipeline", "io.R"))
source(file.path("preprocessing", "helpers.R"))
source(file.path("preprocessing", "prune.R"))
source(file.path("preprocessing", "allowlists.R"))
source(file.path("preprocessing", "standardise.R"))
source(file.path("preprocessing", "export.R"))
source(file.path("preprocessing", "land-cover-summaries.R"))
source(file.path("preprocessing", "workflow.R"))
source(file.path("analysis", "legacy.R"))

parse_cli_args <- function() {
  parser <- OptionParser(
    usage = "usage: %prog --data-root <path> --assessment <name>",
    option_list = list(
      make_option(c("-d", "--data-root"), type = "character"),
      make_option(c("-a", "--assessment"), type = "character")
    ),
    description = "Prepare validated inputs and run the VQA analysis."
  )
  options <- parse_args(parser)
  if (is.null(options$`data-root`) || !nzchar(options$`data-root`) ||
    is.null(options$assessment) || !nzchar(options$assessment)) {
    stop("Both --data-root and --assessment are required.", call. = FALSE)
  }
  list(
    data_root = normalizePath(options$`data-root`, mustWork = TRUE),
    assessment = options$assessment
  )
}

run_analysis <- function(data_root, assessment, seed = Sys.getenv("VQA_TEST_SEED", "")) {
  context <- load_preprocessing_context(data_root, assessment)
  context <- prepare_analysis_inputs(context)

  seed <- if (nzchar(seed)) as.integer(seed) else NULL
  if (!is.null(seed) && is.na(seed)) {
    stop("VQA_TEST_SEED must be an integer.", call. = FALSE)
  }
  run_legacy_batch(data_root, legacy_project_name(context), assessment, seed)
  invisible(context)
}

main <- function() {
  cli <- parse_cli_args()
  run_analysis(cli$data_root, cli$assessment)
}

main()

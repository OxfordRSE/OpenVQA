#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
})

source(file.path("pipeline", "io.R"))
source(file.path("pipeline", "params.R"))
source(file.path("preprocessing", "helpers.R"))
source(file.path("preprocessing", "prune.R"))
source(file.path("preprocessing", "allowlists.R"))
source(file.path("preprocessing", "standardise.R"))
source(file.path("preprocessing", "export.R"))
source(file.path("preprocessing", "land-cover-summaries.R"))
source(file.path("preprocessing", "workflow.R"))
source(file.path("analysis", "legacy.R"))

parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  parser <- OptionParser(
    usage = "usage: %prog --data-root <path> --assessment <name> [--seed <integer>]",
    option_list = list(
      make_option(c("-d", "--data-root"), type = "character"),
      make_option(c("-a", "--assessment"), type = "character"),
      make_option("--seed", type = "character", help = "Randomization seed (integer).")
    ),
    description = "Prepare validated inputs and run the VQA analysis."
  )
  options <- parse_args(parser, args = args)
  if (is.null(options$`data-root`) || !nzchar(options$`data-root`) ||
    is.null(options$assessment) || !nzchar(options$assessment)) {
    stop("Both --data-root and --assessment are required.", call. = FALSE)
  }
  list(
    data_root = normalizePath(options$`data-root`, mustWork = TRUE),
    assessment = options$assessment,
    seed = resolve_seed(options$seed)
  )
}

run_analysis <- function(data_root, assessment, seed = NULL) {
  context <- load_preprocessing_context(data_root, assessment, seed = seed)
  context <- prepare_analysis_inputs(context)

  run_vqa_analysis(
    context,
    config = context$config,
    seed = context$config$seed
  )
}

main <- function() {
  cli <- parse_cli_args()
  run_analysis(cli$data_root, cli$assessment, seed = cli$seed)
}

if (sys.nframe() == 0L) main()

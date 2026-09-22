#!/usr/bin/env Rscript

Sys.setenv(VQA_PREPROCESS_NO_AUTORUN = "1")
source(file.path("pipeline", "io.R"))
source(file.path("preprocessing", "helpers.R"))
source(file.path("preprocessing", "prune.R"))
source(file.path("preprocessing", "allowlists.R"))
source(file.path("preprocessing", "standardise.R"))
source(file.path("preprocessing", "export.R"))
source(file.path("preprocessing", "land-cover-summaries.R"))
source(file.path("preprocessing", "workflow.R"))

testthat::test_dir("tests/testthat", reporter = "progress")

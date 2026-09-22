#!/usr/bin/env Rscript

project_library <- file.path(
  "rv", "library", paste(
    R.version$major, sub("\\..*$", "", R.version$minor),
    sep = "."
  ),
  Sys.info()[["machine"]]
)

testthat::test_dir("tests/refactor/testthat", reporter = "progress")

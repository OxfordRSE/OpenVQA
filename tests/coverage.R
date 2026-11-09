#!/usr/bin/env Rscript

# Keep source references for dependencies imported by the entry points, so covr can
# instrument their functions before the in-process standard suite executes.
coverage <- withr::with_options(list(keep.source = TRUE), {
  covr::file_coverage(c("analyse.R", "validate.R"), "tests/coverage-tests.R")
})
stopifnot(covr::percent_coverage(coverage) > 0)
covr::to_cobertura(coverage, filename = "coverage.xml")
print(coverage)

testthat::test_that("CLI reports the version without requiring an assessment", {
  old_directory <- getwd()
  on.exit(setwd(old_directory))
  setwd(file.path("..", ".."))
  source("analyse.R", local = TRUE)
  expected <- readLines("VERSION", warn = FALSE)
  testthat::expect_identical(parse_cli_args("--version"), list(version = expected))
  output <- system2(file.path(R.home("bin"), "Rscript"),
    c("analyse.R", "--version"),
    stdout = TRUE
  )
  testthat::expect_identical(output, paste("OpenVQA", expected))
})

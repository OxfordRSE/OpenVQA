testthat::test_that("version reporting works without an assessment or the project directory", {
  old_directory <- getwd()
  on.exit(setwd(old_directory))
  setwd(file.path("..", ".."))
  source("analyse.R", local = TRUE)
  expected <- readLines("VERSION", warn = FALSE)
  testthat::expect_length(expected, 1L)
  testthat::expect_match(expected, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  testthat::expect_identical(parse_cli_args("--version"), list(version = expected))
  output <- system2(file.path(R.home("bin"), "Rscript"),
    c("analyse.R", "--version"),
    stdout = TRUE
  )
  testthat::expect_identical(output, paste("OpenVQA", expected))
  setwd(tempdir())
  testthat::expect_identical(openvqa_version(), expected)
})

report_repo <- normalizePath(file.path("..", ".."))
report_schema <- file.path(report_repo, "schema", "run-report.schema.json")

expect_report_schema <- function(path) {
  testthat::expect_true(jsonvalidate::json_validate(
    path, report_schema,
    engine = "ajv", strict = TRUE, error = TRUE
  ))
}

validator <- new.env()
withr::with_dir(report_repo, {
  sys.source("validate.R", envir = validator)
  sys.source("pipeline/run-report.R", envir = validator)
})

testthat::test_that("published examples agree with the report schema", {
  examples <- list.files(file.path(report_repo, "tests", "testthat", "fixtures", "run-reports"),
    full.names = TRUE
  )
  testthat::expect_length(examples, 5)
  for (path in examples) expect_report_schema(path)
  report <- jsonlite::fromJSON(file.path(dirname(examples[1]), "error.json"),
    simplifyVector = FALSE
  )
  report$outcome <- "success"
  testthat::expect_false(jsonvalidate::json_validate(
    jsonlite::toJSON(report, auto_unbox = TRUE, null = "null"), report_schema,
    engine = "ajv"
  ))
})


testthat::test_that("shared report envelopes preserve caller state and tolerate missing metadata", {
  directory <- tempfile("report-archive-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  withr::local_seed(42)
  seed <- .Random.seed
  first <- withr::with_dir(directory, validator$new_run_report("validate", list()))
  second <- withr::with_dir(directory, validator$new_run_report("validate", list()))
  testthat::expect_null(first$backend$version)
  testthat::expect_null(first$backend$revision)
  testthat::expect_identical(.Random.seed, seed)
  testthat::expect_false(identical(first$run_id, second$run_id))
  testthat::expect_match(first$started_at, "Z$")
  for (outcome in c("success", "invalid_data", "execution_error")) {
    testthat::expect_identical(
      validator$run_report_exit_status(list(outcome = outcome)),
      match(outcome, c("success", "invalid_data", "execution_error")) - 1L
    )
  }
  testthat::expect_error(validator$run_report_exit_status(list(outcome = "unknown")), "Unknown")
})

testthat::test_that("shared publication preserves JSON values and protects destination files", {
  directory <- tempfile("report-publication-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  report <- jsonlite::fromJSON(file.path(
    report_repo, "tests", "testthat", "fixtures",
    "run-reports", "success.json"
  ), simplifyVector = FALSE)
  text <- "café \"quoted\" \\ path\nnext line"
  report$validation$assessments[[1]]$validation$rows[[1]]$message <- text
  destination <- file.path(directory, "résultat.json")
  validator$write_run_report(report, destination)
  expect_report_schema(destination)
  loaded <- jsonlite::fromJSON(destination, simplifyVector = FALSE)
  testthat::expect_identical(loaded$validation$assessments[[1]]$validation$rows[[1]]$message, text)
  testthat::expect_null(loaded$error)
  testthat::expect_null(loaded$validation$assessments[[1]]$validation$rows[[1]]$details)
  testthat::expect_length(loaded$scope$assessments, 1)
  testthat::expect_error(validator$write_run_report(report, destination), "already exists")
  testthat::expect_error(validator$write_run_report(report, directory), "new file")
  testthat::expect_error(validator$write_run_report(report, ""), "new file")
  testthat::expect_error(validator$write_run_report(
    report,
    file.path(directory, "missing", "report.json")
  ))
  if (.Platform$OS.type != "windows") {
    link <- file.path(directory, "dangling.json")
    testthat::expect_true(file.symlink(file.path(directory, "absent"), link))
    testthat::expect_error(validator$write_run_report(report, link), "already exists")
  }
  rlang::local_bindings(file.link = function(from, to) {
    writeLines("concurrent writer", to)
    base::file.link(from, to)
  }, .env = validator)
  destination <- file.path(directory, "race.json")
  testthat::expect_error(validator$write_run_report(report, destination), "Could not publish")
  testthat::expect_identical(readLines(destination), "concurrent writer")
  testthat::expect_false(any(grepl(".openvqa-report-", list.files(directory, all.files = TRUE),
    fixed = TRUE
  )))
})

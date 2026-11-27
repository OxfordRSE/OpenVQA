report_repo <- normalizePath(file.path("..", ".."))
report_schema <- file.path(report_repo, "schema", "run-report.schema.json")

expect_report_schema <- function(path) {
  testthat::expect_true(jsonvalidate::json_validate(
    path, report_schema,
    engine = "ajv", strict = TRUE, error = TRUE
  ))
}

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

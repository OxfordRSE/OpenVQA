report_repo <- normalizePath(file.path("..", ".."))
validator <- new.env()
withr::with_dir(report_repo, sys.source("validate.R", envir = validator))
report_schema <- file.path(report_repo, "schema", "run-report.schema.json")

validation_project <- function(method = "assume.0") {
  root <- tempfile("validation project café-")
  dir.create(file.path(root, "assessment", "inputs"), recursive = TRUE)
  yaml::write_yaml(
    list(project = "test", qh_method = method, active_indicators = "SR"),
    file.path(root, "params.yaml")
  )
  land_cover <- validator$read_csv_table(file.path(
    report_repo, "data", "vqa-demo1-min", "main_001_current", "inputs", "landCover.csv"
  ))[1, ]
  write.csv(land_cover, file.path(root, "assessment", "inputs", "landCover.csv"), row.names = FALSE)
  root
}

validation_result <- function(...) {
  result <- NULL
  capture.output(result <- withr::with_dir(report_repo, validator$run_validation(...)))
  result
}

validation_command <- function(args) {
  output <- suppressWarnings(withr::with_dir(report_repo, system2(
    file.path(R.home("bin"), "Rscript"), shQuote(c("validate.R", args)),
    stdout = TRUE, stderr = TRUE
  )))
  status <- attr(output, "status")
  list(status = if (is.null(status)) 0L else status, output = output)
}

expect_report_schema <- function(path) {
  testthat::expect_true(jsonvalidate::json_validate(
    path, report_schema,
    engine = "ajv", strict = TRUE, error = TRUE
  ))
}

assessment_rows <- function(result) {
  validator$validation_report_rows(result$validation$assessments[[1]])
}

assessment_stage_statuses <- function(result) {
  vapply(result$validation$assessments[[1]]$validation$stages, `[[`, character(1), "status")
}

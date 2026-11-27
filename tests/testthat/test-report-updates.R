update_project <- function() {
  root <- validation_project()
  dir.create(file.path(root, "second", "inputs"), recursive = TRUE)
  stopifnot(file.copy(
    file.path(root, "assessment", "inputs", "landCover.csv"),
    file.path(root, "second", "inputs")
  ))
  root
}

read_report <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)

write_project_report <- function(root) {
  path <- file.path(root, "report.json")
  validator$write_run_report(validation_result(root), path)
  path
}

testthat::test_that("params hashes use exact bytes and detect mid-run changes", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  before <- validator$snapshot_project_settings(root)
  cat("\n# formatting change\n", file = file.path(root, "params.yaml"), append = TRUE)
  after <- validator$snapshot_project_settings(root)
  testthat::expect_false(identical(before$hash, after$hash))
  testthat::expect_identical(after$hash, unname(tools::md5sum(file.path(root, "params.yaml"))))
  original <- validator$run_schema_validation
  rlang::local_bindings(run_schema_validation = function(...) {
    report <- original(...)
    cat("\n# mid-run change\n", file = file.path(root, "params.yaml"), append = TRUE)
    report
  }, .env = validator)
  result <- validation_result(root)
  child <- result$validation$assessments[[1]]
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_identical(child$error$stage, "settings_snapshot")
  testthat::expect_false(child$validation$settings$stable)
})

testthat::test_that("version-2.1 reports require valid settings evidence at publication", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  result <- validation_result(root)
  path <- file.path(root, "invalid.json")
  for (field in c("path", "algorithm", "hash", "captured_at", "stable")) {
    invalid <- result
    invalid$validation$assessments[[1]]$validation$settings[[field]] <- NULL
    testthat::expect_error(validator$write_run_report(invalid, path), class = "validation_error")
  }
  invalid <- result
  invalid$validation$assessments[[1]]$validation$settings$hash <- "invalid"
  testthat::expect_error(validator$write_run_report(invalid, path), class = "validation_error")
  invalid <- result
  invalid$settings <- NULL
  testthat::expect_error(validator$write_run_report(invalid, path), class = "validation_error")
  testthat::expect_false(file.exists(path))
})

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

testthat::test_that("report modes require explicit destinations and assessment selection", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- write_project_report(root)
  previous <- readBin(path, "raw", n = file.info(path)$size)
  for (args in list(
    c("--report", path, "--update-report"),
    c("--assessment", "assessment", "--update-report"),
    "--overwrite",
    c("--report", path, "--assessment", "assessment", "--overwrite", "--update-report")
  )) {
    testthat::expect_identical(validation_command(c("--data-root", root, args))$status, 2L)
    testthat::expect_identical(readBin(path, "raw", n = file.info(path)$size), previous)
  }
  missing <- file.path(root, "missing.json")
  testthat::expect_identical(validation_command(c(
    "--data-root", root, "--assessment", "assessment",
    "--report", missing, "--update-report"
  ))$status, 2L)
  testthat::expect_false(file.exists(missing))
})

testthat::test_that("assessment updates retain other outcomes, timestamps, identities and hashes", {
  root <- update_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- write_project_report(root)
  before <- read_report(path)
  yaml::write_yaml(list(
    project = "test", qh_method = "assume.0", active_indicators = "SR",
    min_plots = 7L
  ), file.path(root, "params.yaml"))
  command <- validation_command(c(
    "--data-root", root, "--assessment", "assessment",
    "--report", path, "--update-report"
  ))
  testthat::expect_identical(command$status, 0L, info = paste(command$output, collapse = "\n"))
  stored <- read_report(path)
  expect_report_schema(path)
  testthat::expect_identical(stored$report_type, "collection")
  testthat::expect_false(any(c("outcome", "run_id", "started_at", "ended_at") %in% names(stored)))
  testthat::expect_match(stored$updated_at, "Z$")
  testthat::expect_identical(stored$validation$assessments[[2]], before$validation$assessments[[2]])
  latest <- stored$validation$assessments[[1]]
  testthat::expect_false(identical(latest$run_id, before$validation$assessments[[1]]$run_id))
  testthat::expect_identical(latest$outcome, "success")
  testthat::expect_identical(
    latest$validation$settings$hash,
    unname(tools::md5sum(file.path(root, "params.yaml")))
  )
  testthat::expect_false(stored$settings$hashes_match)
  testthat::expect_match(stored$settings$warning, "different")
  testthat::expect_true(any(grepl("Report warning", command$output, fixed = TRUE)))

  command <- validation_command(c(
    "--data-root", root, "--assessment", "second",
    "--report", path, "--update-report"
  ))
  testthat::expect_identical(command$status, 0L)
  current <- read_report(path)
  testthat::expect_true(current$settings$hashes_match)
  testthat::expect_null(current$settings$warning)
  testthat::expect_identical(current$validation$assessments[[1]], latest)
  expect_report_schema(path)
})

testthat::test_that("updates append assessments and preserve retained outcomes", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- write_project_report(root)
  original <- read_report(path)$validation$assessments[[1]]
  dir.create(file.path(root, "second", "inputs"), recursive = TRUE)
  command <- validation_command(c(
    "--data-root", root, "--assessment", "second",
    "--report", path, "--update-report"
  ))
  testthat::expect_identical(command$status, 1L)
  stored <- read_report(path)
  testthat::expect_length(stored$validation$assessments, 2)
  testthat::expect_identical(stored$validation$assessments[[1]], original)
  testthat::expect_identical(stored$validation$assessments[[2]]$outcome, "invalid_data")
  writeLines("invalid: [", file.path(root, "params.yaml"))
  testthat::expect_identical(validation_command(c(
    "--data-root", root, "--assessment", "second",
    "--report", path, "--update-report"
  ))$status, 2L)
  stored <- read_report(path)
  testthat::expect_identical(stored$validation$assessments[[1]], original)
  testthat::expect_identical(stored$validation$assessments[[2]]$outcome, "execution_error")
  testthat::expect_match(stored$validation$assessments[[2]]$error$stage, "configuration")
  testthat::expect_false(stored$settings$hashes_match)
  expect_report_schema(path)
})

testthat::test_that("overwrite replaces the complete report and never silently merges", {
  root <- update_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- write_project_report(root)
  before <- read_report(path)
  testthat::expect_identical(validation_command(c(
    "--data-root", root,
    "--report", path
  ))$status, 2L)
  testthat::expect_identical(read_report(path), before)
  testthat::expect_identical(validation_command(c(
    "--data-root", root, "--assessment", "assessment",
    "--report", path, "--overwrite"
  ))$status, 0L)
  stored <- read_report(path)
  testthat::expect_identical(stored$report_type, "run")
  testthat::expect_length(stored$validation$assessments, 1)
  testthat::expect_identical(validation_command(c(
    "--data-root", root, "--report", path,
    "--overwrite"
  ))$status, 0L)
  testthat::expect_length(read_report(path)$validation$assessments, 2)
  expect_report_schema(path)
})

testthat::test_that("failed replacement preserves bytes and cleans up owned files", {
  root <- update_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- write_project_report(root)
  result <- validation_result(root, assessment = "assessment")
  before <- readBin(path, "raw", n = file.info(path)$size)
  files <- list.files(root, all.files = TRUE)
  rlang::local_bindings(file.rename = function(...) FALSE, .env = validator)
  testthat::expect_error(validator$update_assessment_report(result, path), "Could not publish")
  testthat::expect_error(
    validator$write_run_report(result, path, overwrite = TRUE), "Could not publish"
  )
  testthat::expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
  testthat::expect_identical(list.files(root, all.files = TRUE), files)
  writeLines("another writer", paste0(path, ".lock"))
  testthat::expect_error(validator$update_assessment_report(result, path), "lock")
  testthat::expect_identical(readLines(paste0(path, ".lock")), "another writer")
  testthat::expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
})

testthat::test_that("updates reject foreign, incompatible and malformed reports", {
  root <- update_project()
  other <- validation_project()
  on.exit(unlink(c(root, other), recursive = TRUE), add = TRUE)
  path <- write_project_report(root)
  result <- validation_result(root, assessment = "assessment")
  foreign <- file.path(other, "report.json")
  validator$write_run_report(validation_result(other), foreign)
  testthat::expect_error(validator$update_assessment_report(result, foreign), "different project")
  malformed <- file.path(root, "malformed.json")
  writeLines("invalid JSON", malformed)
  testthat::expect_error(validator$update_assessment_report(result, malformed), "not an OpenVQA")
  testthat::expect_error(
    validator$write_run_report(result, malformed, overwrite = TRUE),
    "not an OpenVQA"
  )
  testthat::expect_identical(readLines(malformed), "invalid JSON")
  report <- read_report(path)
  report$report_version <- "1.0"
  jsonlite::write_json(report, path, auto_unbox = TRUE, null = "null")
  testthat::expect_error(
    validator$update_assessment_report(result, path),
    class = "validation_error"
  )
  # Explicit whole-report replacement can regenerate a recognised historical version.
  validator$write_run_report(validation_result(root), path, overwrite = TRUE)
  expect_report_schema(path)
  report <- read_report(path)
  report$validation$assessments[[2]] <- report$validation$assessments[[1]]
  jsonlite::write_json(report, path, auto_unbox = TRUE, null = "null")
  testthat::expect_error(validator$update_assessment_report(result, path), "duplicate")
})

testthat::test_that("older version-2 entries retain unknown settings identity", {
  root <- update_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- write_project_report(root)
  report <- read_report(path)
  report$report_version <- "2.0"
  report$settings <- NULL
  for (i in seq_along(report$validation$assessments)) {
    report$validation$assessments[[i]]$report_version <- "2.0"
    report$validation$assessments[[i]]$validation$settings <- NULL
  }
  jsonlite::write_json(report, path, auto_unbox = TRUE, null = "null")
  expect_report_schema(path)
  preserved <- report$validation$assessments[[2]]
  validator$update_assessment_report(validation_result(root, assessment = "assessment"), path)
  stored <- read_report(path)
  testthat::expect_identical(stored$validation$assessments[[2]], preserved)
  testthat::expect_null(stored$settings$hashes_match)
  testthat::expect_match(stored$settings$warning, "unavailable")
  expect_report_schema(path)
})

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

testthat::test_that("the locked merge reloads a later assessment update instead of losing it", {
  root <- update_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- write_project_report(root)
  first <- validation_result(root, assessment = "assessment")
  unlink(file.path(root, "second", "inputs", "landCover.csv"))
  second <- validation_result(root, assessment = "second")
  original <- validator$with_report_lock
  inject <- TRUE
  rlang::local_bindings(with_report_lock = function(path, code) {
    if (inject) {
      inject <<- FALSE
      validator$update_assessment_report(second, path)
    }
    original(path, code)
  }, .env = validator)
  validator$update_assessment_report(first, path)
  stored <- read_report(path)
  testthat::expect_identical(
    stored$validation$assessments[[1]]$run_id,
    first$validation$assessments[[1]]$run_id
  )
  testthat::expect_identical(
    stored$validation$assessments[[2]]$run_id,
    second$validation$assessments[[1]]$run_id
  )
  testthat::expect_identical(stored$validation$assessments[[2]]$outcome, "invalid_data")
  testthat::expect_false("outcome" %in% names(stored))
})

testthat::test_that("report replacement protects source CSVs and refuses external edits", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- write_project_report(root)
  result <- validation_result(root)
  source <- file.path(root, "assessment", "inputs", "landCover.csv")
  before <- tools::md5sum(source)
  testthat::expect_error(
    validator$write_run_report(result, source, overwrite = TRUE), "not an OpenVQA"
  )
  testthat::expect_identical(tools::md5sum(source), before)
  original <- base::writeLines
  rlang::local_bindings(writeLines = function(text, con, ...) {
    original(text, con, ...)
    if (grepl(".openvqa-report-", basename(con), fixed = TRUE) &&
      !grepl("lock-", basename(con), fixed = TRUE)) {
      original("external edit", path)
    }
  }, .env = validator)
  testthat::expect_error(validator$write_run_report(result, path, overwrite = TRUE), "changed")
  testthat::expect_identical(readLines(path), "external edit")
  testthat::expect_false(file.exists(paste0(path, ".lock")))
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

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

testthat::test_that("completed schema files survive a later exception", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  original <- validator$validate_schema_file
  rlang::local_bindings(validate_schema_file = function(file_name, ...) {
    if (file_name == "species.csv") stop("cannot read second CSV")
    original(file_name, ...)
  }, .env = validator)
  fragments <- list()
  testthat::expect_error(validator$run_schema_validation(
    "assessment", file.path(root, "assessment", "inputs"), c("landCover.csv", "species.csv"),
    file.path(report_repo, "schema"),
    on_result = function(row) {
      fragments[[length(fragments) + 1L]] <<- row
    }
  ), "second CSV")
  testthat::expect_length(fragments, 1)
  testthat::expect_identical(fragments[[1]]$status, "pass")
})


testthat::test_that("cross-file callbacks retain complete fragments before a later exception", {
  root <- tempfile("crossfile-fragments-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  demo <- file.path(report_repo, "data", "vqa-demo1-min")
  testthat::expect_true(all(file.copy(file.path(demo, c("params.yaml", "main_001_current")),
    root,
    recursive = TRUE
  )))
  map <- validator$build_file_map(file.path(root, "main_001_current", "inputs"))
  params <- withr::with_dir(report_repo, validator$read_project_params(root, "main_001_current"))
  fragments <- list()
  record <- function(fragment) fragments[[length(fragments) + 1L]] <<- fragment
  report <- validator$run_crossfile_validation(root, "main_001_current",
    params$active_indicators, map, params,
    on_result = record
  )
  testthat::expect_identical(report, validator$bind_validation_reports(fragments))
  testthat::expect_true(all(report$assessment == "main_001_current"))
  testthat::expect_true(any(report$severity == "warning" & report$status == "pass"))
  fragments <- list()
  rlang::local_bindings(
    validate_species_lookup = function(...) stop("cross-file exception"),
    .env = validator
  )
  testthat::expect_error(validator$run_crossfile_validation(root, "main_001_current",
    params$active_indicators, map, params,
    on_result = record
  ), "cross-file exception")
  testthat::expect_length(fragments, 2)
  testthat::expect_true(all(validator$bind_validation_reports(fragments)$assessment ==
    "main_001_current"))
})

withr::with_dir(report_repo, sys.source("validation/snapshot.R", envir = validator))

testthat::test_that("input manifests detect changed, added and removed CSV bytes", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  inputs <- file.path(root, "assessment", "inputs")
  file <- file.path(inputs, "landCover.csv")
  first <- validator$snapshot_validation_inputs(inputs)
  second <- validator$snapshot_validation_inputs(inputs)
  testthat::expect_identical(first$files, second$files)
  testthat::expect_identical(first$files$path, "inputs/landCover.csv")
  testthat::expect_identical(first$files$hash, unname(tools::md5sum(file)))
  Sys.setFileTime(file, Sys.time() + 10)
  touched <- validator$snapshot_validation_inputs(inputs)
  testthat::expect_identical(first$files$hash, touched$files$hash)
  table <- validator$read_csv_table(file)
  table$area_ha <- table$area_ha + 1
  write.csv(table, file, row.names = FALSE)
  changed <- validator$snapshot_validation_inputs(inputs)
  testthat::expect_false(identical(first$files$hash, changed$files$hash))
  writeLines("ignored output", file.path(inputs, "log.txt"))
  testthat::expect_identical(changed$files, validator$snapshot_validation_inputs(inputs)$files)
  write.csv(data.frame(x = 1), file.path(inputs, "additional.csv"), row.names = FALSE)
  added <- validator$snapshot_validation_inputs(inputs)
  testthat::expect_identical(added$files$path, c("inputs/additional.csv", "inputs/landCover.csv"))
  unlink(file.path(inputs, "additional.csv"))
  testthat::expect_identical(changed$files, validator$snapshot_validation_inputs(inputs)$files)
  unlink(file)
  testthat::expect_equal(nrow(validator$snapshot_validation_inputs(inputs)$files), 0)
})

testthat::test_that("snapshot read failures preserve returned checks and remain errors", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  inputs <- file.path(root, "assessment", "inputs")
  file <- file.path(inputs, "landCover.csv")
  if (.Platform$OS.type != "windows") {
    Sys.chmod(file, "0000")
    on.exit(Sys.chmod(file, "0644"), add = TRUE, after = FALSE)
    if (file.access(file, 4) != 0L) {
      testthat::expect_error(
        suppressWarnings(validator$snapshot_validation_inputs(inputs)),
        "Cannot hash"
      )
    }
    Sys.chmod(file, "0644")
    Sys.chmod(inputs, "0000")
    on.exit(Sys.chmod(inputs, "0755"), add = TRUE, after = FALSE)
    if (file.access(inputs, 5) != 0L) {
      testthat::expect_error(validator$snapshot_validation_inputs(inputs), "Cannot read")
    }
    Sys.chmod(inputs, "0755")
  }
})


assessment_result <- function(root) {
  result <- NULL
  capture.output(result <- withr::with_dir(report_repo, validator$run_assessment_validation(
    normalizePath(root, winslash = "/"), "assessment", file.path(report_repo, "schema")
  )))
  result
}

testthat::test_that("standalone assessments retain outcomes, stages and input identity", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  before <- tools::md5sum(list.files(root, recursive = TRUE, full.names = TRUE))
  withr::local_seed(10)
  seed <- .Random.seed
  result <- assessment_result(root)
  testthat::expect_identical(result$report_type, "assessment")
  testthat::expect_identical(result$outcome, "success")
  testthat::expect_identical(result$validation$stages$status, c("completed", "not_applicable"))
  testthat::expect_true(result$validation$inputs$stable)
  testthat::expect_identical(result$validation$inputs$algorithm, "csv-md5-v1")
  testthat::expect_identical(.Random.seed, seed)
  testthat::expect_identical(tools::md5sum(names(before)), before)
  destination <- tempfile(fileext = ".json")
  on.exit(unlink(destination), add = TRUE)
  validator$write_run_report(result, destination)
  expect_report_schema(destination)
  unlink(file.path(root, "assessment", "inputs", "landCover.csv"))
  result <- assessment_result(root)
  testthat::expect_identical(result$outcome, "invalid_data")
  testthat::expect_identical(result$validation$stages$status, c("completed", "skipped"))
  validator$write_run_report(result, paste0(destination, "-invalid"))
  on.exit(unlink(paste0(destination, "-invalid")), add = TRUE)
  expect_report_schema(paste0(destination, "-invalid"))
  yaml::write_yaml(list(qh_method = "bad"), file.path(root, "params.yaml"))
  result <- assessment_result(root)
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_identical(result$error$stage, "configuration")
  testthat::expect_identical(result$validation$stages$status, c("not_run", "not_run"))
})

testthat::test_that("standalone assessment failures preserve earlier schema rows", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  original <- validator$validate_schema_file
  rlang::local_bindings(
    build_assessment_file_plan = function(...) c("landCover.csv", "species.csv"),
    validate_schema_file = function(file_name, ...) {
      if (file_name == "species.csv") stop("second CSV failed")
      original(file_name, ...)
    }, .env = validator
  )
  result <- assessment_result(root)
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_match(result$error$message, "second CSV")
  testthat::expect_identical(result$validation$rows$check_id, "landCover.csv")
  testthat::expect_identical(result$validation$stages$status, c("error", "not_run"))
})

testthat::test_that("standalone assessments reject input changes detected after checks finish", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  original <- validator$run_schema_validation
  rlang::local_bindings(run_schema_validation = function(assessment, inputs_dir, ...) {
    result <- original(assessment = assessment, inputs_dir = inputs_dir, ...)
    cat("\n", file = file.path(inputs_dir, "landCover.csv"), append = TRUE)
    result
  }, .env = validator)
  result <- assessment_result(root)
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_identical(result$error$stage, "input_snapshot")
  testthat::expect_false(result$validation$inputs$stable)
  testthat::expect_true(all(result$validation$rows$status == "pass"))
})

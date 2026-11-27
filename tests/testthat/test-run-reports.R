testthat::test_that("published examples agree with the report schema", {
  examples <- list.files(file.path(report_repo, "tests", "testthat", "fixtures", "run-reports"),
    full.names = TRUE
  )
  testthat::expect_length(examples, 6)
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

testthat::test_that("callable validation returns results and preserves caller state", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  before <- tools::md5sum(list.files(root, recursive = TRUE, full.names = TRUE))
  withr::local_seed(10)
  rng <- .Random.seed
  directory <- getwd()
  sinks <- sink.number()
  result <- validation_result(root)
  testthat::expect_identical(result$outcome, "success")
  testthat::expect_null(result$error)
  testthat::expect_identical(
    assessment_stage_statuses(result), c("completed", "not_applicable")
  )
  testthat::expect_equal(nrow(assessment_rows(result)), 1)
  testthat::expect_identical(result$backend$version, readLines(file.path(report_repo, "VERSION")))
  testthat::expect_identical(.Random.seed, rng)
  testthat::expect_identical(getwd(), directory)
  testthat::expect_identical(sink.number(), sinks)
  testthat::expect_identical(tools::md5sum(names(before)), before)
  testthat::expect_false(identical(result$run_id, validation_result(root)$run_id))
})

testthat::test_that("CLI exports success, invalid inputs and ordinary errors with 0/1/2 exits", {
  root <- validation_project()
  reports <- tempfile("reports-")
  dir.create(reports)
  on.exit(unlink(c(root, reports), recursive = TRUE), add = TRUE)
  success <- file.path(reports, "success.json")
  command <- validation_command(c("--data-root", root, paste0("--report=", success)))
  testthat::expect_identical(command$status, 0L, info = paste(command$output, collapse = "\n"))
  expect_report_schema(success)
  report <- jsonlite::fromJSON(success, simplifyVector = FALSE)
  testthat::expect_length(report$scope$assessments, 1)
  testthat::expect_null(report$error)
  testthat::expect_null(
    report$validation$assessments[[1]]$validation$stages[[1]]$checks[[1]]$details
  )
  testthat::expect_identical(validation_command(c("--data-root", root))$status, 0L)

  unlink(file.path(root, "assessment", "inputs", "landCover.csv"))
  invalid <- file.path(reports, "invalid.json")
  testthat::expect_identical(validation_command(c(
    "--data-root", root, "--report", invalid, "--verbose"
  ))$status, 1L)
  expect_report_schema(invalid)
  report <- jsonlite::fromJSON(invalid, simplifyVector = FALSE)
  testthat::expect_identical(report$outcome, "invalid_data")
  testthat::expect_identical(vapply(
    report$validation$assessments[[1]]$validation$stages, `[[`, character(1), "status"
  ), c("completed", "skipped"))
  testthat::expect_match(
    report$validation$assessments[[1]]$validation$stages[[1]]$checks[[1]]$message, "Missing input"
  )
  testthat::expect_identical(validation_command(c("--data-root", root))$status, 1L)

  writeLines("invalid: [", file.path(root, "params.yaml"))
  error <- file.path(reports, "error.json")
  testthat::expect_identical(validation_command(c(
    "--data-root", root, "--report", error
  ))$status, 2L)
  expect_report_schema(error)
  report <- jsonlite::fromJSON(error, simplifyVector = FALSE)
  testthat::expect_identical(report$outcome, "execution_error")
  testthat::expect_length(report$validation$assessments, 0)
  testthat::expect_length(report$scope$assessments, 1)
  testthat::expect_identical(report$error$stage, "configuration")
  testthat::expect_identical(validation_command(c("--data-root", root))$status, 2L)

  missing <- file.path(reports, "missing-root.json")
  testthat::expect_identical(validation_command(c("--report", missing))$status, 2L)
  expect_report_schema(missing)
  nonexistent <- file.path(reports, "nonexistent-root.json")
  testthat::expect_identical(validation_command(c(
    "--data-root", file.path(root, "absent"), "--report", nonexistent
  ))$status, 2L)
  expect_report_schema(nonexistent)
  testthat::expect_identical(validation_command("--help")$status, 0L)
})

testthat::test_that("empty scope, empty stages and file-plan failures cannot pass", {
  root <- validation_project("empirical")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  result <- validation_result(root)
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_identical(
    result$validation$assessments[[1]]$error$stage, "file_plan"
  )
  testthat::expect_identical(
    assessment_stage_statuses(result), rep("not_run", 2)
  )
  result <- validation_result(root, exclude_dirs = "assessment")
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_match(result$error$message, "No assessments")
  land_cover_path <- file.path(root, "assessment", "inputs", "landCover.csv")
  land_cover <- validator$read_csv_table(land_cover_path)
  land_cover$area_ha <- -1
  write.csv(land_cover, land_cover_path, row.names = FALSE)
  yaml::write_yaml(list(qh_method = "assume.0"), file.path(root, "params.yaml"))
  result <- validation_result(root)
  testthat::expect_identical(result$outcome, "invalid_data")
  testthat::expect_identical(assessment_rows(result)$status, "fail")
  rlang::local_bindings(
    run_schema_validation = function(...) validator$bind_validation_reports(), .env = validator
  )
  yaml::write_yaml(list(qh_method = "assume.0"), file.path(root, "params.yaml"))
  result <- validation_result(root)
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_match(result$validation$assessments[[1]]$error$message, "no checks")
  testthat::expect_identical(
    assessment_stage_statuses(result), c("error", "not_run")
  )
})

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

  dir.create(file.path(root, "second", "inputs"), recursive = TRUE)
  rlang::local_bindings(build_assessment_file_plan = function(...) {
    c("landCover.csv", "species.csv")
  }, .env = validator)
  result <- validation_result(root)
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_identical(
    assessment_rows(result)$check_id, "landCover.csv"
  )
  testthat::expect_identical(
    assessment_stage_statuses(result),
    c("error", "not_run")
  )
  destination <- tempfile(fileext = ".json")
  on.exit(unlink(destination), add = TRUE)
  validator$write_run_report(result, destination)
  expect_report_schema(destination)
})

testthat::test_that("assessment errors preserve results and allow later assessments to run", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "second", "inputs"), recursive = TRUE)
  yaml::write_yaml(
    list(qh_method = "assume.0", assessments = list(second = list(qh_method = "bad"))),
    file.path(root, "params.yaml")
  )
  result <- validation_result(root)
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_null(result$error)
  testthat::expect_identical(result$validation$assessments[[2]]$scope$assessment, "second")
  testthat::expect_identical(result$validation$assessments[[2]]$error$stage, "configuration")
  testthat::expect_equal(nrow(assessment_rows(result)), 1)
  testthat::expect_identical(
    assessment_stage_statuses(result),
    c("completed", "not_applicable")
  )
})

testthat::test_that("real warning and cross-file results keep their scientific semantics", {
  root <- tempfile("validation-demo-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  demo <- file.path(report_repo, "data", "vqa-demo1-min")
  testthat::expect_true(all(file.copy(file.path(demo, c("params.yaml", "main_001_current")),
    root,
    recursive = TRUE
  )))
  result <- validation_result(root)
  testthat::expect_identical(result$outcome, "success")
  testthat::expect_true(
    any(assessment_rows(result)$severity == "warning")
  )
  testthat::expect_true(all(assessment_rows(result)$status == "pass"))
  metadata <- file.path(root, "main_001_current", "inputs", "plotMetadata.csv")
  table <- validator$read_csv_table(metadata)
  write.csv(rbind(table, table[1, ]), metadata, row.names = FALSE)
  result <- validation_result(root)
  testthat::expect_identical(result$outcome, "invalid_data")
  rows <- assessment_rows(result)
  testthat::expect_true(any(rows$stage == "crossfile" & rows$status == "fail"))
  testthat::expect_true(
    all(assessment_stage_statuses(result) == "completed")
  )

  rlang::local_bindings(
    validate_species_lookup = function(...) stop("cross-file exception"),
    .env = validator
  )
  result <- validation_result(root)
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_match(result$validation$assessments[[1]]$error$message, "cross-file exception")
  testthat::expect_identical(
    assessment_stage_statuses(result), c("completed", "error")
  )
  testthat::expect_true(
    any(assessment_rows(result)$stage == "crossfile")
  )
})

testthat::test_that("publication preserves Unicode, nulls, arrays and caller RNG", {
  root <- validation_project()
  directory <- tempfile("report-output-")
  dir.create(directory)
  on.exit(unlink(c(root, directory), recursive = TRUE), add = TRUE)
  result <- validation_result(root, exclude_dirs = character())
  text <- "café \"quoted\" \\ path\nnext line"
  result$validation$assessments[[1]]$validation$stages[[1]]$checks$message <- text
  result$validation$assessments[[1]]$validation$stages[[1]]$checks$details <- ""
  destination <- file.path(directory, "résultat.json")
  validator$write_run_report(result, destination)
  expect_report_schema(destination)
  loaded <- jsonlite::fromJSON(destination, simplifyVector = FALSE)
  testthat::expect_identical(
    loaded$validation$assessments[[1]]$validation$stages[[1]]$checks[[1]]$message, text
  )
  testthat::expect_identical(
    loaded$validation$assessments[[1]]$validation$stages[[1]]$checks[[1]]$details, ""
  )
  testthat::expect_null(
    loaded$validation$assessments[[1]]$validation$stages[[1]]$checks[[1]]$extra_columns
  )
  testthat::expect_length(loaded$scope$excluded_dirs, 0)
  testthat::expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 1)
  testthat::expect_match(loaded$started_at, "Z$")
  testthat::expect_true(loaded$started_at <= loaded$ended_at)
})

testthat::test_that("exports never clobber existing files, symlinks or publication races", {
  root <- validation_project()
  directory <- tempfile("report-destinations-")
  dir.create(directory)
  on.exit(unlink(c(root, directory), recursive = TRUE), add = TRUE)
  result <- validation_result(root)
  destination <- file.path(directory, "existing.json")
  validator$write_run_report(result, destination)
  previous <- readBin(destination, "raw", n = file.info(destination)$size)
  command <- validation_command(c("--data-root", root, "--report", destination))
  testthat::expect_identical(command$status, 2L)
  testthat::expect_true(any(grepl("not this invocation", command$output, fixed = TRUE)))
  testthat::expect_identical(readBin(destination, "raw", n = file.info(destination)$size), previous)
  testthat::expect_error(validator$write_run_report(result, directory), "new file")
  testthat::expect_error(validator$write_run_report(result, ""), "new file")
  absent <- file.path(directory, "absent", "report.json")
  testthat::expect_identical(validation_command(c(
    "--data-root", root, "--report", absent
  ))$status, 2L)
  testthat::expect_false(file.exists(absent))
  if (.Platform$OS.type != "windows") {
    link <- file.path(directory, "dangling.json")
    testthat::expect_true(file.symlink(file.path(directory, "missing-target"), link))
    testthat::expect_error(validator$write_run_report(result, link), "already exists")
  }

  destination <- file.path(directory, "race.json")
  rlang::local_bindings(file.link = function(from, to) {
    writeLines("concurrent writer", to)
    base::file.link(from, to)
  }, .env = validator)
  testthat::expect_error(validator$write_run_report(result, destination), "Could not publish")
  testthat::expect_identical(readLines(destination), "concurrent writer")
  testthat::expect_false(any(grepl(".openvqa-report-", list.files(directory, all.files = TRUE),
    fixed = TRUE
  )))
})

testthat::test_that("write and publication failures exit 2 and remove temporary files", {
  root <- validation_project()
  directory <- tempfile("report-failures-")
  dir.create(directory)
  on.exit(unlink(c(root, directory), recursive = TRUE), add = TRUE)
  rlang::local_bindings(file.link = function(...) FALSE, .env = validator)
  destination <- file.path(directory, "report.json")
  status <- NULL
  testthat::expect_message(capture.output(status <- withr::with_dir(report_repo, validator$main(c(
    "--data-root", root, "--report", destination
  )))), "Could not publish")
  testthat::expect_identical(status, 2L)
  testthat::expect_false(file.exists(destination))
  testthat::expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 0)
  if (.Platform$OS.type != "windows") {
    Sys.chmod(directory, "0555")
    on.exit(Sys.chmod(directory, "0755"), add = TRUE, after = FALSE)
    if (file.access(directory, 2) != 0L) {
      testthat::expect_identical(validation_command(c(
        "--data-root", root, "--report", destination
      ))$status, 2L)
      testthat::expect_false(file.exists(destination))
    }
  }
})

testthat::test_that("archive metadata is nullable and malformed successful reports are rejected", {
  directory <- tempfile("report-archive-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  result <- withr::with_dir(directory, validator$new_run_report("validate", list()))
  testthat::expect_null(result$backend$version)
  testthat::expect_null(result$backend$revision)
  testthat::expect_error(validator$run_report_exit_status(list(outcome = "unknown")), "Unknown")
  report <- jsonlite::fromJSON(file.path(
    report_repo, "tests", "testthat", "fixtures", "run-reports", "success.json"
  ), simplifyVector = FALSE)
  report$validation$assessments[[1]]$validation$stages[[1]]$status <- "skipped"
  report$validation$assessments[[1]]$validation$stages[[1]]$reason <- "unavailable"
  testthat::expect_false(jsonvalidate::json_validate(
    jsonlite::toJSON(report, auto_unbox = TRUE, null = "null"), report_schema,
    engine = "ajv"
  ))
})


testthat::test_that("single-assessment selection uses overrides and ignores unrelated failures", {
  root <- validation_project()
  reports <- tempfile("assessment-reports-")
  dir.create(reports)
  on.exit(unlink(c(root, reports), recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "broken", "inputs"), recursive = TRUE)
  dir.create(file.path(root, "third", "inputs"), recursive = TRUE)
  testthat::expect_true(file.copy(
    file.path(root, "assessment", "inputs", "landCover.csv"),
    file.path(root, "third", "inputs")
  ))
  yaml::write_yaml(list(qh_method = "empirical", active_indicators = "SR", assessments = list(
    assessment = list(qh_method = "assume.0"), broken = list(qh_method = "bad"),
    third = list(qh_method = "assume.0")
  )), file.path(root, "params.yaml"))
  result <- validation_result(root)
  children <- result$validation$assessments
  testthat::expect_identical(
    vapply(children, function(x) x$outcome, character(1)),
    c("success", "execution_error", "success")
  )
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_null(result$error)
  testthat::expect_identical(children[[2]]$error$stage, "configuration")
  path <- file.path(reports, "project.json")
  testthat::expect_identical(
    validation_command(c("--data-root", root, "--report", path))$status, 2L
  )
  expect_report_schema(path)

  path <- file.path(reports, "selected.json")
  testthat::expect_identical(validation_command(c(
    "--data-root", root, "--assessment", "assessment", "--report", path
  ))$status, 0L)
  expect_report_schema(path)
  selected <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  testthat::expect_identical(selected$scope$type, "assessment")
  testthat::expect_identical(selected$scope$requested_assessment, "assessment")
  testthat::expect_identical(selected$scope$assessments, list("assessment"))
  testthat::expect_length(selected$validation$assessments, 1)
  child <- selected$validation$assessments[[1]]
  testthat::expect_identical(child$report_type, "assessment")
  testthat::expect_identical(child$validation$stages[[2]]$status, "not_applicable")
  # The same schema accepts a child extracted from a project or a single-assessment run.
  standalone <- file.path(reports, "standalone.json")
  validator$write_run_report(child, standalone)
  expect_report_schema(standalone)
  testthat::expect_identical(validation_command(c("--data-root", root, "-a", "third"))$status, 0L)
  testthat::expect_identical(
    validation_result(root, assessment = "broken")$outcome, "execution_error"
  )
})

testthat::test_that("assessment selection rejects missing, excluded and escaping paths", {
  root <- validation_project()
  outside <- validation_project()
  on.exit(unlink(c(root, outside), recursive = TRUE), add = TRUE)
  for (name in c("missing", "../assessment", ".", "assessment/inputs", "")) {
    result <- validation_result(root, assessment = name)
    testthat::expect_identical(result$outcome, "execution_error")
    testthat::expect_identical(result$error$stage, "selection")
    testthat::expect_length(result$validation$assessments, 0)
  }
  result <- validation_result(root, assessment = "assessment", exclude_dirs = "assessment")
  testthat::expect_match(result$error$message, "excluded")
  if (.Platform$OS.type != "windows") {
    testthat::expect_true(file.symlink(file.path(outside, "assessment"), file.path(root, "escape")))
    result <- validation_result(root, assessment = "escape")
    testthat::expect_identical(result$validation$assessments[[1]]$outcome, "execution_error")
    testthat::expect_match(result$validation$assessments[[1]]$error$message, "belong")
  }
})

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
  result <- validation_result(root)
  testthat::expect_identical(result$outcome, "invalid_data")
  testthat::expect_true(result$validation$assessments[[1]]$validation$inputs$stable)
})

testthat::test_that("input changes prevent success and do not stop other assessments", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "second", "inputs"), recursive = TRUE)
  testthat::expect_true(file.copy(
    file.path(root, "assessment", "inputs", "landCover.csv"),
    file.path(root, "second", "inputs")
  ))
  original <- validator$run_schema_validation
  rlang::local_bindings(run_schema_validation = function(assessment, inputs_dir, ...) {
    result <- original(assessment = assessment, inputs_dir = inputs_dir, ...)
    if (assessment == "assessment") {
      file <- file.path(inputs_dir, "landCover.csv")
      cat("\n", file = file, append = TRUE)
    }
    result
  }, .env = validator)
  result <- validation_result(root)
  testthat::expect_identical(result$outcome, "execution_error")
  first <- result$validation$assessments[[1]]
  testthat::expect_identical(first$error$stage, "input_snapshot")
  testthat::expect_false(first$validation$inputs$stable)
  testthat::expect_true(all(validator$validation_report_rows(first)$status == "pass"))
  testthat::expect_identical(result$validation$assessments[[2]]$outcome, "success")
  destination <- tempfile(fileext = ".json")
  on.exit(unlink(destination), add = TRUE)
  validator$write_run_report(result, destination)
  expect_report_schema(destination)
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
  original <- validator$snapshot_validation_inputs
  calls <- 0L
  rlang::local_bindings(snapshot_validation_inputs = function(...) {
    calls <<- calls + 1L
    if (calls == 2L) stop("Snapshot recheck failed")
    original(...)
  }, .env = validator)
  result <- validation_result(root)
  child <- result$validation$assessments[[1]]
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_match(child$error$message, "recheck failed")
  testthat::expect_false(child$validation$inputs$stable)
  testthat::expect_equal(nrow(validator$validation_report_rows(child)), 1)
})

testthat::test_that("completed reports inherit context and keep warnings separate from failure", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  result <- validation_result(root)
  child <- result$validation$assessments[[1]]
  testthat::expect_identical(result$report_version, "2.1")
  testthat::expect_named(child$scope, c("data_root", "assessment"))
  testthat::expect_named(child$validation, c("stages", "inputs", "settings"))
  for (stage in child$validation$stages) {
    testthat::expect_named(stage, c("stage", "status", "reason", "checks"))
    testthat::expect_false(any(c("assessment", "stage") %in% names(stage$checks)))
  }
  checks <- child$validation$stages[[1]]$checks
  testthat::expect_identical(checks$status, "pass")
  testthat::expect_true(all(is.na(checks$severity)))
  rows <- validator$validation_report_rows(child)
  testthat::expect_identical(rows$assessment, "assessment")
  testthat::expect_identical(rows$stage, "schema")

  land_cover <- data.frame(vegClass = "forest", lc.type = "vegetated/forest")
  vegetation <- land_cover
  agree <- validator$check_vegetation_type_consistency(land_cover, vegetation)
  testthat::expect_identical(agree$severity, "warning")
  testthat::expect_identical(agree$status, "pass")
  testthat::expect_true(is.na(validator$pass_result("ok", "No issue")$severity))
  vegetation$lc.type <- "changed"
  warning <- validator$check_vegetation_type_consistency(land_cover, vegetation)
  testthat::expect_identical(warning$severity, "warning")
  testthat::expect_identical(warning$status, "pass")
})

testthat::test_that("schema-invalid reports cannot be returned or written to any file", {
  root <- validation_project()
  directory <- tempfile("invalid-report-export-")
  dir.create(directory)
  on.exit(unlink(c(root, directory), recursive = TRUE), add = TRUE)
  result <- validation_result(root)
  result$validation$assessments[[1]]$validation$stages[[1]]$checks$severity <- "error"
  destination <- file.path(directory, "invalid.json")
  testthat::expect_error(
    validator$write_run_report(result, destination),
    class = "validation_error"
  )
  testthat::expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 0)

  original <- validator$run_schema_validation
  rlang::local_bindings(run_schema_validation = function(..., on_result = NULL) {
    record <- function(fragment) {
      fragment$severity <- "error"
      on_result(fragment)
    }
    original(..., on_result = record)
  }, .env = validator)
  testthat::expect_error(withr::with_dir(report_repo, validator$run_assessment_validation(
    normalizePath(root, winslash = "/"), "assessment", file.path(report_repo, "schema")
  )), class = "validation_error")
  # The project may return a valid execution-error envelope, but never the malformed child.
  result <- validation_result(root)
  testthat::expect_identical(result$outcome, "execution_error")
  testthat::expect_identical(result$error$stage, "assessment_reporting")
  testthat::expect_length(result$validation$assessments, 0)
  testthat::expect_no_error(validator$validated_run_report_json(result))
})

testthat::test_that("parent report validation is enforced even without requesting JSON export", {
  root <- validation_project()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  original <- validator$run_assessment_validation
  rlang::local_bindings(run_assessment_validation = function(...) {
    child <- original(...)
    child$validation$stages[[1]]$checks$severity <- "error"
    child
  }, .env = validator)
  testthat::expect_error(validation_result(root), class = "validation_error")
  status <- NULL
  testthat::expect_message(capture.output(status <- withr::with_dir(
    report_repo,
    validator$main(c("--data-root", root))
  )), "validating json")
  testthat::expect_identical(status, 2L)
})

testthat::test_that("export revalidates edited objects and refuses incompatible report versions", {
  root <- validation_project()
  directory <- tempfile("revalidate-report-")
  dir.create(directory)
  on.exit(unlink(c(root, directory), recursive = TRUE), add = TRUE)
  result <- validation_result(root)
  result$report_version <- "1.0"
  testthat::expect_error(validator$write_run_report(result, file.path(directory, "old.json")),
    class = "validation_error"
  )
  result$report_version <- "2.1"
  result$validation$assessments[[1]]$validation$stages[[1]]$checks$check_id <- NULL
  testthat::expect_error(validator$write_run_report(result, file.path(directory, "missing.json")),
    class = "validation_error"
  )
  testthat::expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 0)
})

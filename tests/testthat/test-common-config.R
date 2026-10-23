testthat::test_that("application defaults and catalogue agree", {
  defaults <- load_default_params()
  schema <- jsonlite::read_json(file.path(.params_root, "schema", "params.schema.json"))
  testthat::expect_true(all(names(defaults) %in% names(schema$properties)))
  testthat::expect_identical(names(defaults), names(legacy_common_symbols))
  testthat::expect_no_error(validate_params_schema(defaults))
  for (property in schema$properties) {
    testthat::expect_true(nzchar(property$title))
    testthat::expect_true(nzchar(property$description))
    testthat::expect_null(property$default)
  }
  for (invalid in list(list(beta_buffer = 0.11), list(beta_algorithm = "unknown"),
                       list(permutation_reps = 1.5), list(plot_hist = "true"),
                       list(logfile_basename = "../outside"), list(misspelt = TRUE),
                       list(group_mtext_line = 4), list(inverse_logit_beta = 0))) {
    testthat::expect_error(validate_params_schema(resolve_params(defaults, invalid)))
  }
  missing <- defaults
  missing$beta_buffer <- NULL
  testthat::expect_error(validate_params_schema(missing))
  # JSON arrays must remain arrays for zero, one, and several indicator codes.
  for (indicators in list(list(), "SR", c("SR", "TD"))) {
    testthat::expect_no_error(validate_params_schema(c(defaults, list(active_indicators = indicators))))
  }
  testthat::expect_error(validate_params_schema(c(defaults, list(active_indicators = c("SR", "SR")))))
})

testthat::test_that("all six assessments use the expected common script parameters", {
  expected <- dget("fixtures-common-params.R")
  old_directory <- getwd()
  on.exit(setwd(old_directory))
  setwd(.params_root)
  contracts <- list(
    "vqa-demo1" = c("main_001_current", "offset_001_current"),
    "vqa-demo2" = c("offset_baseline", "offset_current", "project_baseline", "project_current")
  )
  for (project in names(contracts)) for (assessment in contracts[[project]]) {
    root <- file.path(.params_root, "data", paste0(project, "-min"))
    context <- load_preprocessing_context(root, assessment, seed = 7L)
    testthat::expect_no_error(validate_params_schema(context$params))
    mapped <- legacy_common_params(context$params)
    testthat::expect_equal(mapped, expected, info = paste(project, assessment))
    env <- prepare_legacy_environment(context, context$config, 7L)
    testthat::expect_equal(mget(names(expected), env), expected)
  }
})

testthat::test_that("common configuration resolves once with full override precedence", {
  root <- fixture_root("vqa-demo2")
  on.exit(unlink(root, recursive = TRUE))
  project <- load_project_config(root)$data
  project$beta_buffer <- 0.02
  project$assessments$offset_baseline$beta_buffer <- 0.03
  yaml::write_yaml(project, file.path(root, "params.yaml"))
  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(tempdir())
  first <- load_preprocessing_context(root, "offset_baseline", seed = 7L)
  testthat::expect_equal(first$params$beta_buffer, 0.03)
  second <- load_preprocessing_context(root, "offset_baseline", seed = 7L,
                                       explicit = list(beta_buffer = 0.04))
  testthat::expect_equal(second$params$beta_buffer, 0.04)
  testthat::expect_equal(load_default_params()$beta_buffer, 0.01)
  testthat::expect_error(load_preprocessing_context(root, "offset_baseline", seed = 7L,
    explicit = list(unknown_setting = TRUE)))
  testthat::expect_error(resolve_assessment_params(list(beta_buffer = 0.11), "current"))
  yaml::write_yaml(list(project = "changed", beta_buffer = 0.1), file.path(root, "params.yaml"))
  setwd(.params_root)
  second$params$beta_buffer <- 0.2
  env <- prepare_legacy_environment(second, second$config, 7L)
  testthat::expect_equal(env$buffer, 0.04)
  testthat::expect_equal(first$params$beta_buffer, 0.03)
  testthat::expect_identical(env$VQA_COMMON_PARAMS, legacy_common_params(second$config))
})


testthat::test_that("standalone legacy callers load the same application defaults", {
  old_directory <- getwd()
  on.exit(setwd(old_directory))
  root <- fixture_root("vqa-demo2")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  setwd(.params_root)
  env <- new.env(parent = globalenv())
  env$wd <- .params_root
  env$job <- "vqa.batch"
  env$VQA_RUN_ENV <- env
  env$source <- function(file, ...) sys.source(file, envir = env)
  env$VQA_DATA_ROOT <- root
  env$VQA_PROJECT <- "vqa-demo2"
  env$VQA_ASSESSMENT <- "offset_current"
  sys.source("includes/functions.R", envir = env)
  sys.source("params.R", envir = env)
  expected <- legacy_common_params(load_default_params())
  testthat::expect_equal(mget(names(expected), env), expected)
})

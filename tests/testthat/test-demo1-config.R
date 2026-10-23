testthat::test_that("demo 1 assessments resolve the expected analysis parameters", {
  expected <- dget("fixtures-demo1-params.R")
  root <- fixture_root("vqa-demo1")
  on.exit(unlink(root, recursive = TRUE))
  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(.params_root)
  for (assessment in c("main_001_current", "offset_001_current")) {
    context <- load_preprocessing_context(root, assessment, seed = 7L)
    testthat::expect_no_error(validate_analysis_params(context$params))
    env <- prepare_legacy_environment(context, context$config, 7L)
    scalars <- setdiff(names(expected), c("indicators", "nmds"))
    testthat::expect_equal(mget(scalars, env), expected[scalars])
    for (code in env$EI.vec) {
      parameters <- env$ei.params(code)
      testthat::expect_equal(
        parameters[names(expected$indicators[[code]])],
        expected$indicators[[code]]
      )
    }
    testthat::expect_equal(env$nmds.params("forest"), expected$nmds)
    testthat::expect_false(exists("scope", envir = env, inherits = FALSE))
  }
  testthat::expect_true(
    file.rename(
      file.path(root, "main_001_current"),
      file.path(root, "renamed_assessment")
    )
  )
  context <- load_preprocessing_context(root, "renamed_assessment", seed = 7L)
  testthat::expect_no_error(prepare_legacy_environment(context, context$config, 7L))
})

testthat::test_that("nested calculation overrides preserve other resolved settings", {
  root <- fixture_root("vqa-demo1")
  on.exit(unlink(root, recursive = TRUE))
  project <- load_project_config(root)$data
  project$assessments <- list(main_001_current = list(
    indicator_settings = list(SR = list(test_tail = "both")), nmds = list(seed = 21L)
  ))
  yaml::write_yaml(project, file.path(root, "params.yaml"))
  original <- load_preprocessing_context(root, "offset_001_current", seed = 7L)
  changed <- load_preprocessing_context(root, "main_001_current",
    seed = 7L,
    explicit = list(
      indicator_settings = list(TD = list(td_multiplier_omit = "forest")),
      nmds = list(seed = 31L)
    )
  )
  testthat::expect_identical(changed$params$indicator_settings$SR$test_tail, "both")
  testthat::expect_identical(changed$params$indicator_settings$TD$td_multiplier_omit, "forest")
  testthat::expect_identical(
    changed$params$indicator_settings$PCESS,
    original$params$indicator_settings$PCESS
  )
  testthat::expect_equal(changed$params$nmds$seed, 31L)
  testthat::expect_equal(changed$params$nmds$dimensions, original$params$nmds$dimensions)
  testthat::expect_equal(original$params$nmds$seed, 10L)
  testthat::expect_identical(original$params$indicator_settings$SR$test_tail, "lower")
  testthat::expect_identical(load_project_config(root)$data, project)
  changed$params$indicator_settings$TD$td_multiplier_omit <- list()
  testthat::expect_no_error(validate_analysis_params(changed$params))
})

testthat::test_that("demo 1 rejects incomplete or inconsistent calculation settings", {
  root <- fixture_root("vqa-demo1")
  on.exit(unlink(root, recursive = TRUE))
  for (invalid in list(
    list(bootstrap_reps = 0),
    list(bootstrap_reps = NULL),
    list(unknown_setting = TRUE),
    list(nmds = list(dimensions = 0)),
    list(nmds = list(seed = -2147483648)),
    list(nmds = list(unknown_setting = TRUE)),
    list(indicator_settings = list(SR = list(test_tail = "unknown"))),
    list(indicator_settings = list(SR = list(unknown_setting = TRUE))),
    list(indicator_settings = list(PCESS = list(benchmark_value = NULL))),
    list(indicator_settings = list(TD = list(td_multiplier = NULL))),
    list(active_indicators = "BA"), list(active_indicators = "SR"),
    list(indicator_groups = list(SR = NULL))
  )) {
    testthat::expect_error(
      load_preprocessing_context(
        root, "main_001_current",
        seed = 7L, explicit = invalid
      )
    )
  }
})

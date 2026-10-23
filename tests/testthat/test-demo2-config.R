testthat::test_that("demo 2 assessments resolve the expected analysis parameters", {
  expected <- dget("fixtures-demo2-params.R")
  root <- fixture_root("vqa-demo2")
  on.exit(unlink(root, recursive = TRUE))
  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(.params_root)
  for (assessment in names(expected$assessments)) {
    context <- load_preprocessing_context(root, assessment, seed = 7L)
    testthat::expect_no_error(validate_analysis_params(context$params))
    env <- prepare_legacy_environment(context, context$config, 7L)
    settings <- expected$assessments[[assessment]]
    testthat::expect_equal(mget(names(settings), env), settings)
    testthat::expect_false(exists("scope", envir = env, inherits = FALSE))
    if (context$params$qh_method == "empirical") {
      testthat::expect_equal(env$EI.F.GROUPS, expected$groups)
      testthat::expect_identical(env$EI.agg.list$fname, c("PCGF", "ASC"))
      for (code in env$EI.vec) {
        parameters <- env$ei.params(code)
        testthat::expect_equal(
          parameters[
            names(expected$indicators[[code]])
          ],
          expected$indicators[[code]]
        )
      }
    } else {
      testthat::expect_identical(env$EI.vec, character())
      testthat::expect_identical(dim(env$EI.F.GROUPS), c(0L, 2L))
      testthat::expect_equal(nrow(env$EI.agg.list), 0L)
    }
  }
})

testthat::test_that("basal-area normalization preserves sampling boundaries and missing values", {
  expected <- dget("fixtures-demo2-params.R")$basal_area
  root <- fixture_root("vqa-demo2")
  on.exit(unlink(root, recursive = TRUE))
  context <- load_preprocessing_context(root, "project_current", seed = 7L)
  parameters <- legacy_project_params(context$config)
  testthat::expect_equal(
    parameters$dbh.ba.scaled(expected$dbh),
    expected$per_hectare,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    parameters$dbh.ba.scaled(expected$dbh, 100),
    expected$per_100_m2,
    tolerance = 1e-12
  )
  changed <- load_preprocessing_context(root, "project_current",
    seed = 7L,
    explicit = list(stem_sampling = list(small_plot_area_m2 = 96))
  )
  changed_parameters <- legacy_project_params(changed$config)
  testthat::expect_equal(changed_parameters$dbh.ba.scaled(2.5), parameters$dbh.ba.scaled(2.5) / 2)
  testthat::expect_equal(changed_parameters$dbh.ba.scaled(10), parameters$dbh.ba.scaled(10))
})

testthat::test_that("assumed quality and source labels are selected by configuration", {
  root <- fixture_root("vqa-demo2")
  on.exit(unlink(root, recursive = TRUE))
  project <- load_project_config(root)$data
  project$assessments$renamed_assessment <- project$assessments$offset_baseline
  yaml::write_yaml(project, file.path(root, "params.yaml"))
  testthat::expect_true(file.rename(
    file.path(root, "offset_baseline"),
    file.path(root, "renamed_assessment")
  ))
  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(.params_root)
  context <- load_preprocessing_context(root, "renamed_assessment", seed = 7L)
  env <- prepare_legacy_environment(context, context$config, 7L)
  testthat::expect_identical(env$QH.METHOD, "assume.0")
  testthat::expect_identical(env$RAW_PLOTDATA_FILENAME, "Demo_data_offsets.xlsx")
  testthat::expect_length(env$EI.vec, 0L)
  testthat::expect_no_error(run_vqa_analysis(context))
  changed <- load_preprocessing_context(root, "renamed_assessment",
    seed = 7L,
    explicit = list(
      qh_method = "empirical", active_indicators = project$active_indicators,
      aggregate_indicators = project$aggregate_indicators
    )
  )
  testthat::expect_identical(
    prepare_legacy_environment(changed, changed$config, 7L)$QH.METHOD,
    "empirical"
  )
})

testthat::test_that("demo 2 rejects incomplete or inconsistent indicator and sampling settings", {
  root <- fixture_root("vqa-demo2")
  on.exit(unlink(root, recursive = TRUE))
  for (invalid in list(
    list(bootstrap_reps = NULL), list(active_indicators = list()),
    list(aggregate_indicators = "TD"), list(unknown_setting = TRUE),
    list(stem_sampling = NULL), list(stem_sampling = list(small_plot_area_m2 = 0)),
    list(stem_sampling = list(split_dbh_cm = 2.5)),
    list(stem_sampling = list(unknown_setting = TRUE)),
    list(indicator_settings = list(PCES = list(benchmark_value = NULL))),
    list(indicator_settings = list(BA = list(normalize_area_m2 = TRUE))),
    list(indicator_settings = list(ASC = list(multiplier = 0))),
    list(indicator_settings = list(SR = list(data_type = "unknown")))
  )) {
    testthat::expect_error(load_preprocessing_context(root, "project_current",
      seed = 7L,
      explicit = invalid
    ))
  }
})

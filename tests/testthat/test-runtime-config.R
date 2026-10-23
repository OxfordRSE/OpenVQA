testthat::test_that("parameter precedence preserves inputs and empty assessment overrides", {
  defaults <- list(value = 1, default_only = TRUE)
  project <- list(
    value = 2, project_only = TRUE,
    assessments = list(current = list(value = 3, active_indicators = list()))
  )
  original <- project
  testthat::expect_identical(resolve_params(defaults)$value, 1)
  testthat::expect_identical(resolve_params(defaults, project)$value, 2)
  testthat::expect_identical(
    resolve_params(defaults, project, project$assessments$current)$value,
    3
  )
  resolved <- resolve_params(defaults, project, project$assessments$current, list(value = 4))
  testthat::expect_identical(resolved$value, 4)
  testthat::expect_true(resolved$default_only)
  testthat::expect_true(resolved$project_only)
  testthat::expect_identical(resolved$active_indicators, list())
  nested <- resolve_params(
    list(
      indicator_settings = list(
        TD = list(
          td_multiplier_omit = list(
            "forest"
          )
        )
      )
    ),
    explicit = list(indicator_settings = list(TD = list(td_multiplier_omit = list())))
  )
  testthat::expect_identical(nested$indicator_settings$TD$td_multiplier_omit, list())
  testthat::expect_null(resolved$assessments)
  testthat::expect_identical(project, original)
  project <- list(beta_buffer = 0.02, assessments = list(current = list(beta_buffer = 0.03)))
  testthat::expect_identical(resolve_assessment_params(project, "current")$beta_buffer, 0.03)
  testthat::expect_identical(resolve_assessment_params(project, "other")$beta_buffer, 0.02)
  testthat::expect_identical(resolve_assessment_params(NULL, "current"), list())
  testthat::expect_error(resolve_params(project = list("unnamed")), "named list")
  testthat::expect_error(resolve_params(project = "invalid"), "named list")
})

testthat::test_that("only known project labels map to legacy parameter scripts", {
  testthat::expect_identical(legacy_project_name("vqa-demo1-min"), "vqa-demo1")
  testthat::expect_identical(legacy_project_name("vqa-demo2-min"), "vqa-demo2")
  testthat::expect_identical(legacy_project_name("custom-min"), "custom-min")
  testthat::expect_error(legacy_project_name(NULL), "declare project")
})

testthat::test_that("seeds are integers and explicit seeds override the fallback", {
  old_seed <- Sys.getenv("VQA_TEST_SEED", unset = NA_character_)
  on.exit(if (is.na(old_seed)) {
    Sys.unsetenv("VQA_TEST_SEED")
  } else {
    Sys.setenv(VQA_TEST_SEED = old_seed)
  })
  Sys.unsetenv("VQA_TEST_SEED")
  testthat::expect_null(resolve_seed())
  for (invalid in list(
    NA, NA_integer_, numeric(), c(1, 2), 1.5, "1.5",
    "", "text", TRUE, Inf, NaN, 2147483648
  )) {
    testthat::expect_error(resolve_seed(invalid), "one non-missing integer")
  }
  for (seed in list(0L, -1, "20260810", "2147483647")) {
    testthat::expect_identical(resolve_seed(seed), as.integer(seed))
  }
  Sys.setenv(VQA_TEST_SEED = "20260810")
  testthat::expect_identical(resolve_seed(), 20260810L)
  testthat::expect_identical(resolve_seed(10L), 10L)
  Sys.setenv(VQA_TEST_SEED = "invalid")
  testthat::expect_error(resolve_seed(), "integer")
  testthat::expect_identical(resolve_seed(10L), 10L)
})

testthat::test_that("context resolution applies assessment overrides and derives paths", {
  root <- fixture_root("vqa-demo2")
  on.exit(unlink(root, recursive = TRUE))
  context <- load_preprocessing_context(root, "offset_baseline", seed = 7L)
  testthat::expect_error(run_vqa_analysis(context, seed = 1.5), "integer")
  changed <- context$config
  changed$assessment <- "offset_current"
  testthat::expect_error(run_vqa_analysis(context, config = changed), "resolved run context")
  testthat::expect_identical(context$params$qh_method, "assume.0")
  testthat::expect_length(context$params$active_indicators, 0L)
  testthat::expect_null(context$params$assessments)
  testthat::expect_identical(context$config$seed, 7L)
  testthat::expect_identical(
    context$paths$results,
    file.path(normalizePath(root), "offset_baseline", "results")
  )
  context$params$qh_method <- "changed"
  testthat::expect_identical(context$config$qh_method, "assume.0")
  override <- load_preprocessing_context(root, "offset_baseline",
    seed = 7L,
    explicit = list(qh_method = "empirical", active_indicators = "SR")
  )
  testthat::expect_identical(override$params$qh_method, "empirical")
  testthat::expect_identical(override$config$qh_method, "empirical")
  testthat::expect_error(
    load_preprocessing_context(root, "missing", seed = 7L),
    "does not exist"
  )
  for (assessment in c("../outside", "/outside", "offset_baseline/inputs", "..")) {
    testthat::expect_error(
      load_preprocessing_context(root, assessment, seed = 7L),
      "directory name"
    )
  }
  for (key in c(
    "data_root", "assessment", "seed", "paths", "inputs_dir", "output_dir",
    "results_dir", "figures_dir", "log_dir"
  )) {
    testthat::expect_error(
      resolve_params(project = setNames(list("elsewhere"), key)),
      "cannot be configured"
    )
  }
  yaml::write_yaml(
    list(project = "label", results_dir = "elsewhere"),
    file.path(root, "params.yaml")
  )
  testthat::expect_error(
    load_preprocessing_context(root, "offset_baseline", seed = 7L),
    "cannot be configured"
  )
})

testthat::test_that("assessment symlinks cannot escape the data root", {
  testthat::skip_on_os("windows")
  root <- tempfile("runtime-root-")
  outside <- tempfile("runtime-outside-")
  dir.create(root)
  dir.create(outside)
  on.exit(unlink(c(root, outside), recursive = TRUE))
  testthat::expect_true(file.symlink(outside, file.path(root, "escape")))
  testthat::expect_error(resolve_runtime_config(root, "escape", seed = 1L), "must belong")
})

testthat::test_that("CLI and function seeds reject fractions before preprocessing", {
  old_directory <- getwd()
  on.exit(setwd(old_directory))
  setwd(file.path("..", ".."))
  source("analyse.R", local = TRUE)
  cli <- parse_cli_args(c(
    "--data-root", "data/vqa-demo2-min", "--assessment",
    "offset_baseline", "--seed", "7"
  ))
  testthat::expect_identical(cli$seed, 7L)
  testthat::expect_error(parse_cli_args(c(
    "--data-root", "data/vqa-demo2-min",
    "--assessment", "offset_baseline", "--seed", "1.5"
  )), "integer")
  testthat::expect_error(
    run_analysis("data/vqa-demo2-min", "missing", seed = 1L),
    "does not exist"
  )
  testthat::expect_error(
    run_analysis("data/vqa-demo2-min", "offset_baseline", seed = 1.5),
    "integer"
  )
})

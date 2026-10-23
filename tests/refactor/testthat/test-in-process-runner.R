testthat::test_that("in-process analysis invocations do not leak legacy state", {
  selected_contract <- Sys.getenv("VQA_TEST_CONTRACT")
  if (nzchar(selected_contract) &&
    selected_contract != "vqa-demo1/main_001_current") {
    testthat::skip("The in-process isolation check runs in one CI shard.")
  }

  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(refactor_project_root)
  source("analyse.R", local = TRUE)

  first_root <- copy_refactor_project("vqa-demo1")
  second_root <- copy_refactor_project("vqa-demo2")
  on.exit(cleanup_refactor_project(first_root), add = TRUE)
  on.exit(cleanup_refactor_project(second_root), add = TRUE)

  old_seed <- Sys.getenv("VQA_TEST_SEED", unset = NA_character_)
  on.exit(if (is.na(old_seed)) Sys.unsetenv("VQA_TEST_SEED") else
    Sys.setenv(VQA_TEST_SEED = old_seed), add = TRUE)
  Sys.setenv(VQA_TEST_SEED = "invalid-fallback")
  first <- run_analysis(first_root, "main_001_current", seed = 20260810L)
  context <- load_preprocessing_context(second_root, "offset_baseline", seed = 1L)
  context <- prepare_analysis_inputs(context)
  yaml::write_yaml(list(project = "changed-after-resolution"),
                   file.path(second_root, "params.yaml"))
  second <- run_vqa_analysis(context, seed = 20260810L)

  testthat::expect_named(first, c("context", "seed", "output_paths", "metadata"))
  testthat::expect_identical(first$metadata$seed, 20260810L)
  testthat::expect_identical(first$metadata$config, first$context$config)
  testthat::expect_equal(second$context$params$project, "vqa-demo2-min")
  testthat::expect_identical(second$metadata$project, "vqa-demo2-min")
  testthat::expect_identical(second$metadata$seed, 20260810L)
  testthat::expect_false(any(vapply(
    c("PROJ", "ASSESS", "RESULTSDIR", "EI.vec"),
    exists, logical(1),
    envir = .GlobalEnv, inherits = FALSE
  )))
})

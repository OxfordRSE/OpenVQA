fixture_root <- function(project) {
  repo_root <- normalizePath(file.path("..", ".."))
  project_root <- file.path(repo_root, "data", paste0(project, "-min"))
  testthat::expect_true(dir.exists(project_root))
  target <- tempfile(paste0("vqa-fixture-", project, "-"), tmpdir = tempdir())
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(list.files(project_root, full.names = TRUE), target, recursive = TRUE)
  testthat::expect_true(all(ok))
  target
}

run_fixture <- function(project, assessment) {
  root <- fixture_root(project)
  before <- tools::md5sum(list.files(file.path(root, assessment, "inputs"), full.names = TRUE))
  context <- load_preprocessing_context(root, assessment)
  context$output_dir <- file.path(root, assessment, "prepared")
  context <- prune_loaded_data_objects(context)
  context <- build_allowlists(context)
  context <- standardise_preprocessed_data(context)
  write_preprocessed_outputs(context)
  original_files <- names(before)
  after <- tools::md5sum(original_files)
  list(context = context, root = root, before = before, after = after)
}

expect_same_table <- function(x, y, tolerance = 1e-12) {
  testthat::expect_identical(names(x), names(y))
  testthat::expect_equal(x, y, tolerance = tolerance, check.attributes = FALSE)
}

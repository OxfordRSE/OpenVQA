legacy_project_name <- function(context) {
  project <- context$params$project
  if (is.null(project) || !nzchar(project)) {
    stop("params.yaml must declare project for legacy analysis.", call. = FALSE)
  }
  sub("-min$", "", project)
}

run_legacy_batch <- function(data_root, project, assessment, seed = NULL) {
  confirmation <- tempfile("vqa-confirm-")
  on.exit(unlink(confirmation), add = TRUE)
  writeLines("y", confirmation)

  environment <- c(
    paste0("VQA_DATA_ROOT=", normalizePath(data_root, mustWork = TRUE)),
    paste0("VQA_PROJECT=", project),
    paste0("VQA_ASSESSMENT=", assessment)
  )
  if (!is.null(seed)) {
    environment <- c(environment, paste0("VQA_TEST_SEED=", as.integer(seed)))
  }

  status <- system2(
    file.path(R.home("bin"), "Rscript"), "vqa.batch.R",
    env = environment, stdin = confirmation
  )
  if (!identical(status, 0L)) {
    stop("Legacy vqa.batch.R failed.", call. = FALSE)
  }
  invisible(TRUE)
}

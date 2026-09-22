# Shared file loading helpers for validation, preprocessing, and later stages.

check_dir_exist <- function(path) {
  dir.exists(path)
}

#' Load a YAML configuration file
#'
#' YAML parsing is shared infrastructure. Workflow code should interpret the
#' returned document, but should not duplicate path handling or package checks.
#'
#' @param file_path Path to the YAML file.
#' @param required Whether a missing file is an error.
#' @return Parsed YAML object, or NULL when an optional file is absent.
#' @keywords internal
load_yaml_file <- function(file_path, required = TRUE) {
  if (!file.exists(file_path)) {
    if (isTRUE(required)) {
      stop(sprintf("YAML file does not exist: %s", file_path), call. = FALSE)
    }
    return(NULL)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop(
      "Package 'yaml' is required to load YAML configuration files.",
      call. = FALSE
    )
  }
  yaml::read_yaml(file_path)
}

#' Load the project-level YAML configuration
#'
#' @param data_root Project data root.
#' @param file_name Configuration filename.
#' @param required Whether a missing configuration is an error.
#' @return List with `data`, `path`, and `exists` fields.
#' @keywords internal
load_project_config <- function(
  data_root,
  file_name = "params.yaml",
  required = FALSE
) {
  path <- file.path(data_root, file_name)
  list(
    data = load_yaml_file(path, required = required),
    path = path,
    exists = file.exists(path)
  )
}

resolve_assessment_params <- function(params, assessment) {
  if (is.null(params)) {
    return(list())
  }
  overrides <- if (is.null(assessment) || is.null(params$assessments)) {
    NULL
  } else {
    params$assessments[[assessment]]
  }
  params$assessments <- NULL
  if (is.null(overrides)) {
    return(params)
  }
  for (name in names(overrides)) params[[name]] <- overrides[[name]]
  params
}

list_assessments <- function(data_root, exclude_dirs = character()) {
  assessments <- list.dirs(data_root, full.names = FALSE, recursive = FALSE)
  setdiff(assessments, exclude_dirs)
}

#' Index CSV inputs by their basename
#'
#' Validation reports refer to contract filenames rather than absolute paths,
#' while loading still needs the absolute path. The named vector returned here
#' provides both without allowing unrelated file types into the input set.
#'
#' @param inputs_dir Assessment `inputs/` directory.
#' @return Named character vector mapping CSV basenames to full paths.
#' @keywords internal
build_file_map <- function(inputs_dir) {
  files <- list.files(inputs_dir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) {
    return(setNames(character(), character()))
  }
  setNames(files, basename(files))
}

read_csv_table <- function(file_path) {
  read.csv(
    file_path,
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = FALSE
  )
}

#' Load all existing files from an assessment input map
#'
#' Optional files must not cause loading to fail merely because they are
#' absent. Required-file failures are reported by the validation stages, so
#' this helper only loads files that actually exist.
#'
#' @param file_map Named path vector from [build_file_map()].
#' @return Named list of loaded data frames.
#' @keywords internal
load_assessment_inputs <- function(file_map) {
  present <- file_map[file.exists(file_map)]
  inputs <- lapply(present, read_csv_table)
  names(inputs) <- basename(present)
  inputs
}

load_input <- function(inputs, file_name) {
  inputs[[file_name]]
}

has_input <- function(inputs, file_name) {
  !is.null(inputs[[file_name]])
}

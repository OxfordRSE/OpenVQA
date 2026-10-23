# Capture the module location so defaults and the catalogue do not depend on getwd().
.params_root <- local({
  frames <- sys.frames()
  files <- lapply(frames, function(frame) {
    if (!is.null(frame$ofile)) frame$ofile else if (is.character(frame$file)) frame$file else NULL
  })
  files <- unlist(files)
  path <- utils::tail(files[grepl("(^|[/\\\\])params\\.R$", files)], 1L)
  normalizePath(file.path(dirname(path), ".."), mustWork = TRUE)
})

validate_params_schema <- function(params) {
  schema <- jsonlite::read_json(file.path(.params_root, "schema", "params.schema.json"))
  for (name in names(schema$properties)) {
    if (identical(schema$properties[[name]]$type, "array") && !is.null(params[[name]])) {
      params[[name]] <- I(params[[name]])
    }
  }
  jsonvalidate::json_validate(
    jsonlite::toJSON(params, auto_unbox = TRUE, null = "null"),
    jsonlite::toJSON(schema, auto_unbox = TRUE),
    engine = "ajv", error = TRUE
  )
  invisible(TRUE)
}

load_default_params <- function() {
  defaults <- load_yaml_file(file.path(.params_root, "params.yaml"))
  validate_resolved_params(defaults)
  validate_params_schema(defaults)
  defaults
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
  resolved <- resolve_params(load_default_params(), project = params, assessment = overrides)
  validate_params_schema(resolved)
  resolved
}

validate_resolved_params <- function(params) {
  if (!is.list(params) || (length(params) &&
      (is.null(names(params)) || anyNA(names(params)) ||
       any(!nzchar(names(params))) || anyDuplicated(names(params))))) {
    stop("Parameters must be a named list.", call. = FALSE)
  }
  reserved <- c("data_root", "assessment", "seed", "paths", "inputs_dir",
                "output_dir", "results_dir", "figures_dir", "log_dir")
  if (any(names(params) %in% reserved)) {
    stop("Runtime selection and derived paths cannot be configured in YAML.", call. = FALSE)
  }
  params
}

resolve_params <- function(defaults = list(), project = list(),
                           assessment = list(), explicit = list()) {
  resolved <- list()
  for (layer in list(defaults, project, assessment, explicit)) {
    if (is.null(layer)) next
    if (!is.list(layer)) stop("Parameters must be a named list.", call. = FALSE)
    layer$assessments <- NULL
    layer <- validate_resolved_params(layer)
    for (name in names(layer)) resolved[name] <- layer[name]
  }
  validate_resolved_params(resolved)
}

resolve_seed <- function(seed = NULL) {
  if (is.null(seed)) {
    fallback <- Sys.getenv("VQA_TEST_SEED", "")
    if (!nzchar(fallback)) return(NULL)
    seed <- fallback
  }
  if (length(seed) != 1L || is.na(seed) ||
      !(is.numeric(seed) || is.character(seed))) {
    stop("seed must be one non-missing integer.", call. = FALSE)
  }
  value <- suppressWarnings(as.numeric(seed))
  integer <- suppressWarnings(as.integer(value))
  if (!is.finite(value) || is.na(integer) || value != integer) {
    stop("seed must be one non-missing integer.", call. = FALSE)
  }
  integer
}

resolve_runtime_config <- function(data_root, assessment, seed = NULL,
                                   defaults = NULL, project = NULL,
                                   assessment_overrides = NULL, explicit = list()) {
  if (!is.character(data_root) || length(data_root) != 1L ||
      is.na(data_root) || !dir.exists(data_root)) {
    stop("data_root must be an existing directory.", call. = FALSE)
  }
  data_root <- normalizePath(data_root, mustWork = TRUE)
  if (!is.character(assessment) || length(assessment) != 1L ||
      is.na(assessment) || !grepl("^[[:alnum:]_-]+$", assessment)) {
    stop("assessment must be one directory name within data_root.", call. = FALSE)
  }
  assessment_dir <- file.path(data_root, assessment)
  if (!dir.exists(assessment_dir)) {
    stop("Assessment directory does not exist within data_root.", call. = FALSE)
  }
  assessment_dir <- normalizePath(assessment_dir, mustWork = TRUE)
  if (!identical(dirname(assessment_dir), data_root)) {
    stop("Assessment directory must belong to data_root.", call. = FALSE)
  }
  seed <- resolve_seed(seed)
  if (is.null(project)) project <- load_project_config(data_root)$data
  if (is.null(assessment_overrides)) assessment_overrides <- project$assessments[[assessment]]
  defaults <- resolve_params(load_default_params(), defaults)
  settings <- resolve_params(defaults, project, assessment_overrides, explicit)
  validate_params_schema(settings)
  paths <- list(inputs = file.path(assessment_dir, "inputs"),
                results = file.path(assessment_dir, "results"),
                figures = file.path(assessment_dir, "figs"),
                log = file.path(assessment_dir, "log"))
  list(config = c(list(data_root = data_root, assessment = assessment, seed = seed), settings),
       params = settings, paths = paths)
}


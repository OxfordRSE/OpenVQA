# Hash the same top-level CSV inventory the validator can load, including present optional files.
snapshot_validation_inputs <- function(inputs_dir) {
  if (dir.exists(inputs_dir) && file.access(inputs_dir, 5L) != 0L) {
    stop("Cannot read assessment input directory: ", inputs_dir, call. = FALSE)
  }
  files <- build_file_map(inputs_dir)
  files <- files[order(names(files), method = "radix")]
  hashes <- unname(tools::md5sum(files))
  if (anyNA(hashes)) stop("Cannot hash assessment input files.", call. = FALSE)
  list(
    captured_at = run_report_time(),
    files = data.frame(
      path = file.path("inputs", names(files)), hash = hashes,
      modified_at = run_report_time(file.info(files)$mtime), stringsAsFactors = FALSE
    )
  )
}

# Hash exact file bytes, including formatting; resolved defaults are not this identity.
snapshot_project_settings <- function(data_root) {
  path <- file.path(data_root, "params.yaml")
  hash <- if (file.exists(path)) unname(tools::md5sum(path)) else NULL
  if (anyNA(hash)) stop("Cannot hash project params.yaml.", call. = FALSE)
  list(
    path = "params.yaml", algorithm = "md5", hash = hash,
    captured_at = run_report_time(), stable = NULL
  )
}

validation_settings_summary <- function(assessments) {
  known <- vapply(assessments, function(child) {
    settings <- child$validation$settings
    !is.null(settings$captured_at) && isTRUE(settings$stable)
  }, logical(1))
  hashes <- vapply(assessments[known], function(child) {
    hash <- child$validation$settings$hash
    if (is.null(hash)) "<absent>" else hash
  }, character(1))
  if (!length(assessments)) {
    return(list(hashes_match = NULL, warning = NULL))
  }
  if (length(unique(hashes)) <= 1L && !all(known)) {
    return(list(
      hashes_match = NULL,
      warning = "Project params.yaml hashes are unavailable for some stored assessments."
    ))
  }
  match <- length(unique(hashes)) <= 1L
  list(
    hashes_match = match,
    warning = if (!match) {
      paste(
        "Assessment reports contain different project params.yaml hashes;",
        "some results describe earlier settings."
      )
    } else {
      NULL
    }
  )
}

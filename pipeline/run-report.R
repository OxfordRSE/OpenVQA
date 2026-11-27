# Sourced by validate.R so CLI workflows can share report metadata and safe publication.
# Capture the application path before callers change directories after sourcing the CLI.
.run_report_schema <- normalizePath(file.path("schema", "run-report.schema.json"), mustWork = TRUE)

run_report_time <- function(time = Sys.time()) {
  format(time, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

# This is an internal draft; completed reports are validated at return and export boundaries.
new_run_report <- function(operation, scope) {
  # Missing release metadata must not prevent a report from describing a validation failure.
  revision <- NULL
  if (file.exists(".git") && nzchar(Sys.which("git"))) {
    revision <- tryCatch(
      {
        value <- suppressWarnings(system2(
          "git", c("rev-parse", "--verify", "HEAD"),
          stdout = TRUE, stderr = FALSE
        ))
        if (is.null(attr(value, "status")) && length(value) == 1L) value else NULL
      },
      error = function(e) NULL
    )
  }
  version <- tryCatch(
    {
      value <- suppressWarnings(readLines("VERSION", n = 1L, warn = FALSE))
      if (length(value) == 1L && nzchar(value)) value else NULL
    },
    error = function(e) NULL
  )
  list(
    report_version = "2.1",
    report_type = "run",
    operation = operation,
    # tempfile supplies uniqueness without consuming the scientific random-number stream.
    run_id = paste0(
      format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "-",
      Sys.getpid(), "-", basename(tempfile("run-"))
    ),
    scope = scope,
    backend = list(
      version = version,
      revision = revision
    ),
    started_at = run_report_time(),
    ended_at = NULL,
    outcome = NULL,
    error = NULL
  )
}

prepare_report_destination <- function(path, overwrite = FALSE) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) || dir.exists(path)) {
    stop("Report destination must be a new file in an existing directory.", call. = FALSE)
  }
  path <- file.path(normalizePath(dirname(path), winslash = "/", mustWork = TRUE), basename(path))
  # Never follow a report symlink when replacement is explicitly requested.
  link <- Sys.readlink(path)
  if (!is.na(link) && nzchar(link)) {
    stop("Report destination already exists as a symlink.", call. = FALSE)
  }
  if (file.exists(path)) {
    if (!overwrite) {
      stop(paste(
        "Report destination already exists; no report was written.",
        "The existing file is not this invocation's report:", path
      ), call. = FALSE)
    }
    existing <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.list(existing) || !identical(existing$operation, "validate") ||
      length(existing$report_type) != 1L ||
      !existing$report_type %in% c("run", "assessment", "collection") ||
      !is.character(existing$report_version) || length(existing$report_version) != 1L) {
      stop("Refusing to overwrite a file that is not an OpenVQA validation report.", call. = FALSE)
    }
  }
  path
}

# As with parameter validation, reject schema violations before exposing a completed report.
validated_run_report_json <- function(report) {
  json <- jsonlite::toJSON(report,
    auto_unbox = TRUE, dataframe = "rows", null = "null", na = "null", pretty = TRUE
  )
  jsonvalidate::json_validate(json, .run_report_schema,
    engine = "ajv", strict = TRUE, error = TRUE
  )
  json
}

# Keep the read/merge/publish critical section short; validation itself needs no report lock.
with_report_lock <- function(path, code) {
  temporary <- tempfile(".openvqa-report-lock-", tmpdir = dirname(path))
  lock <- paste0(path, ".lock")
  on.exit(unlink(temporary), add = TRUE)
  writeLines(paste("pid", Sys.getpid(), run_report_time()), temporary)
  if (!isTRUE(suppressWarnings(file.link(temporary, lock)))) {
    stop("Could not acquire report lock: ", lock, call. = FALSE)
  }
  on.exit(unlink(lock), add = TRUE)
  force(code)
}

# Internal publication accepts only JSON already checked by validated_run_report_json().
.publish_run_report_json <- function(json, path, overwrite = FALSE) {
  path <- prepare_report_destination(path, overwrite = overwrite)
  previous <- if (file.exists(path)) unname(tools::md5sum(path)) else NULL
  if (anyNA(previous)) stop("Cannot read the existing report.", call. = FALSE)
  temporary <- tempfile(".openvqa-report-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  writeLines(json, temporary, useBytes = TRUE)
  if (overwrite) {
    # Refuse concurrent external edits; cooperating update/overwrite writers share the lock.
    current <- if (file.exists(path)) unname(tools::md5sum(path)) else NULL
    if (!identical(previous, current)) stop("Report changed before publication.", call. = FALSE)
    published <- suppressWarnings(file.rename(temporary, path))
  } else {
    # A hard link creates a new destination atomically without clobbering a concurrent writer.
    published <- suppressWarnings(file.link(temporary, path))
  }
  if (!isTRUE(published)) stop("Could not publish report: ", path, call. = FALSE)
  invisible(path)
}

write_run_report <- function(report, path, overwrite = FALSE) {
  # Revalidate edited objects before creating any temporary output or lock file.
  json <- validated_run_report_json(report)
  path <- prepare_report_destination(path, overwrite = overwrite)
  if (overwrite) {
    with_report_lock(path, .publish_run_report_json(json, path, overwrite = TRUE))
  } else {
    .publish_run_report_json(json, path)
  }
  invisible(path)
}

run_report_exit_status <- function(report) {
  switch(report$outcome,
    success = 0L,
    invalid_data = 1L,
    execution_error = 2L,
    stop("Unknown run-report outcome.", call. = FALSE)
  )
}

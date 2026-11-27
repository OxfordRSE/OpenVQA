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

prepare_report_destination <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) || dir.exists(path)) {
    stop("Report destination must be a new file in an existing directory.", call. = FALSE)
  }
  path <- file.path(normalizePath(dirname(path), winslash = "/", mustWork = TRUE), basename(path))
  # file.exists() misses dangling symlinks, which must also be protected from replacement.
  if (file.exists(path) || !is.na(Sys.readlink(path)) && nzchar(Sys.readlink(path))) {
    stop(paste(
      "Report destination already exists; no report was written.",
      "The existing file is not this invocation's report:", path
    ), call. = FALSE)
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

write_run_report <- function(report, path) {
  # Validate in memory before creating even a temporary file; callers may have edited the object.
  json <- validated_run_report_json(report)
  # Recheck because the destination may have appeared while validation was running.
  path <- prepare_report_destination(path)
  # Hard-link publication requires the temporary file to share the destination filesystem.
  temporary <- tempfile(".openvqa-report-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  writeLines(json, temporary, useBytes = TRUE)
  # A hard link publishes complete bytes atomically and refuses concurrent destination creation.
  if (!isTRUE(suppressWarnings(file.link(temporary, path)))) {
    stop(paste("Could not publish report without overwriting:", path), call. = FALSE)
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

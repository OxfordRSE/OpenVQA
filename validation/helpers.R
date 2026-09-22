# Shared helper functions for validation stages.

#' Test whether a named file exists in an input map
#'
#' A named path vector returns `NA` for an unknown name. Normalizing that case
#' here prevents every dependency check from needing its own defensive logic.
#'
#' @param file_map Named path vector.
#' @param file_name Contract filename.
#' @return A single logical value.
#' @keywords internal
is_present <- function(file_map, file_name) {
  !is.na(file_map[file_name]) && nzchar(file_map[file_name])
}

#' Build stable comparable signatures for key columns
#'
#' Base R joins are more expensive and less diagnostic for the small lookup
#' checks used here. A normalized signature lets the validator implement
#' membership and duplicate checks while preserving the original rows for
#' useful error details.
#'
#' @param df Data frame containing the keys.
#' @param keys Character vector of key columns.
#' @return Character vector with one signature per row.
#' @keywords internal
key_signature <- function(df, keys) {
  if (!length(keys)) {
    return(rep("__all__", nrow(df)))
  }

  cols <- lapply(df[keys], function(col) {
    ifelse(is.na(col), "<NA>", as.character(col))
  })
  do.call(paste, c(cols, sep = "||"))
}

#' Return rows in one table whose keys are absent from another
#'
#' This is the validator's lightweight foreign-key primitive. It reports the
#' offending child rows rather than only a count, so failures can identify the
#' unmatched keys.
#'
#' @param x Child data frame.
#' @param y Parent data frame.
#' @param by Key columns shared by both tables.
#' @return Subset of `x` without matching keys in `y`.
#' @keywords internal
anti_join_rows <- function(x, y, by) {
  x[!(key_signature(x, by) %in% unique(key_signature(y, by))), , drop = FALSE]
}

#' Return all rows participating in duplicate keys
#'
#' Returning both sides of each duplicate makes the resulting validation
#' details actionable; a single duplicated row would hide the conflicting
#' record that caused the failure.
#'
#' @param df Data frame to inspect.
#' @param keys Columns that should identify one row.
#' @return Rows whose key signature occurs more than once.
#' @keywords internal
duplicate_rows <- function(df, keys) {
  sig <- key_signature(df, keys)
  df[duplicated(sig) | duplicated(sig, fromLast = TRUE), , drop = FALSE]
}

#' Construct one normalized validation report row
#'
#' Both validation stages emit the same report shape so their results can be
#' combined into one assessment summary and filtered consistently for CI exit
#' status.
#'
#' @param assessment Assessment name.
#' @param stage Validation stage name.
#' @param check_id Stable check identifier.
#' @param severity Severity label.
#' @param status `pass` or `fail`.
#' @param message Human-readable result message.
#' @param file Related contract file.
#' @param details Optional diagnostic details.
#' @param extra_columns Optional schema extra-column report.
#' @return One-row data frame in the validation report format.
#' @keywords internal
validation_report_row <- function(
  assessment = NA_character_,
  stage,
  check_id,
  severity,
  status,
  message,
  file = NA_character_,
  details = NA_character_,
  extra_columns = NA_character_
) {
  data.frame(
    assessment = as.character(assessment),
    stage = as.character(stage),
    check_id = as.character(check_id),
    severity = as.character(severity),
    status = as.character(status),
    message = as.character(message),
    file = as.character(file),
    details = as.character(details),
    extra_columns = as.character(extra_columns),
    stringsAsFactors = FALSE
  )
}

#' Combine validation result fragments into one report
#'
#' Checks naturally return small reports or lists of reports. This function
#' removes empty fragments and guarantees that downstream summary code always
#' receives the same column structure.
#'
#' @param ... Report data frames or lists of report data frames.
#' @return Combined validation report data frame.
#' @keywords internal
bind_validation_reports <- function(...) {
  reports <- list(...)
  if (
    length(reports) == 1 &&
      is.list(reports[[1]]) &&
      !is.data.frame(reports[[1]])
  ) {
    reports <- reports[[1]]
  }
  reports <- Filter(
    function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0,
    reports
  )
  if (!length(reports)) {
    return(validation_report_row(
      assessment = character(),
      stage = character(),
      check_id = character(),
      severity = character(),
      status = character(),
      message = character(),
      file = character(),
      details = character(),
      extra_columns = character()
    ))
  }
  do.call(rbind, reports)
}

#' Count passes, failures, and warnings in a report
#'
#' Summary counts are kept separate from status filtering because warnings are
#' represented by severity, including warning-level informational checks that
#' have a passing status.
#'
#' @param report Validation report data frame.
#' @return List containing `pass`, `fail`, and `warn` integer counts.
#' @keywords internal
report_counts <- function(report) {
  if (is.null(report) || !nrow(report)) {
    return(list(pass = 0L, fail = 0L, warn = 0L))
  }
  list(
    pass = sum(report$status == "pass", na.rm = TRUE),
    fail = sum(report$status == "fail", na.rm = TRUE),
    warn = sum(report$severity == "warning", na.rm = TRUE)
  )
}

#' Print a validation report in compact or diagnostic form
#'
#' Compact output is suitable for normal command-line runs, while verbose
#' output exposes messages, details, and schema extra columns needed to diagnose
#' a failing assessment without rerunning individual checks manually.
#'
#' @param report Validation report data frame.
#' @param verbose Whether to print each check.
#' @param prefix Text prepended to each displayed line.
#' @return Invisibly `NULL`.
#' @keywords internal
print_validation_report <- function(report, verbose = FALSE, prefix = NULL) {
  if (is.null(report) || !nrow(report)) {
    return(invisible(NULL))
  }

  if (isTRUE(verbose)) {
    for (i in seq_len(nrow(report))) {
      label <- report$check_id[i]
      if (is.na(label) || !nzchar(label)) {
        label <- report$file[i]
      }
      line_prefix <- if (is.null(prefix)) "" else paste0(prefix, " ")
      cat(sprintf("%s[%s] %s\n", line_prefix, toupper(report$status[i]), label))

      if (!is.na(report$message[i]) && nzchar(report$message[i])) {
        cat(sprintf("%s  %s\n", line_prefix, report$message[i]))
      }
      if (!is.na(report$details[i]) && nzchar(report$details[i])) {
        cat(sprintf("%s  details: %s\n", line_prefix, report$details[i]))
      }
      if (!is.na(report$extra_columns[i]) && nzchar(report$extra_columns[i])) {
        cat(sprintf("%s  extra columns: %s\n", line_prefix, report$extra_columns[i]))
      }
    }
    return(invisible(NULL))
  }

  counts <- report_counts(report)
  cat(sprintf(
    "%s%d passed, %d failed, %d warnings\n",
    if (is.null(prefix)) "" else paste0(prefix, ": "),
    counts$pass,
    counts$fail,
    counts$warn
  ))

  failed <- report[report$status == "fail", , drop = FALSE]
  if (nrow(failed) > 0) {
    cat("  Failed checks:\n")
    for (i in seq_len(nrow(failed))) {
      cat(sprintf(
        "    - %s/%s: %s\n",
        ifelse(is.na(failed$stage[i]), "", failed$stage[i]),
        ifelse(is.na(failed$check_id[i]), "", failed$check_id[i]),
        failed$message[i]
      ))
    }
  }

  invisible(NULL)
}

#' Format key values for validation diagnostics
#'
#' Error messages need to identify representative offending records without
#' dumping entire input tables. This formatter creates compact, readable
#' `key=value` strings for that purpose.
#'
#' @param df Data frame containing key columns.
#' @param keys Columns to include.
#' @return Character vector with one formatted value per row.
#' @keywords internal
format_key_values <- function(df, keys) {
  if (!length(keys)) {
    return(character())
  }
  apply(df[keys], 1L, function(row) paste(sprintf("%s=%s", names(row), row), collapse = ", "))
}

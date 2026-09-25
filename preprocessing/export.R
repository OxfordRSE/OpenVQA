# Preprocessing step: write analysis-ready outputs.

write_preprocessed_outputs <- function(context) {
  # This will eventually be the final step of the preprocessing pipeline.
  # It should write the standardised tables to the assessment inputs folder or
  # to a clearly separated derived-data location.
  write_preprocessed_table_set(context)
  invisible(context)
}

write_preprocessed_table_set <- function(context) {
  output_dir <- context$output_dir %||% context$inputs_dir
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  tables <- context$inputs
  if (!is.null(context$allowlists)) {
    tables[["ei.stratum.include.csv"]] <- context$allowlists$blocklist
    tables[["ei.stratum.veg.include.csv"]] <- context$allowlists$allowlist
  }
  for (file_name in names(tables)) {
    if (is.null(tables[[file_name]]) || !is.data.frame(tables[[file_name]])) next
    utils::write.csv(
      tables[[file_name]],
      file = file.path(output_dir, file_name),
      row.names = FALSE, na = "NA"
    )
  }
  invisible(context)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

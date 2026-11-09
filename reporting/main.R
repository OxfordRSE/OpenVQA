source(file.path("analysis", "quality-summary.R"), local = TRUE)
source(file.path("reporting", "summary-tables.R"), local = TRUE)
source(file.path("reporting", "workbook-tables.R"), local = TRUE)
source(file.path("reporting", "workbook.R"), local = TRUE)

write_summary_tables <- function(tables, directory, na = "NA") {
  for (name in names(tables)) {
    write.csv(tables[[name]], file = file.path(directory, name), row.names = FALSE, na = na)
  }
  invisible(tables)
}

write_assessment_reports <- function(config, output_paths) {
  summary <- build_quality_summary(config, output_paths)
  write_summary_tables(summary, output_paths$results)
  # Preserve the execution date recorded from the overall-quality CSV.
  analysis_time <- file.info(file.path(output_paths$results, "summary_focal_q_qh.csv"))$mtime
  # Retain the column types previously inferred when reading the summary CSVs.
  summary <- lapply(summary, function(table) type.convert(table, as.is = TRUE))
  indicator_details <- read.csv(file.path(output_paths$results, "ei.summary.csv"))
  write_summary_tables(
    build_summary_exports(summary, indicator_details), output_paths$results,
    na = ""
  )
  inputs <- load_assessment_inputs(build_file_map(output_paths$input))
  sheets <- build_workbook_tables(summary, inputs, config, analysis_time)
  write_summary_workbook(sheets, file.path(output_paths$results, "vqa_summary.xlsx"))
  invisible(summary)
}

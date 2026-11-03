add_summary_sheet <- function(workbook, name, table) {
  workbook <- openxlsx2::wb_add_worksheet(workbook, sheet = name)
  workbook <- openxlsx2::wb_add_data(workbook, name, table)
  workbook$add_font(name, "A1:Z500", name = "Arial")
  workbook$add_font(name, "A1:Z1", name = "Arial", bold = TRUE)
  xl.col.wj(curr.wb = workbook, curr.ws = name, curr.df = table)
  for (column in intersect(names(table), c(
    "Q [Lower, Upper 95% CLs]", "QH [Lower, Upper 95% CLs]"
  ))) {
    letter <- xlcolconv(match(column, names(table)))
    workbook$add_cell_style(
      sheet = name, dims = paste0(letter, "1:", letter, "500"), horizontal = "center"
    )
  }
  workbook
}

write_summary_workbook <- function(sheets, path) {
  workbook <- openxlsx2::wb_workbook()
  workbook <- openxlsx2::wb_add_worksheet(workbook, sheet = "Meta")
  metadata <- sheets$Meta
  workbook <- openxlsx2::wb_add_data(workbook, "Meta", metadata, col_names = FALSE)
  # Metadata has bold labels and plain values, without a table header.
  workbook$add_font("Meta", "A1:A100", name = "Arial", bold = TRUE)
  workbook$add_font("Meta", "B1:B100", name = "Arial")
  workbook$set_col_widths(sheet = "meta", cols = 1, widths = max(nchar(metadata$head)) + 2)
  workbook$set_col_widths(sheet = "meta", cols = 2, widths = max(nchar(metadata$value)) + 2)
  for (name in setdiff(names(sheets), "Meta")) {
    workbook <- add_summary_sheet(workbook, name, sheets[[name]])
  }
  workbook$save(path)
  invisible(path)
}

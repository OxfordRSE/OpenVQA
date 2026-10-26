fixed_quality_tables <- function(inputs, config) {
  if (!config$qh_method %in% c("assume.0", "assume.1")) {
    stop("Invalid fixed-quality method: ", config$qh_method, call. = FALSE)
  }
  land_cover <- inputs[["landCover.csv"]]
  if (!is.data.frame(land_cover) || !all(c("vegClass", "area_ha") %in% names(land_cover))) {
    stop("Fixed quality requires landCover.csv with vegClass and area_ha.", call. = FALSE)
  }
  quality <- if (config$qh_method == "assume.0") 0 else 1
  land_cover$area_ha <- as.numeric(land_cover$area_ha)
  land_cover$area_ha[is.na(land_cover$area_ha)] <- 0
  benchmark <- aggregate(area_ha ~ vegClass, data = land_cover, FUN = sum)
  benchmark$qh <- benchmark$area_ha * quality
  benchmark$qh.lcl <- benchmark$area_ha * quality
  benchmark$qh.ucl <- benchmark$area_ha * quality
  names(benchmark)[names(benchmark) == "vegClass"] <- "bm.veg"
  benchmark$notes <- ""
  benchmark <- benchmark[, c("bm.veg", "area_ha", "qh", "qh.lcl", "qh.ucl", "notes")]
  bootstrap <- benchmark[!is.na(benchmark$qh), "bm.veg", drop = FALSE]
  columns <- data.frame(matrix(quality, nrow = nrow(bootstrap), ncol = config$bootstrap_reps))
  columns[] <- lapply(columns, as.numeric)
  bootstrap <- cbind(bootstrap, columns)
  list(benchmark = benchmark, bootstrap = bootstrap)
}

run_fixed_quality <- function(inputs, config, output_paths) {
  tables <- fixed_quality_tables(inputs, config)
  cat("Running fixed-quality assessment: ", config$qh_method, "\n", sep = "")
  readr::write_excel_csv(tables$benchmark, file.path(
    output_paths$results,
    "summary_focal_qh_bm.csv"
  ))
  readr::write_excel_csv(tables$bootstrap, file.path(
    output_paths$results,
    "summary_qh_boot_bm.csv"
  ))
  invisible(tables)
}

indicator_parameters <- function(code, config) {
  settings <- config$indicator_settings[[code]]
  if (is.null(settings)) stop("Unknown indicator: ", code, call. = FALSE)
  settings <- resolve_params(config$indicator_defaults, settings)
  if (is.null(settings$benchmark_value)) settings$benchmark_value <- NA
  if (is.null(settings$td_multiplier)) settings$td_multiplier <- NA
  omit <- unlist(settings$td_multiplier_omit, use.names = FALSE)
  settings$td_multiplier_omit <- if (length(omit)) omit else ""
  if (is.null(settings$data_type)) {
    settings$data_type <- if (code %in% c("SR", "TD", "TS")) config$data_type else "cover"
  }
  tables <- c(
    SR = "speciesCover", TD = "speciesCover", TS = "speciesCover",
    PCGF = "coverByGrowthForm", GC = "groundCover", PCESS = "exoticCoverByStratum",
    PCES = "exoticCoverByStratum", BA = "speciesStems", ASC = "speciesStems",
    PCS = "coverByStratum", CH = "speciesCover", BAES = "speciesStems"
  )
  settings$input_table <-
    if (settings$data_type == "ind") "speciesStems" else unname(tables[[code]])
  settings$source_file <- paste0(settings$input_table, ".csv")
  settings
}

# Convert DBH in cm to basal area in m2, normalized by diameter-specific plot areas.
scaled_basal_area <- function(dbh, config, target_area_m2 = 10000) {
  sampling <- config$stem_sampling
  dplyr::case_when(
    dbh >= sampling$min_dbh_cm & dbh < sampling$split_dbh_cm ~
      pi * ((dbh / 2)^2) / 10000 * target_area_m2 / sampling$small_plot_area_m2,
    dbh >= sampling$split_dbh_cm ~
      pi * ((dbh / 2)^2) / 10000 * target_area_m2 / sampling$large_plot_area_m2,
    is.na(dbh) ~ NA,
    .default = NA
  )
}

stem_test_config <- function() {
  resolve_params(load_default_params(), list(
    project = "stem-test", data_type = "mixed", bootstrap_reps = 5, permutation_reps = 19,
    active_indicators = list("BA", "ASC"), aggregate_indicators = list("ASC"),
    indicator_groups = list(BA = "structure", ASC = "structure"),
    indicator_settings = list(ASC = list(distribution = "gamma")),
    stem_sampling = list(
      min_dbh_cm = 2.5, split_dbh_cm = 10, small_plot_area_m2 = 48, large_plot_area_m2 = 192
    ),
    plot_hist = FALSE, plot_overlap = FALSE, remove_ba_outliers = FALSE
  ))
}

stem_test_inputs <- function() {
  plots <- data.frame(
    plotCode = c("b1", "b2", "f1", "f2", "f3"),
    focalOrBenchmark = c("b", "b", "f", "f", "f"),
    landCover = c("forest", "forest", "focal", "focal", "focal"), vegClass = "forest"
  )
  stems <- plots[c(1, 1, 1, 2, 3, 4), ]
  stems$species <- c("native", "native", "native", "exotic", "native", "unknown")
  stems$dbh_cm <- c(2.49, 2.5, 9.999, 10, 11, NA)
  stems$ba_m2 <- pi * (stems$dbh_cm / 2)^2 / 10000
  stems$ind_id <- c("a", "a", "b", "a", "a", "a")
  stems$stratum <- c(rep("Stems 2.5 to <10 cm DBH", 3), rep("Stems >=10 cm DBH", 3))
  include <- data.frame(
    EI = "ASC", stratum = unique(stems$stratum), f.group = "structure", include = TRUE
  )
  exceptions <- data.frame(
    EI = character(), stratum = character(), bm.vegetation = character(), include = logical()
  )
  list(
    plotMetadata.csv = plots, speciesStems.csv = stems,
    species.csv = data.frame(species = c("native", "exotic"), is_exotic = c(0, 1)),
    ei.stratum.include.csv = include, ei.stratum.veg.include.csv = exceptions
  )
}

taxonomic_test_config <- function() {
  resolve_params(load_default_params(), list(
    project = "taxonomic-test", data_type = "cover", bootstrap_reps = 5,
    permutation_reps = 19, active_indicators = list("TS", "TD"), aggregate_indicators = list(),
    indicator_groups = list(TS = "composition", TD = "composition"),
    plot_hist = FALSE, plot_overlap = FALSE, nodata_sd = 4,
    nmds = list(dimensions = 2, trymax = 20, max_iterations = 20)
  ))
}

taxonomic_test_inputs <- function() {
  plots <- data.frame(
    plotCode = c("b1", "b2", "b3", "f1", "f2"),
    focalOrBenchmark = c("b", "b", "b", "f", "f"),
    landCover = c("forest", "forest", "forest", "focal", "focal"), vegClass = "forest"
  )
  cover <- plots[c(1, 1, 1, 2, 2, 3, 4, 4), ]
  cover$species <- c("A", "B", "A", "A", "exotic", "exotic", "B", "unknown")
  cover$cover <- c(0, 0.3, 0.2, 1, 1, 1, 0.5, 0.1)
  list(
    plotMetadata.csv = plots, speciesCover.csv = cover,
    species.csv = data.frame(species = c("A", "B", "exotic"), is_exotic = c(0, 0, 1))
  )
}

taxonomic_ordination_matrix <- function() {
  matrix <- outer(1:16, 1:8, function(i, j) (i * j + i^2 + j^2) %% 19 + 1)
  dimnames(matrix) <- list(paste0("p", 1:16), LETTERS[1:8])
  matrix
}

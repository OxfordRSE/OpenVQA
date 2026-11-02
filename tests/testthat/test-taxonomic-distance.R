testthat::test_that("TD preparation keeps ordination and outlier objects local across calls", {
  config <- taxonomic_test_config()
  config$remove_td_outliers <- FALSE
  config$td_outlier_upper_threshold <- 0
  matrix <- taxonomic_ordination_matrix()
  plots <- data.frame(
    plotCode = rownames(matrix), focalOrBenchmark = rep(c("b", "f"), each = 8),
    landCover = rep(c("forest", "focal"), each = 8), vegClass = "forest"
  )
  occurrences <- expand.grid(
    plotCode = rownames(matrix), species = colnames(matrix),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  cover <- plots[match(occurrences$plotCode, plots$plotCode), ]
  cover$species <- occurrences$species
  cover$cover <- as.vector(matrix) / max(matrix)
  missing <- plots[16, ]
  missing$plotCode <- "f0"
  inputs <- list(speciesCover.csv = cover, plotMetadata.csv = rbind(plots, missing))
  original <- inputs
  directory <- tempfile("td-preparation-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  paths_a <- list(results = file.path(directory, "a"), figures = file.path(directory, "figs"))
  paths_b <- list(results = file.path(directory, "b"), figures = file.path(directory, "figs"))
  dir.create(paths_a$results)
  dir.create(paths_b$results)
  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(directory)
  writeLines("caller-owned", "spCov.txt")
  rlang::local_bindings(
    source = function(...) stop("Execution must not source scripts"),
    sys.source = function(...) stop("Execution must not source scripts"),
    INPUTDIR = "missing-input-directory", RESULTSDIR = "missing-result-directory",
    spCov.mds = "caller-owned", td.outliers = "caller-owned", .env = globalenv()
  )
  first <- prepare_taxonomic_distance(inputs, config, paths_a, seed = 42L)
  testthat::expect_equal(nrow(first), 17)
  testthat::expect_identical(unique(stats::na.omit(first$dim)), 2)
  testthat::expect_true(first$no_data_plot[first$plotCode == "f0"])
  testthat::expect_true(file.exists(file.path(paths_a$results, "TD_raw.bak.csv")))
  testthat::expect_true(file.exists(file.path(paths_a$results, "TD_outliers.csv")))
  other <- config
  other$nmds$dimensions <- 3L
  other$td_outlier_upper_threshold <- 10000
  second <- prepare_taxonomic_distance(inputs, other, paths_b, seed = 42L)
  testthat::expect_identical(unique(stats::na.omit(second$dim)), 3L)
  testthat::expect_false(any(second$outlier, na.rm = TRUE))
  testthat::expect_false(file.exists(file.path(paths_b$results, "TD_outliers.csv")))
  testthat::expect_equal(prepare_taxonomic_distance(inputs, config, paths_a, seed = 42L), first)
  testthat::expect_identical(readLines("spCov.txt"), "caller-owned")
  testthat::expect_identical(get("spCov.mds", globalenv()), "caller-owned")
  testthat::expect_identical(get("td.outliers", globalenv()), "caller-owned")
  testthat::expect_identical(inputs, original)
  testthat::expect_error(prepare_taxonomic_distance(list(), config, paths_a), "requires")
})

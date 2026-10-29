testthat::test_that("taxonomic similarity preserves native presence and pair ordering", {
  inputs <- taxonomic_test_inputs()
  original <- inputs
  config <- taxonomic_test_config()
  set.seed(42)
  before <- .Random.seed
  raw <- taxonomic_similarity_data(inputs, config)
  testthat::expect_identical(.Random.seed, before)
  testthat::expect_identical(raw$plotCode, c(
    "b1_b2", "b1_b3", "b2_b3", "f1_b1", "f1_b2", "f1_b3", "f2_b1", "f2_b2", "f2_b3"
  ))
  testthat::expect_equal(raw$EI, c(2 / 3, 0, 0, 0.5, 0, 0, 0, 0, 0))
  testthat::expect_identical(raw$EI.tr, raw$EI)
  testthat::expect_identical(raw$stratum, rep("nostrata", 9))
  testthat::expect_identical(inputs, original)
  zero_cover <- inputs
  zero_cover$speciesCover.csv$cover[c(1, 3)] <- 0
  testthat::expect_equal(taxonomic_similarity_data(zero_cover, config), raw)
  inputs$speciesStems.csv <- inputs$speciesCover.csv[, c(
    "plotCode", "focalOrBenchmark", "landCover", "vegClass", "species"
  )]
  config$indicator_settings$TS$data_type <- "ind"
  testthat::expect_equal(taxonomic_similarity_data(inputs, config), raw)
  config$missing_exotic_status_assume_native <- FALSE
  testthat::expect_error(taxonomic_similarity_data(inputs, config), "is_exotic")
  testthat::expect_error(taxonomic_similarity_data(list(), config), "requires")
})

testthat::test_that("taxonomic similarity executes directly and reuses prepared pair tables", {
  config <- taxonomic_test_config()
  config$seed <- 42L
  config$force_prepare_raw <- TRUE
  inputs <- taxonomic_test_inputs()
  directory <- tempfile("ts-results-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  paths <- list(results = directory, figures = file.path(directory, "figs"))
  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(tempdir())
  rlang::local_bindings(
    source = function(...) stop("Execution must not source scripts"),
    sys.source = function(...) stop("Execution must not source scripts"),
    INPUTDIR = "missing-input-directory", RESULTSDIR = "missing-result-directory",
    .env = globalenv()
  )
  first <- run_taxonomic_similarity(inputs, config, paths)
  testthat::expect_equal(first$raw$EI, c(2 / 3, 0, 0, 0.5, 0, 0, 0, 0, 0))
  testthat::expect_equal(read.csv(file.path(directory, "TS_focal.csv")), first$focal)
  config$force_prepare_raw <- FALSE
  testthat::expect_equal(run_taxonomic_similarity(list(), config, paths), first)
  config$plot_figures_only <- TRUE
  unlink(file.path(directory, "TS_benchmark.csv"))
  testthat::expect_null(run_taxonomic_similarity(list(), config, paths))
  testthat::expect_false(dir.exists(paths$figures))
})

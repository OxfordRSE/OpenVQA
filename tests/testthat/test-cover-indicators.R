cover_test_config <- function() {
  config <- resolve_params(load_default_params(), list(
    project = "cover-test", data_type = "cover", bootstrap_reps = 5,
    active_indicators = list("PCGF", "GC", "PCESS", "PCES"),
    aggregate_indicators = list(), indicator_groups = list(
      PCGF = "structure", GC = "structure", PCESS = "composition", PCES = "composition"
    ),
    plot_hist = FALSE, plot_overlap = FALSE, include_plots_nodata = FALSE
  ))
  for (indicator in config$active_indicators) {
    config$indicator_settings[[indicator]]$na_to_zero <- TRUE
  }
  config
}

cover_test_inputs <- function() {
  plots <- data.frame(
    plotCode = c("b1", "b2", "f1", "f2", "f3"),
    focalOrBenchmark = c("b", "b", "f", "f", "f"),
    landCover = c("forest", "forest", "focal", "focal", "focal"),
    vegClass = c("forest", "forest", "forest", "grass", "forest")
  )
  cover <- plots[c(1, 1, 2, 3, 4), ]
  cover$stratum <- c("tree", "herb", "tree", "herb", "tree")
  cover$cover <- c(NA, 0.3, 2, 0.4, 0.2)
  include <- expand.grid(
    EI = c("PCGF", "GC", "PCESS"), stratum = c("tree", "herb"),
    stringsAsFactors = FALSE
  )
  include$f.group <- "structure"
  include$include <- include$stratum == "tree"
  exceptions <- data.frame(
    EI = c("PCGF", "GC", "PCESS"), stratum = "herb", bm.vegetation = "forest", include = TRUE
  )
  list(
    plotMetadata.csv = plots, coverByGrowthForm.csv = cover, groundCover.csv = cover,
    exoticCoverByStratum.csv = cover,
    ei.stratum.include.csv = include, ei.stratum.veg.include.csv = exceptions
  )
}

testthat::test_that("cover strata retain exceptions, fill missing strata, and truncate cover", {
  inputs <- cover_test_inputs()
  original <- inputs
  config <- cover_test_config()
  for (prepare in list(pcgf_data, gc_data, pcess_data)) {
    raw <- prepare(inputs, config)
    testthat::expect_identical(names(raw), c(
      "plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI", "EI.tr"
    ))
    values <- setNames(raw$EI, paste(raw$plotCode, raw$stratum))
    testthat::expect_equal(unname(values[c("b1 tree", "b2 tree", "f1 tree")]), c(0, 1, 0))
    testthat::expect_equal(unname(values[c("b1 herb", "f1 herb", "b2 herb")]), c(0.3, 0.4, 0))
    testthat::expect_false("f2 herb" %in% names(values))
    testthat::expect_identical(raw$EI.tr, raw$EI)
    testthat::expect_error(prepare(list(), config), "requires")
  }
  config$indicator_settings$PCGF$convert_percent <- TRUE
  raw <- pcgf_data(inputs, config)
  testthat::expect_equal(raw$EI[raw$plotCode == "b2" & raw$stratum == "tree"], 0.02)
  testthat::expect_identical(inputs, original)
})

testthat::test_that("cover preparation keeps indicator-specific missing-value behavior", {
  inputs <- cover_test_inputs()
  config <- cover_test_config()
  config$indicator_settings$PCGF$remove_zero_cover_plots <- TRUE
  inputs$coverByGrowthForm.csv$cover[1:2] <- 0
  testthat::expect_false("b1" %in% pcgf_data(inputs, config)$plotCode)
  inputs$groundCover.csv$cover[inputs$groundCover.csv$plotCode == "b2"] <- NA
  testthat::expect_false("b2" %in% gc_data(inputs, config)$plotCode)
  inputs$exoticCoverByStratum.csv$cover[inputs$exoticCoverByStratum.csv$plotCode == "b2"] <- NA
  config$include_plots_nodata <- FALSE
  config$indicator_settings$PCESS$na_to_zero <- FALSE
  testthat::expect_false("b2" %in% pcess_data(inputs, config)$plotCode)
})

testthat::test_that("total exotic cover preserves summed rows and adds missing plots", {
  inputs <- cover_test_inputs()
  original <- inputs
  config <- cover_test_config()
  config$include_plots_nodata <- TRUE
  raw <- pces_data(inputs, config)
  testthat::expect_equal(raw$EI[raw$plotCode == "b1"], c(0.3, 0.3))
  testthat::expect_equal(raw$EI[raw$plotCode == "b2"], 2)
  testthat::expect_equal(raw$EI[raw$plotCode == "f3"], 0)
  testthat::expect_identical(raw$stratum, rep("nostrata", nrow(raw)))
  testthat::expect_identical(raw$EI.tr, raw$EI)
  config$indicator_settings$PCES$na_to_zero <- FALSE
  testthat::expect_equal(sum(pces_data(inputs, config)$plotCode == "b1"), 1)
  config$include_plots_nodata <- FALSE
  testthat::expect_false("f3" %in% pces_data(inputs, config)$plotCode)
  testthat::expect_error(pces_data(list(), config), "requires")
  testthat::expect_identical(inputs, original)
})

testthat::test_that("cover workflows run independently and reuse raw tables", {
  inputs <- cover_test_inputs()
  config <- cover_test_config()
  config$force_prepare_raw <- TRUE
  config$seed <- 42L
  inputs$plotMetadata.csv$vegClass <- "forest"
  for (name in c("coverByGrowthForm.csv", "groundCover.csv", "exoticCoverByStratum.csv")) {
    inputs[[name]]$vegClass <- "forest"
    inputs[[name]]$cover <- 0
  }
  workflows <- list(PCGF = run_pcgf, GC = run_gc, PCESS = run_pcess, PCES = run_pces)
  directory <- tempfile("cover-results-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(tempdir())
  rlang::local_bindings(
    source = function(...) stop("Calculation must not source scripts"),
    sys.source = function(...) stop("Calculation must not source scripts"),
    INPUTDIR = "missing-input-directory", RESULTSDIR = "missing-result-directory",
    .env = globalenv()
  )
  paths <- list(results = directory, figures = file.path(directory, "figs"))
  for (indicator in names(workflows)) {
    workflow <- workflows[[indicator]]
    result <- workflow(inputs, config, paths, seed = 42L)
    testthat::expect_equal(result$raw$EI, rep(0, nrow(result$raw)))
    testthat::expect_equal(result$focal$q, rep(1, nrow(result$focal)))
    for (table in c("raw", "focal", "benchmark")) {
      filename <- file.path(directory, paste0(indicator, "_", table, ".csv"))
      testthat::expect_equal(read.csv(filename), result[[table]])
    }
    reuse <- config
    reuse$force_prepare_raw <- FALSE
    reuse$indicator_settings[[indicator]]$prepare_raw <- FALSE
    testthat::expect_equal(workflow(list(), reuse, paths), result)
    reuse$plot_figures_only <- TRUE
    unlink(file.path(directory, paste0(indicator, "_benchmark.csv")))
    testthat::expect_null(workflow(list(), reuse, paths))
  }
  testthat::expect_false(dir.exists(paths$figures))
})

testthat::test_that("stem execution is independent and preserves explicit seed behavior", {
  config <- stem_test_config()
  config$force_prepare_raw <- FALSE
  config$seed <- 7L
  directory <- tempfile("stem-execution-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  paths <- list(results = directory, figures = file.path(directory, "figs"))
  values <- c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  raw <- data.frame(
    plotCode = paste0(rep(c("b", "f"), each = 10), rep(1:10, 2)),
    focalOrBenchmark = rep(c("b", "f"), each = 10),
    landCover = rep(c("forest", "focal"), each = 10),
    vegClass = "forest", stratum = "nostrata", EI = c(values + 1, values)
  )
  raw$EI.tr <- raw$EI
  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(tempdir())
  rlang::local_bindings(
    source = function(...) stop("Calculation must not source scripts"),
    sys.source = function(...) stop("Calculation must not source scripts"),
    INPUTDIR = "missing-input-directory", RESULTSDIR = "missing-result-directory",
    seed = -1L, .env = globalenv()
  )
  workflows <- list(BA = run_basal_area, ASC = run_stem_abundance)
  for (indicator in names(workflows)) {
    workflow <- workflows[[indicator]]
    raw$stratum <- if (indicator == "BA") "nostrata" else "Stems >=10 cm DBH"
    write.csv(raw, file.path(directory, paste0(indicator, "_raw.csv")), row.names = FALSE)
    first <- workflow(list(), config, paths, seed = 42L)
    bootstrap_file <- file.path(directory, paste0(indicator, "_boot.q.csv"))
    bootstrap <- read.csv(bootstrap_file)
    testthat::expect_equal(first$raw, raw)
    testthat::expect_true(all(is.finite(first$focal$q)))
    testthat::expect_equal(workflow(list(), config, paths, seed = 42L), first)
    testthat::expect_identical(read.csv(bootstrap_file), bootstrap)
    workflow(list(), config, paths, seed = 43L)
    testthat::expect_false(identical(read.csv(bootstrap_file), bootstrap))
    figures_config <- config
    figures_config$plot_figures_only <- TRUE
    unlink(file.path(directory, paste0(indicator, "_benchmark.csv")))
    testthat::expect_null(workflow(list(), figures_config, paths))
  }
  testthat::expect_false(dir.exists(paths$figures))
})

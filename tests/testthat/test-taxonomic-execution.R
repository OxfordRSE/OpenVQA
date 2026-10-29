testthat::test_that("taxonomic quality calls preserve explicit seeds and raw-file reuse", {
  config <- taxonomic_test_config()
  config$force_prepare_raw <- FALSE
  config$seed <- 7L
  directory <- tempfile("taxonomic-execution-")
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
    seed = -1L, spCov.mds = "caller-owned", .env = globalenv()
  )
  workflows <- list(TS = run_taxonomic_similarity, TD = run_taxonomic_distance)
  for (indicator in names(workflows)) {
    workflow <- workflows[[indicator]]
    values <- if (indicator == "TS") {
      seq(0.1, 0.8, length.out = 10)
    } else {
      c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
    }
    benchmark <- values + if (indicator == "TS") 0.05 else 1
    raw <- data.frame(
      plotCode = paste0(rep(c("b", "f"), each = 10), rep(1:10, 2)),
      focalOrBenchmark = rep(c("b", "f"), each = 10),
      landCover = rep(c("forest", "focal"), each = 10),
      vegClass = "forest", stratum = "nostrata", EI = c(benchmark, values)
    )
    raw$EI.tr <- raw$EI
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
  testthat::expect_identical(get("spCov.mds", globalenv()), "caller-owned")
})

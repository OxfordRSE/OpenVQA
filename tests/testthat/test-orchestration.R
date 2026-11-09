testthat::test_that("indicators dispatch in configured order and summary-only skips them", {
  calls <- character()
  rlang::local_bindings(
    run_pcs = function(...) calls <<- c(calls, "PCS"),
    run_canopy_height = function(...) calls <<- c(calls, "CH"),
    run_exotic_basal_area = function(...) calls <<- c(calls, "BAES"),
    .env = environment(run_analysis_indicators)
  )
  config <- list(
    summary_only = FALSE, td_only = FALSE,
    active_indicators = c("PCS", "CH", "BAES")
  )
  run_analysis_indicators(list(), config, list())
  testthat::expect_identical(calls, config$active_indicators)
  config$summary_only <- TRUE
  run_analysis_indicators(list(), config, list())
  testthat::expect_length(calls, 3)
  config$summary_only <- FALSE
  config$active_indicators <- "unknown"
  testthat::expect_error(run_analysis_indicators(list(), config, list()), "Unknown indicator")
})

testthat::test_that("TD-only dispatch overrides the configured indicator list", {
  calls <- character()
  rlang::local_bindings(
    run_taxonomic_distance = function(...) calls <<- c(calls, "TD"),
    .env = environment(run_analysis_indicators)
  )
  run_analysis_indicators(list(), list(summary_only = FALSE, td_only = TRUE), list())
  testthat::expect_identical(calls, "TD")
})

testthat::test_that("failed analyses restore caller sinks and connections", {
  caller <- file(tempfile(), open = "w")
  sink(caller)
  on.exit(
    {
      sink()
      close(caller)
    },
    add = TRUE
  )
  depth <- sink.number()
  connections <- showConnections(all = TRUE)
  config <- list(replace_log = TRUE, logfile_basename = "analysis-test")
  testthat::expect_error(with_analysis_log(config, list(log = tempdir()), {
    stop("Calculation failed")
  }), "Calculation failed")
  testthat::expect_identical(sink.number(), depth)
  testthat::expect_identical(showConnections(all = TRUE), connections)
})

testthat::test_that("required inputs are checked before indicator dispatch", {
  config <- list(qh_method = "assume.1")
  testthat::expect_error(check_analysis_inputs(list(), config), "landCover.csv")
  inputs <- list(landCover.csv = data.frame())
  testthat::expect_no_error(check_analysis_inputs(inputs, config))
  config <- resolve_params(load_default_params(), list(
    active_indicators = "SR",
    data_type = "cover"
  ))
  config$qh_method <- "empirical"
  testthat::expect_error(check_analysis_inputs(inputs, config), "plotMetadata.csv")
})

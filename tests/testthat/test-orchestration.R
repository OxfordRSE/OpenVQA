orchestration_environment <- function() {
  run_env <- list2env(list(
    QH.METHOD = "empirical", VQA.SUMMARY.ONLY = FALSE,
    PLOT.FIGS.ONLY = FALSE, TD.ONLY = FALSE, EI.vec = c("TS", "SR"),
    REPLACE.LOG = TRUE, LOGDIR = tempdir(),
    LOGFILE.BASENAME = basename(tempfile("orchestration-")),
    RESULTSDIR = "results", FIGDIR = "figs",
    INPUTDIR = tempdir(), DF.LANDCOVER = character(),
    DF.PLOTMETADATA = "plotMetadata", DF.SPECIES = "species",
    input_file_list = function(...) character(), stop_quietly = base::stop,
    gmean = base::mean, generalized_mean = base::mean,
    scripts = character(), indicator_order = character()
  ))
  run_env$source <- function(file, ...) {
    run_env$scripts <- c(run_env$scripts, file)
    if (dirname(file) == "indicators") {
      run_env$indicator_order <- c(run_env$indicator_order, run_env$EI)
    }
  }
  run_env
}

testthat::test_that("summary-only runs skip indicators and keep summary script order", {
  run_env <- orchestration_environment()
  run_env$VQA.SUMMARY.ONLY <- TRUE
  execute_legacy_batch(run_env)
  testthat::expect_identical(run_env$scripts, c(
    "libraries.R", "vqa.summary.R",
    file.path("vqa.summary", "vqa.summary.csv.R"),
    file.path("vqa.summary", "vqa.summary.xl.R")
  ))
})

testthat::test_that("TD-only and figures-only runs suppress summaries", {
  run_env <- orchestration_environment()
  run_env$TD.ONLY <- TRUE
  execute_legacy_batch(run_env)
  testthat::expect_identical(run_env$scripts, c("libraries.R", file.path("indicators", "td.R")))

  run_env <- orchestration_environment()
  run_env$PLOT.FIGS.ONLY <- TRUE
  rm("TD.ONLY", envir = run_env)
  execute_legacy_batch(run_env)
  testthat::expect_identical(run_env$scripts, c(
    "libraries.R", file.path("indicators", c("ts.R", "sr.R"))
  ))
  testthat::expect_identical(run_env$indicator_order, c("TS", "SR"))
  testthat::expect_identical(run_env$TD.ONLY, FALSE)
})

testthat::test_that("both fixed-quality methods bypass indicators and summaries", {
  for (method in c("assume.0", "assume.1")) {
    run_env <- orchestration_environment()
    run_env$QH.METHOD <- method
    run_env$VQA.SUMMARY.ONLY <- TRUE
    run_env$input_file_list <- function(...) stop("Empirical check must be skipped")
    execute_legacy_batch(run_env)
    testthat::expect_identical(run_env$scripts, c("libraries.R", "q.fixed.R"))
  }
})

testthat::test_that("input checks accept directories without a trailing separator", {
  run_env <- orchestration_environment()
  run_env$INPUTDIR <- tempfile("orchestration-inputs-")
  dir.create(run_env$INPUTDIR)
  on.exit(unlink(run_env$INPUTDIR, recursive = TRUE))
  run_env$DF.LANDCOVER <- "landCover"
  file.create(file.path(run_env$INPUTDIR, "landCover.csv"))
  run_env$QH.METHOD <- "assume.1"
  testthat::expect_no_error(check_legacy_inputs(run_env))
  run_env$QH.METHOD <- "empirical"
  run_env$input_file_list <- function(...) c("landCover", "species")
  testthat::expect_error(check_legacy_inputs(run_env), "required input files missing")
  file.create(file.path(run_env$INPUTDIR, "species.csv"))
  testthat::expect_no_error(check_legacy_inputs(run_env))
})

testthat::test_that("missing inputs stop dispatch and preserve caller sinks and connections", {
  caller_log <- file(tempfile(), open = "w")
  caller_message_log <- file(tempfile(), open = "w")
  old_message_sink <- sink.number(type = "message")
  sink(caller_log)
  sink(caller_message_log, type = "message")
  on.exit({
    sink(type = "message")
    if (old_message_sink != 2L) sink(old_message_sink, type = "message")
    sink()
    close(caller_log)
    close(caller_message_log)
  })
  caller_depth <- sink.number()
  connections <- showConnections(all = TRUE)
  run_env <- orchestration_environment()
  run_env$input_file_list <- function(...) "missing-orchestration-input"
  testthat::expect_error(execute_legacy_batch(run_env), "required input files missing")
  testthat::expect_identical(run_env$scripts, character())
  testthat::expect_identical(sink.number(), caller_depth)
  testthat::expect_identical(sink.number(type = "message"), as.integer(caller_message_log))
  testthat::expect_identical(showConnections(all = TRUE), connections)
  cat("Caller sink still open\n")
  flush(caller_log)
})

testthat::test_that("script failures close the run log and restore temporary global functions", {
  run_env <- orchestration_environment()
  run_env$source <- function(...) stop("Script failed")
  depth <- sink.number()
  connections <- showConnections(all = TRUE)
  globals <- lapply(c("gmean", "generalized_mean"), function(name) {
    if (exists(name, envir = .GlobalEnv, inherits = FALSE)) get(name, envir = .GlobalEnv)
  })
  testthat::expect_error(execute_legacy_batch(run_env), "Script failed")
  testthat::expect_identical(sink.number(), depth)
  testthat::expect_identical(showConnections(all = TRUE), connections)
  testthat::expect_identical(lapply(c("gmean", "generalized_mean"), function(name) {
    if (exists(name, envir = .GlobalEnv, inherits = FALSE)) get(name, envir = .GlobalEnv)
  }), globals)
})

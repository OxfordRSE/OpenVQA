testthat::test_that("summary builders return tables without writing files", {
  repo <- normalizePath(file.path("..", ".."))
  config <- resolve_runtime_config(
    file.path(repo, "data", "vqa-demo1-min"), "main_001_current",
    seed = 20260810L
  )$config
  prepared <- run_fixture("vqa-demo1", "main_001_current")
  on.exit(unlink(prepared$root, recursive = TRUE), add = TRUE)
  paths <- list(
    input = prepared$context$output_dir,
    results = file.path(
      repo, "tests", "refactor", "fixtures", "vqa-demo1",
      "main_001_current", "results"
    )
  )
  files <- list.files(paths$results, full.names = TRUE)
  before <- tools::md5sum(files)
  for (group_by in c("indicator", "indicator_group")) {
    for (method in c("arithmetic", "generalized_mean")) {
      config$functional_group_group_by <- group_by
      config$functional_group_method <- method
      if (group_by == "indicator" && method == "generalized_mean") {
        testthat::expect_error(build_quality_summary(config, paths), "unused argument")
        next
      }
      tables <- build_quality_summary(config, paths)
      focal <- tables[["summary_focal_q_qh.csv"]]
      testthat::expect_true(all(c("landcover", "q", "q.lcl", "q.ucl") %in% names(focal)))
      testthat::expect_true(nrow(focal) > 0L)
      testthat::expect_true(all(is.finite(focal$q[!is.na(focal$q)])))
    }
  }
  testthat::expect_identical(tools::md5sum(files), before)
})

testthat::test_that("disabled figure stages do not read inputs or open devices", {
  state <- list()
  config <- list()
  paths <- list(generate_figures = FALSE)
  devices <- grDevices::dev.list()
  testthat::expect_identical(plot_indicator_overlap(state, config, paths), state)
  testthat::expect_identical(plot_indicator_distributions(state, config, paths), state)
  testthat::expect_identical(grDevices::dev.list(), devices)
})

testthat::test_that("report formatting preserves precision, missing values, and column order", {
  table <- data.frame(q = c(0.5, NA), qh = c(1.234, 0), notes = c("a", "b"))
  formatted <- format_table_columns(table, c(q = 2, qh = 1))
  testthat::expect_identical(formatted$q, c("0.50", NA_character_))
  testthat::expect_identical(formatted$qh, c("1.2", "0.0"))
  testthat::expect_identical(formatted$notes, table$notes)
  testthat::expect_identical(names(formatted), names(table))
  testthat::expect_identical(format_table_columns(table[0, ], c(q = 2)), table[0, ])
  testthat::expect_identical(
    format_quality_interval("0.50", "0.40", "0.60"), "0.50 [0.40, 0.60]"
  )
  testthat::expect_identical(
    names(rename_workbook_headers(table)), c("Q", "QH", "notes")
  )
})

testthat::test_that("bootstrap limits retain run-deviance direction and missing-value handling", {
  quality <- c(0.2, 0.4, NA, 0.8)
  testthat::expect_equal(
    bootstrap_quality_limits(quality), c("2.5%" = -47 / 150, "97.5%" = 77 / 300)
  )
})

testthat::test_that("indicator output stages use explicit results without caller globals", {
  config <- resolve_params(load_default_params(), list(
    bootstrap_reps = 3L, permutation_reps = 5L
  ))
  config$seed <- 42L
  directory <- tempfile("indicator-output-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  paths <- list(
    results = directory, figures = file.path(directory, "figures"),
    generate_figures = FALSE
  )
  raw <- data.frame(
    plotCode = c("b1", "b2", "f1", "f2"), focalOrBenchmark = c("b", "b", "f", "f"),
    landCover = c("forest", "forest", "focal", "focal"), vegClass = "forest",
    stratum = "nostrata", EI = 0, EI.tr = 0
  )
  write.csv(raw, file.path(directory, "SR_raw.csv"), row.names = FALSE)
  rlang::local_bindings(
    RESULTSDIR = "missing-results", FIGDIR = "missing-figures", seed = -1L,
    lowlim = -100, shape1 = -100, shape2 = -100, .env = globalenv()
  )
  result <- read_indicator_output("SR", config, paths)
  original <- result
  prepared <- prepare_indicator_results(result, config, paths)
  testthat::expect_identical(result, original)
  testthat::expect_equal(prepared$focal$f.n, 2)
  testthat::expect_equal(prepared$focal$b.n, 2)
  calculated <- calculate_indicator_quality(prepared, config, paths)
  testthat::expect_equal(calculated$focal$q, 1)
  testthat::expect_identical(calculated$bootstrap$boot.reps, 3L)
  testthat::expect_equal(
    unlist(calculated$bootstrap[paste0("X", 1:3)], use.names = FALSE), rep(1, 3)
  )
  testthat::expect_identical(plot_indicator_distributions(calculated, config, paths), calculated)
  testthat::expect_false(dir.exists(paths$figures))
  testthat::expect_error(read_indicator_output("unknown", config, paths), "Unknown indicator")
})

testthat::test_that("cover-only fixture preprocesses without changing its source inputs", {
  result <- run_fixture("vqa-demo1", "main_001_current")
  testthat::expect_equal(result$before, result$after)
  testthat::expect_true(
    file.exists(
      file.path(
        result$root,
        "main_001_current",
        "prepared",
        "ei.stratum.include.csv"
      )
    )
  )
  testthat::expect_true(nrow(result$context$inputs[["plotMetadata.csv"]]) > 0)
})

testthat::test_that("stem fixture calculates basal area before dbh", {
  result <- run_fixture("vqa-demo2", "project_baseline")
  stems <- result$context$inputs[["speciesStems.csv"]]
  testthat::expect_true(match("ba_m2", names(stems)) == match("dbh_cm", names(stems)) - 1)
  testthat::expect_equal(stems$ba_m2[1], pi * (13 / 2)^2 / 10000)
})

testthat::test_that("pruning removes low-sample land cover and orphan benchmarks", {
  inputs <- list(
    "landCover.csv" = data.frame(landCover = c("keep", "drop"), all.n.OK = c(TRUE, FALSE)),
    "plotMetadata.csv" = data.frame(
      plotCode = c("f1", "b1", "b2"),
      focalOrBenchmark = c("f", "b", "b"),
      vegClass = c("A", "A", "Z"),
      landCover = c("keep", "drop", "keep")
    ),
    "speciesCover.csv" = data.frame(plotCode = c("f1", "b1", "b2"), cover = 0.1)
  )
  context <- prune_loaded_data_objects(list(inputs = inputs))
  testthat::expect_equal(context$inputs[["plotMetadata.csv"]]$plotCode, c("f1", "b1"))
  testthat::expect_equal(context$inputs[["speciesCover.csv"]]$plotCode, c("f1", "b1"))
})

testthat::test_that("percent cover is rejected", {
  context <- list(inputs = list("coverByGrowthForm.csv" = data.frame(cover = 50)))
  testthat::expect_error(standardise_cover_tables(context), "proportions")
})

testthat::test_that("missing required fields and empty retention fail clearly", {
  testthat::expect_error(prune_low_sample_land_cover(list(inputs = list(
    "landCover.csv" = data.frame(landCover = "x", all.n.OK = FALSE),
    "plotMetadata.csv" = data.frame(focalOrBenchmark = "f", landCover = "x", vegClass = "A")
  ))), "No land-cover")
  testthat::expect_error(standardise_plot_metadata(list(inputs = list(
    "plotMetadata.csv" = data.frame(plotCode = "p")
  ))), "missing")
})

testthat::test_that("preprocessing is idempotent after include tables exist", {
  result <- run_fixture("vqa-demo1", "main_001_current")
  prepared <- file.path(result$root, "main_001_current", "prepared")
  first_files <- lapply(list.files(prepared, full.names = TRUE), read_csv_table)
  names(first_files) <- basename(list.files(prepared, full.names = TRUE))
  context <- load_preprocessing_context(result$root, "main_001_current")
  context$inputs <- load_assessment_inputs(build_file_map(prepared))
  context$output_dir <- prepared
  context <- build_allowlists(standardise_preprocessed_data(prune_loaded_data_objects(context)))
  write_preprocessed_outputs(context)
  second_files <- lapply(list.files(prepared, full.names = TRUE), read_csv_table)
  names(second_files) <- basename(list.files(prepared, full.names = TRUE))
  testthat::expect_equal(first_files, second_files)
})

testthat::test_that("land-cover summaries match the demo1 golden outputs", {
  result <- run_fixture("vqa-demo1", "main_001_current")
  summaries <- build_land_cover_summaries(result$context)
  fixture_dir <- file.path(
    normalizePath(file.path("..", "..")), "tests", "refactor", "fixtures",
    "vqa-demo1", "main_001_current", "results"
  )
  expect_same_table(summaries$summary, read_csv_table(file.path(fixture_dir, "lc.summary.csv")))
  detailed <- summaries$detailed
  names(detailed)[names(detailed) == "landCover"] <- "Land cover class"
  names(detailed)[names(detailed) == "bm.veg"] <- "Benchmark vegetation"
  expect_same_table(
    detailed,
    read_csv_table(file.path(fixture_dir, "lc.summary.detailed.csv"))
  )
})

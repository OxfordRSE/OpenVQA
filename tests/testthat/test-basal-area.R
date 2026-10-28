testthat::test_that("basal area retains DBH sampling boundaries, units, and metadata plots", {
  inputs <- stem_test_inputs()
  original <- inputs
  config <- stem_test_config()
  config$indicator_settings$BA$multiplier <- 7
  expected <- c(
    pi * ((2.5 / 2)^2 + (9.999 / 2)^2) / 48,
    pi * (10 / 2)^2 / 192, pi * (11 / 2)^2 / 192, 0, 0
  )
  totals <- basal_area_data(inputs, config)
  testthat::expect_identical(totals$plotCode, c("b1", "b2", "f1", "f2", "f3"))
  testthat::expect_equal(totals$ba, expected)
  config$indicator_settings$BA$normalize_area_m2 <- 100
  testthat::expect_equal(basal_area_data(inputs, config)$ba, expected / 100)
  config$indicator_settings$BA$normalize_area_m2 <- FALSE
  expected_raw <- c(
    sum(inputs$speciesStems.csv$ba_m2[1:3]),
    inputs$speciesStems.csv$ba_m2[4:5], 0, 0
  )
  testthat::expect_equal(basal_area_data(inputs, config)$ba, expected_raw * 7)
  testthat::expect_identical(inputs, original)
})

testthat::test_that("basal-area validation preserves missing values and native-status rules", {
  inputs <- stem_test_inputs()
  config <- stem_test_config()
  testthat::expect_error(basal_area_data(list(), config), "requires")
  config$indicator_settings$BA$na_to_zero <- FALSE
  testthat::expect_error(basal_area_data(inputs, config), "NA basal area")
  config$indicator_settings$BA$na_to_zero <- TRUE
  config$indicator_settings$BA$exclude_exotics <- TRUE
  before <- basal_area_data(inputs, config)
  testthat::expect_gt(before$ba[before$plotCode == "b2"], 0)
  config$missing_exotic_status_assume_native <- FALSE
  testthat::expect_error(basal_area_data(inputs, config), "is_exotic")
  config$indicator_settings$BA$exclude_exotics <- FALSE
  inputs$speciesStems.csv$plotCode[5] <- "b1"
  testthat::expect_error(basal_area_data(inputs, config), "Plot codes not unique")
})

testthat::test_that("basal-area outlier preparation preserves backups and subsequent exclusions", {
  inputs <- stem_test_inputs()
  config <- stem_test_config()
  config$indicator_settings$BA$normalize_area_m2 <- FALSE
  inputs$speciesStems.csv$ba_m2 <- c(0, 0, 0, 0, 10, 0)
  config$remove_ba_outliers <- TRUE
  config$ba_outliers_upper_threshold <- 3
  directory <- tempfile("ba-outliers-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  paths <- list(results = directory)
  first <- prepare_basal_area(inputs, config, paths)
  backup_file <- file.path(directory, "BA_raw.bak.csv")
  outlier_file <- file.path(directory, "BA_outliers.csv")
  backup <- readLines(backup_file)
  testthat::expect_equal(read.csv(backup_file), first)
  testthat::expect_true(first$outlier[first$plotCode == "f1"])
  testthat::expect_identical(read.csv(outlier_file)$plotCode, "f1")
  second <- prepare_basal_area(inputs, config, paths)
  testthat::expect_false("f1" %in% second$plotCode)
  testthat::expect_identical(readLines(backup_file), backup)
  inputs$speciesStems.csv$ba_m2[4] <- 11
  prepare_basal_area(inputs, config, paths)
  testthat::expect_identical(read.csv(outlier_file)$plotCode, c("f1", "b2"))
  config$ba_outliers_append <- FALSE
  inputs$speciesStems.csv$ba_m2[1] <- 12
  prepare_basal_area(inputs, config, paths)
  testthat::expect_identical(read.csv(outlier_file)$plotCode, "b1")
  testthat::expect_identical(readLines(backup_file), backup)
  config$remove_ba_outliers <- FALSE
  testthat::expect_true("f1" %in% prepare_basal_area(inputs, config, paths)$plotCode)
})

testthat::test_that("basal-area execution prepares inputs and writes the established files", {
  inputs <- stem_test_inputs()
  inputs$speciesStems.csv$ba_m2 <- 0
  config <- stem_test_config()
  config$indicator_settings$BA$normalize_area_m2 <- FALSE
  config$force_prepare_raw <- TRUE
  config$remove_ba_outliers <- TRUE
  config$seed <- 42L
  directory <- tempfile("ba-preparation-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  paths <- list(results = directory, figures = file.path(directory, "figs"))
  result <- run_basal_area(inputs, config, paths)
  testthat::expect_equal(result$raw$EI, rep(0, 5))
  testthat::expect_equal(result$focal$q, 1)
  testthat::expect_true(file.exists(file.path(directory, "BA_raw.bak.csv")))
  testthat::expect_false(file.exists(file.path(directory, "BA_outliers.csv")))
  testthat::expect_equal(read.csv(file.path(directory, "BA_raw.csv")), result$raw)
})

testthat::test_that("PCS, CH and BAES retain their seeded output contracts", {
  # Expected tables were captured from indicators at af8fbb2 with seed 42,
  # using supported_indicator_inputs() and the original params.R defaults.
  expected <- jsonlite::read_json(
    file.path(.params_root, "tests", "testthat", "fixtures-supported-indicators.json"),
    simplifyVector = TRUE
  )
  inputs <- supported_indicator_inputs()
  original <- inputs
  config <- supported_indicator_config()
  config$seed <- 7L
  runners <- list(PCS = run_pcs, CH = run_canopy_height, BAES = run_exotic_basal_area)
  directory <- tempfile("supported-execution-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  paths <- list(results = directory, figures = file.path(directory, "figs"))
  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(tempdir())
  rlang::local_bindings(
    source = function(...) stop("Execution must not source scripts"),
    sys.source = function(...) stop("Execution must not source scripts"),
    INPUTDIR = "missing-input-directory", RESULTSDIR = "missing-results-directory",
    seed = -1L, dat = "caller-owned", .env = globalenv()
  )
  for (code in names(runners)) {
    run <- runners[[code]]
    first <- run(inputs, config, paths, seed = 42L)
    for (file in names(expected[[code]])) {
      testthat::expect_equal(read.csv(file.path(directory, file)), expected[[code]][[file]],
        tolerance = 1e-12, info = file
      )
    }
    if (code == "CH") {
      cover_config <- config
      cover_config$indicator_settings$CH$data_type <- "cover"
      cover_inputs <- inputs
      cover_inputs$speciesCover.csv <- cover_inputs$speciesStems.csv
      cover_inputs$speciesStems.csv <- NULL
      testthat::expect_equal(run(cover_inputs, cover_config, paths, seed = 42L), first)
    }
    boot_file <- file.path(directory, paste0(code, "_boot.q.csv"))
    bootstrap <- read.csv(boot_file)
    testthat::expect_equal(run(inputs, config, paths, seed = 42L), first)
    testthat::expect_identical(read.csv(boot_file), bootstrap)
    run(inputs, config, paths, seed = 43L)
    testthat::expect_false(identical(read.csv(boot_file), bootstrap))
    reuse <- config
    reuse$force_prepare_raw <- FALSE
    testthat::expect_equal(run(list(), reuse, paths, seed = 42L), first)
    reuse$plot_figures_only <- TRUE
    unlink(file.path(directory, paste0(code, "_benchmark.csv")))
    testthat::expect_null(run(list(), reuse, paths))
  }
  testthat::expect_identical(inputs, original)
  testthat::expect_identical(get("dat", globalenv()), "caller-owned")
  testthat::expect_false(dir.exists(paths$figures))
})

testthat::test_that("CH retains five tallest rows, status handling and the plus-one offset", {
  config <- supported_indicator_config()
  inputs <- stem_test_inputs()
  inputs$speciesStems.csv$height_m <- c(100, 20, 10, 9, 8, 7)
  result <- canopy_height_data(inputs, config)
  testthat::expect_equal(result$EI[result$plotCode == "b1"], mean(c(100, 20, 10)) + 1)
  testthat::expect_equal(result$EI[result$plotCode == "b2"], 10)
  testthat::expect_equal(result$EI[result$plotCode == "f3"], 1)
  config$indicator_settings$CH$exclude_exotics <- TRUE
  testthat::expect_equal(canopy_height_data(inputs, config), result)
  # Seven observations make a five-row mean distinct from a ten-percent rule.
  inputs$speciesStems.csv <- inputs$speciesStems.csv[rep(1, 7), ]
  inputs$speciesStems.csv$height_m <- 1:7
  testthat::expect_equal(canopy_height_data(inputs, config)$EI[1], mean(3:7) + 1)
  config$indicator_settings$CH$exclude_exotics <- TRUE
  testthat::expect_equal(canopy_height_data(inputs, config)$EI[1], mean(3:7) + 1)
  config$missing_exotic_status_assume_native <- FALSE
  testthat::expect_error(canopy_height_data(inputs, config), "is_exotic")
  inputs$speciesStems.csv$height_m <- NULL
  testthat::expect_error(canopy_height_data(inputs, config), "height_m")
})

testthat::test_that("BAES sums only exotic basal area and completes metadata-only plots", {
  config <- supported_indicator_config()
  inputs <- stem_test_inputs()
  inputs$speciesStems.csv$ba_m2 <- 1:6
  result <- exotic_basal_area_data(inputs, config)
  testthat::expect_equal(result$EI, c(0, 400, 0, 0, 0))
  testthat::expect_equal(result$EI.tr, result$EI)
  config$indicator_settings$BAES$multiplier <- 2
  testthat::expect_equal(exotic_basal_area_data(inputs, config)$EI, c(0, 8, 0, 0, 0))
  config$missing_exotic_status_assume_native <- FALSE
  testthat::expect_error(exotic_basal_area_data(inputs, config), "is_exotic")
  inputs$speciesStems.csv$species[6] <- "native"
  testthat::expect_equal(exotic_basal_area_data(inputs, config)$EI[2], 8)
  inputs$speciesStems.csv$species <- "native"
  testthat::expect_error(exotic_basal_area_data(inputs, config), "no rows to aggregate")
})

testthat::test_that("PCS drops missing cover and fills only included observed plot strata", {
  config <- supported_indicator_config()
  inputs <- supported_indicator_inputs()
  cover <- inputs$coverByStratum.csv
  cover$cover[1] <- NA
  cover$cover[2] <- 150
  inputs$coverByStratum.csv <- cover
  config$indicator_settings$PCS$convert_percent <- TRUE
  config$indicator_settings$PCS$scale_abundance <- TRUE
  config$indicator_settings$PCS$logit <- TRUE
  first <- pcs_data(inputs, config)
  testthat::expect_equal(first$EI[first$plotCode == cover$plotCode[2] &
    first$stratum == cover$stratum[2]], 1)
  testthat::expect_identical(first$EI.tr, first$EI)
  testthat::expect_false(anyNA(first$EI))
  testthat::expect_true(any(first$EI == 0))
  # The exclusions and vegetation exceptions also govern zero-filled combinations.
  inputs$ei.stratum.include.csv$include[1] <- FALSE
  omitted <- pcs_data(inputs, config)
  testthat::expect_false(any(omitted$stratum == inputs$ei.stratum.include.csv$stratum[1]))
  inputs$ei.stratum.veg.include.csv <- data.frame(
    EI = "PCS", stratum = inputs$ei.stratum.include.csv$stratum[1],
    bm.vegetation = "W17", include = TRUE
  )
  restored <- pcs_data(inputs, config)
  keys <- paste(first$plotCode, first$stratum)
  restored_keys <- paste(restored$plotCode, restored$stratum)
  testthat::expect_setequal(restored_keys, keys)
  testthat::expect_equal(restored$EI[match(keys, restored_keys)], first$EI)
  config$indicator_settings$PCS$remove_zero_cover_plots <- TRUE
  cover$cover[cover$plotCode == cover$plotCode[1]] <- 0
  inputs$coverByStratum.csv <- cover
  testthat::expect_false(cover$plotCode[1] %in% pcs_data(inputs, config)$plotCode)
  for (prepare in list(pcs_data, canopy_height_data, exotic_basal_area_data)) {
    testthat::expect_error(prepare(list(), config), "requires")
  }
})

testthat::test_that("supported configuration and input mappings cover every accepted indicator", {
  config <- supported_indicator_config()
  testthat::expect_no_error(validate_analysis_params(config))
  schema <- jsonlite::read_json(file.path(.params_root, "schema", "params.schema.json"))
  codes <- unlist(schema$properties$active_indicators$items$enum)
  testthat::expect_setequal(names(config$indicator_settings), codes)
  testthat::expect_setequal(names(config$indicator_groups), codes)
  testthat::expect_setequal(names(schema$properties$indicator_settings$properties), codes)
  parameters <- legacy_project_params(config)
  testthat::expect_identical(parameters$ei.params("PCS")$source.file, "coverByStratum.csv")
  testthat::expect_identical(parameters$ei.params("BAES")$source.file, "speciesStems.csv")
  source(file.path(.params_root, "validation", "requirements.R"), local = TRUE)
  manifest <- validation_requirements()$indicators
  testthat::expect_setequal(names(manifest), codes)
  config$active_indicators <- list("unknown")
  testthat::expect_error(validate_analysis_params(config), "validating json")
})

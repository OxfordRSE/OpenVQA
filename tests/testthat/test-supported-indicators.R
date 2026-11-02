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

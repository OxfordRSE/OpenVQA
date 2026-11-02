testthat::test_that("stem abundance counts individuals within the supplied size-class labels", {
  inputs <- stem_test_inputs()
  original <- inputs
  config <- stem_test_config()
  raw <- stem_abundance_data(inputs, config)
  values <- setNames(raw$EI, paste(raw$plotCode, raw$stratum))
  testthat::expect_equal(unname(values["b1 Stems 2.5 to <10 cm DBH"]), 2)
  testthat::expect_equal(unname(values["b1 Stems >=10 cm DBH"]), 0)
  testthat::expect_equal(unname(values["b2 Stems >=10 cm DBH"]), 1)
  testthat::expect_equal(unname(values["f2 Stems >=10 cm DBH"]), 0)
  testthat::expect_equal(unname(values["f3 Stems >=10 cm DBH"]), 0)
  testthat::expect_identical(
    sort(unique(raw$stratum)),
    sort(unique(inputs$speciesStems.csv$stratum))
  )
  testthat::expect_identical(raw$EI.tr, raw$EI)
  testthat::expect_identical(inputs, original)
  inputs$speciesStems.csv$stratum[2] <- "Stems >=10 cm DBH"
  changed <- stem_abundance_data(inputs, config)
  testthat::expect_equal(changed$EI[changed$plotCode == "b1" &
    changed$stratum == "Stems >=10 cm DBH"], 1)
  testthat::expect_equal(changed$EI[changed$plotCode == "b1" &
    changed$stratum == "Stems 2.5 to <10 cm DBH"], 2)
})

testthat::test_that("stem abundance keeps inclusion exceptions and missing-plot completion", {
  inputs <- stem_test_inputs()
  config <- stem_test_config()
  inputs$ei.stratum.include.csv$include[2] <- FALSE
  inputs$ei.stratum.veg.include.csv <- data.frame(
    EI = "ASC", stratum = "Stems >=10 cm DBH", bm.vegetation = "forest", include = TRUE
  )
  inputs$plotMetadata.csv$vegClass[4] <- "grass"
  inputs$speciesStems.csv$vegClass[6] <- "grass"
  raw <- stem_abundance_data(inputs, config)
  testthat::expect_true(any(raw$plotCode == "b2" & raw$stratum == "Stems >=10 cm DBH"))
  testthat::expect_false(any(raw$plotCode == "f2" & raw$stratum == "Stems >=10 cm DBH"))
  config$include_plots_nodata <- FALSE
  testthat::expect_true("f3" %in% stem_abundance_data(inputs, config)$plotCode)
  inputs$ei.stratum.veg.include.csv$include <- FALSE
  testthat::expect_false("Stems >=10 cm DBH" %in% stem_abundance_data(inputs, config)$stratum)
})

testthat::test_that("stem abundance preserves its NBin transformations and validation", {
  inputs <- stem_test_inputs()
  config <- stem_test_config()
  config$indicator_settings$ASC$distribution <- "NBin"
  config$indicator_settings$ASC$multiplier <- 99
  config$zero_to_one <- FALSE
  raw <- stem_abundance_data(inputs, config)
  testthat::expect_true(all(raw$EI >= 1))
  testthat::expect_equal(raw$EI[raw$plotCode == "b1" &
    raw$stratum == "Stems 2.5 to <10 cm DBH"], 2)
  testthat::expect_identical(raw$EI.tr, raw$EI)
  testthat::expect_error(stem_abundance_data(list(), config), "requires")
  config$indicator_settings$ASC$exclude_exotics <- TRUE
  config$missing_exotic_status_assume_native <- FALSE
  testthat::expect_error(stem_abundance_data(inputs, config), "is_exotic")
  config$indicator_settings$ASC$exclude_exotics <- FALSE
  inputs$speciesStems.csv$plotCode[5] <- "b1"
  testthat::expect_error(stem_abundance_data(inputs, config), "Plot codes not unique")
})

testthat::test_that("stem abundance writes raw, pooled benchmark, and per-stratum focal tables", {
  inputs <- stem_test_inputs()
  inputs$speciesStems.csv$dbh_cm <- 10
  config <- stem_test_config()
  config$force_prepare_raw <- TRUE
  config$seed <- 42L
  directory <- tempfile("asc-preparation-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  paths <- list(results = directory, figures = file.path(directory, "figs"))
  result <- run_stem_abundance(inputs, config, paths)
  testthat::expect_identical(names(result), c("raw", "focal", "benchmark"))
  testthat::expect_identical(result$benchmark$stratum, "nostrata")
  testthat::expect_identical(sort(result$focal$stratum), sort(unique(result$raw$stratum)))
  testthat::expect_equal(read.csv(file.path(directory, "ASC_raw.csv")), result$raw)
  testthat::expect_false(file.exists(file.path(directory, "ASC_raw.bak.csv")))
})

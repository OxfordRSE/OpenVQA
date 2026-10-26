sr_test_config <- function() {
  resolve_params(load_default_params(), list(
    project = "sr-unit-test", data_type = "cover", bootstrap_reps = 5,
    active_indicators = list("SR"), aggregate_indicators = list(), indicator_groups = list(
      SR = "composition"
    ),
    plot_hist = FALSE, plot_overlap = FALSE, zero_to_one = FALSE
  ))
}

sr_test_inputs <- function() {
  list(
    plotMetadata.csv = data.frame(
      plotCode = c("b1", "b2", "f1", "f2"), focalOrBenchmark = c("b", "b", "f", "f"),
      landCover = c("forest", "forest", "focal", "focal"), vegClass = "forest"
    ),
    species.csv = data.frame(species = c("native", "exotic", "unknown"), is_exotic = c(0, 1, NA)),
    speciesCover.csv = data.frame(
      plotCode = c("b1", "b1", "b2", "f1", "f1", "f2"),
      species = c("native", "native", "exotic", "native", "unknown", "native"),
      cover = c(1, 2, 5, 0, 3, 2)
    )
  )
}

testthat::test_that(
  "fixed-quality tables preserve aggregation, ordering, types, and bootstrap columns",
  {
    inputs <- list(landCover.csv = data.frame(
      vegClass = c("woodland", "forest", "woodland", "forest"),
      area_ha = c("2.5", "3", NA, "1.5")
    ))
    original <- inputs
    for (method in c("assume.0", "assume.1")) {
      config <- list(qh_method = method, bootstrap_reps = 3)
      tables <- fixed_quality_tables(inputs, config)
      q <- if (method == "assume.0") 0 else 1
      expected <- data.frame(
        bm.veg = c("forest", "woodland"), area_ha = c(4.5, 2.5),
        qh = c(4.5, 2.5) * q, qh.lcl = c(4.5, 2.5) * q, qh.ucl = c(4.5, 2.5) * q, notes = ""
      )
      testthat::expect_identical(tables$benchmark, expected)
      testthat::expect_identical(tables$bootstrap, data.frame(
        bm.veg = expected$bm.veg, X1 = rep(q, 2), X2 = rep(q, 2), X3 = rep(q, 2)
      ))
    }
    testthat::expect_identical(inputs, original)
    testthat::expect_error(fixed_quality_tables(inputs, list(qh_method = "empirical")), "Invalid")
    testthat::expect_error(fixed_quality_tables(list(), list(qh_method = "assume.0")), "landCover")
  }
)

testthat::test_that("fixed-quality writing uses the established filenames and tables", {
  directory <- tempfile("fixed-quality-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  inputs <- list(landCover.csv = data.frame(vegClass = "forest", area_ha = 3))
  tables <- run_fixed_quality(
    inputs, list(qh_method = "assume.0", bootstrap_reps = 2),
    list(results = directory)
  )
  testthat::expect_identical(sort(list.files(directory)), c(
    "summary_focal_qh_bm.csv", "summary_qh_boot_bm.csv"
  ))
  summary <- read_csv_table(file.path(directory, "summary_focal_qh_bm.csv"))
  testthat::expect_identical(names(summary), names(tables$benchmark))
  testthat::expect_equal(summary$qh, 0)
  bootstrap <- read_csv_table(file.path(directory, "summary_qh_boot_bm.csv"))
  testthat::expect_identical(names(bootstrap), c("bm.veg", "X1", "X2"))
  testthat::expect_equal(unlist(bootstrap[-1], use.names = FALSE), c(0, 0))
})

testthat::test_that("SR preparation counts unique native species and retains empty plots", {
  inputs <- sr_test_inputs()
  original <- inputs
  config <- sr_test_config()
  raw <- species_richness_data(inputs, config)
  testthat::expect_identical(raw$plotCode, c("b1", "b2", "f1", "f2"))
  testthat::expect_identical(raw$EI, c(1L, 0L, 1L, 1L))
  testthat::expect_identical(raw$EI.tr, raw$EI)
  testthat::expect_identical(raw$stratum, rep("nostrata", 4))
  testthat::expect_identical(names(raw), c(
    "plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI", "EI.tr"
  ))
  testthat::expect_identical(inputs, original)
  config$zero_to_one <- TRUE
  testthat::expect_identical(species_richness_data(inputs, config)$EI, rep(1, 4))
  config$vegetation_without_data <- list("focal")
  testthat::expect_identical(species_richness_data(inputs, config)$plotCode, c("b1", "b2"))
  config$missing_exotic_status_assume_native <- FALSE
  testthat::expect_error(species_richness_data(inputs, config), "native|is_exotic")
  testthat::expect_error(species_richness_data(list(), config), "requires")
})

testthat::test_that("SR stem inputs retain occurrences without applying a cover filter", {
  inputs <- sr_test_inputs()
  inputs$speciesStems.csv <- inputs$speciesCover.csv[, c("plotCode", "species")]
  inputs$speciesCover.csv <- NULL
  config <- sr_test_config()
  config$indicator_settings$SR$data_type <- "ind"
  raw <- species_richness_data(inputs, config)
  testthat::expect_identical(raw$EI, c(1L, 0L, 2L, 1L))
})

testthat::test_that("the all-zero SR workflow is callable without scripts or caller paths", {
  inputs <- sr_test_inputs()
  config <- sr_test_config()
  config$seed <- 42L
  directory <- tempfile("sr-results-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  raw <- species_richness_data(inputs, config)
  raw$EI <- raw$EI.tr <- 0L
  write.csv(raw, file.path(directory, "SR_raw.csv"), row.names = FALSE)
  config$force_prepare_raw <- FALSE
  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(tempdir())
  rlang::local_bindings(
    source = function(...) stop("Calculation must not source scripts"),
    sys.source = function(...) stop("Calculation must not source scripts"),
    INPUTDIR = "missing-input-directory", RESULTSDIR = "missing-result-directory",
    .env = globalenv()
  )
  paths <- list(results = directory, figures = file.path(directory, "figs"))
  result <- run_species_richness(inputs, config, paths)
  focal <- read.csv(result$focal)
  testthat::expect_identical(focal$EI, "SR")
  testthat::expect_equal(focal$f.mean, 0)
  testthat::expect_equal(focal$b.mean, 0)
  testthat::expect_equal(focal$q, 1)
  testthat::expect_equal(focal$q.lcl, 1)
  testthat::expect_equal(focal$q.ucl, 1)
  bootstrap <- read.csv(file.path(directory, "SR_boot.q.csv"))
  testthat::expect_equal(unlist(bootstrap[paste0("X", 1:5)], use.names = FALSE), rep(1, 5))
  testthat::expect_false(dir.exists(paths$figures))
  first <- lapply(list.files(directory, full.names = TRUE), readLines, warn = FALSE)
  run_species_richness(inputs, config, paths)
  second <- lapply(list.files(directory, full.names = TRUE), readLines, warn = FALSE)
  testthat::expect_identical(second, first)
})

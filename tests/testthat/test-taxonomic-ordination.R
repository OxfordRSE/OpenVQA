testthat::test_that("NMDS matrix conversion retains ordering and does not change the RNG", {
  occurrences <- data.frame(
    plot = c("b", "a", "b", "a"), abundance = c(2, 1, 4, 3), species = c("A", "A", "B", "B")
  )
  set.seed(9)
  before <- .Random.seed
  raw <- taxonomic_distance_matrix(occurrences, FALSE)
  testthat::expect_identical(.Random.seed, before)
  testthat::expect_identical(rownames(raw), c("a", "b"))
  testthat::expect_identical(colnames(raw), c("A", "B"))
  testthat::expect_s3_class(raw, "data.frame")
  testthat::expect_equal(unname(as.matrix(raw)), matrix(c(1, 2, 3, 4), nrow = 2))
  normalized <- taxonomic_distance_matrix(occurrences, TRUE)
  testthat::expect_equal(rowSums(normalized), c(a = 1, b = 1))
})

testthat::test_that("NMDS seeding uses its configured seed separately from the run seed", {
  config <- taxonomic_test_config()
  matrix <- taxonomic_ordination_matrix()
  first <- fit_taxonomic_ordination(matrix, config, seed = 42L)
  after <- .Random.seed
  second <- fit_taxonomic_ordination(matrix, config, seed = 43L)
  testthat::expect_identical(second$points, first$points)
  testthat::expect_identical(.Random.seed, after)
  config$nmds$seed_enabled <- FALSE
  testthat::expect_identical(fit_taxonomic_ordination(matrix, config,
    seed = 42L
  )$points, first$points)
  config$nmds$seed <- 11L
  fit_taxonomic_ordination(matrix, config, seed = 42L)
  testthat::expect_false(identical(.Random.seed, after))
  set.seed(91)
  no_reset <- fit_taxonomic_ordination(matrix, config, seed = NULL)
  after_no_reset <- .Random.seed
  set.seed(91)
  expected <- vegan::metaMDS(matrix,
    distance = "bray", try = 20, trymax = config$nmds$trymax,
    k = config$nmds$dimensions, maxit = config$nmds$max_iterations,
    autotransform = FALSE, trace = config$nmds$verbose
  )
  testthat::expect_identical(no_reset$points, expected$points)
  testthat::expect_identical(.Random.seed, after_no_reset)
})

testthat::test_that("TD abundance transformations retain scaling and raw/transformed values", {
  config <- taxonomic_test_config()
  raw <- data.frame(EI = c(0.1, 0.4, 0.8), EI.tr = NA_real_)
  config$indicator_settings$TD$scale_abundance <- TRUE
  transformed <- transform_taxonomic_abundance(raw, config)
  testthat::expect_equal(transformed$EI, raw$EI)
  testthat::expect_equal(transformed$EI.tr, c(0, 3 / 7, 1))
  config$indicator_settings$TD$scale_abundance <- FALSE
  config$indicator_settings$TD$convert_percent <- TRUE
  raw$EI <- c(10, 50, 150)
  transformed <- transform_taxonomic_abundance(raw, config)
  testthat::expect_equal(transformed$EI, c(0.1, 0.5, 1))
  testthat::expect_equal(transformed$EI.tr, transformed$EI)
  config$indicator_settings$TD$data_type <- "ind"
  raw$EI.tr <- raw$EI
  testthat::expect_equal(transform_taxonomic_abundance(raw, config), raw)
})

testthat::test_that("ordination distances preserve two- and three-dimensional centroid geometry", {
  plots <- data.frame(
    plotCode = c("b1", "b2", "f1"), focalOrBenchmark = c("b", "b", "f"),
    landCover = c("forest", "forest", "focal"), vegClass = "forest"
  )
  points <- matrix(c(0, 2, 1, 0, 0, 3),
    ncol = 2,
    dimnames = list(plots$plotCode, c("x", "y"))
  )
  ordination <- list(points = points, stress = 0.1, converged = 1L)
  two <- taxonomic_ordination_distances(ordination, plots, 2L)
  testthat::expect_equal(two$td, c(1, 1, 3))
  testthat::expect_identical(two$z, rep("", 3))
  ordination$points <- cbind(points, z = c(0, 2, 1))
  three <- taxonomic_ordination_distances(ordination, plots, 3L)
  testthat::expect_equal(three$td, c(sqrt(2), sqrt(2), 3))
  testthat::expect_identical(three$dim, rep(3L, 3))
  testthat::expect_error(taxonomic_ordination_distances(ordination, plots, 4L), "invalid value")
})

testthat::test_that("TD outlier tests preserve separate focal and benchmark samples", {
  scores <- data.frame(
    plotCode = c("b1", "b2", "b3", "f1", "f2", "f3"),
    focalOrBenchmark = rep(c("b", "f"), each = 3),
    landCover = rep(c("forest", "focal"), each = 3), vegClass = "forest", dim = 2L,
    EI.tr = rep(c(0, 1, 1000), 2)
  )
  config <- taxonomic_test_config()
  config$td_outlier_upper_threshold <- 100
  config$td_outlier_stdevs <- 100
  flagged <- flag_taxonomic_outliers(scores, config, "focal")
  testthat::expect_identical(flagged$outlier, rep(c(FALSE, FALSE, TRUE), 2))
  scores$EI.tr <- rep(c(0, 1, 2), 2)
  config$td_outlier_stdevs <- 0.5
  flagged <- flag_taxonomic_outliers(scores, config, "focal")
  testthat::expect_identical(flagged$outlier, rep(c(TRUE, FALSE, TRUE), 2))
})

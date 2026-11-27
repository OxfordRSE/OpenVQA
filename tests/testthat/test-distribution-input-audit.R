testthat::test_that("distribution audit handles adjustments, bypasses and shared benchmarks", {
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "utilities", "audit-distribution-inputs.R"), env)
  root <- tempfile("distribution-audit-")
  directory <- file.path(root, "project", "assessment", "results")
  dir.create(directory, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  focal <- data.frame(
    landcover = c("A", "B"), stratum = "s", bm.vegetation = "V",
    dist = "Bet", q.method = "empirical"
  )
  raw <- data.frame(
    focalOrBenchmark = rep(c("f", "f", "b"), each = 3),
    landCover = rep(c("A", "B", "V"), each = 3), vegClass = "V",
    stratum = "s", EI = c(0, 0.5, 1, 0.2, 0.4, 0.6, 0.5, 0.5, 0.5)
  )
  write.csv(focal, file.path(directory, "TEST_focal.csv"), row.names = FALSE)
  write.csv(raw, file.path(directory, "TEST_raw.csv"), row.names = FALSE)
  fixed <- focal
  fixed$q.method <- "fixed"
  write.csv(fixed, file.path(directory, "FIXED_focal.csv"), row.names = FALSE)
  gamma <- focal
  gamma$dist <- "gamma"
  write.csv(gamma, file.path(directory, "GAMMA_focal.csv"), row.names = FALSE)
  raw$EI <- c(0, 0, 0, 0, 1, 2, 0, 0, 0)
  write.csv(raw, file.path(directory, "GAMMA_raw.csv"), row.names = FALSE)
  audit <- env$audit_distribution_inputs(root)
  testthat::expect_equal(nrow(audit), 6L)
  testthat::expect_false(any(audit$indicator == "FIXED"))
  testthat::expect_false(any(audit$indicator == "GAMMA" & audit$landcover == "A"))
  summary <- env$summarise_distribution_audit(audit)
  beta <- subset(summary, distribution == "Bet" & unit == "unique input datasets")
  testthat::expect_equal(beta$total, 3L)
  testthat::expect_equal(beta$beta_boundary, 1L)
  testthat::expect_equal(beta$beta_constant, 1L)
  testthat::expect_equal(beta$adjusted, 2L)
  testthat::expect_equal(subset(summary, distribution == "Bet" &
    unit == "indicator-quality outputs")$adjusted, 2L)
  testthat::expect_equal(subset(summary, distribution == "gamma" &
    unit == "unique input datasets")$gamma_zero, 2L)
  raw$EI[1] <- Inf
  write.csv(raw, file.path(directory, "GAMMA_raw.csv"), row.names = FALSE)
  testthat::expect_error(env$audit_distribution_inputs(root), "nonfinite")
  raw$EI[1] <- NA_real_
  write.csv(raw, file.path(directory, "GAMMA_raw.csv"), row.names = FALSE)
  testthat::expect_error(env$audit_distribution_inputs(root), "nonfinite")
  raw$EI[1] <- -1
  write.csv(raw, file.path(directory, "GAMMA_raw.csv"), row.names = FALSE)
  testthat::expect_error(env$audit_distribution_inputs(root), "below zero")
})

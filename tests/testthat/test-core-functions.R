core_functions_environment <- function() {
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "includes", "functions.R"), envir = env)
  # Numerical helpers use the same package functions as the analysis libraries.
  env$boot.ci <- boot::boot.ci
  env
}

testthat::test_that("the compatibility loader works outside the repository root", {
  loader <- normalizePath(file.path("..", "..", "includes", "functions.R"))
  old_directory <- getwd()
  on.exit(setwd(old_directory))
  setwd(tempdir())
  for (load in list(
    function(env) base::source(loader, local = env),
    function(env) sys.source(loader, envir = env)
  )) {
    env <- new.env(parent = globalenv())
    before <- ls(globalenv(), all.names = TRUE)
    load(env)
    testthat::expect_identical(ls(globalenv(), all.names = TRUE), before)
    testthat::expect_true(is.function(env$functions.loaded))
    for (name in ls(env)) {
      testthat::expect_identical(environment(env[[name]]), env)
    }
    testthat::expect_equal(env$q.linear(5, 10, 0), 0.5)
    testthat::expect_false(any(c("includes_dir", "frame", "path", "file") %in% ls(env)))
  }
})

testthat::test_that("quality transformations preserve endpoints and validation", {
  env <- core_functions_environment()
  testthat::expect_equal(env$q.linear(5, 0, 10), 0.5)
  testthat::expect_error(env$q.linear(2, 1, 0), "between")
  testthat::expect_error(env$q.linear(0, 1, 1), "cannot be equal")
  testthat::expect_equal(env$q.logistic(0.5), 0.5)
  testthat::expect_error(env$q.logistic(-0.1), "must be")
  for (q in c(0, 0.5, 1)) {
    testthat::expect_equal(env$q.inverse.logit(q), q)
    testthat::expect_equal(env$q.scaled.beta(q, 1, 1), q)
  }
  testthat::expect_error(env$q.inverse.logit(0.5, beta = 0), "beta must be")
  testthat::expect_error(env$q.scaled.beta(1.1), "must be")
  for (weight in c("beta.wt", "u.beta")) {
    testthat::expect_equal(env[[weight]](0), 1)
    testthat::expect_equal(env[[weight]](0.5), 0)
    testthat::expect_equal(env[[weight]](1), 1)
    testthat::expect_error(env[[weight]](-0.1), "outside domain")
  }
  testthat::expect_equal(env$u.beta.buffered(0.01, buffer = 0.02), 1)
  testthat::expect_equal(env$u.beta.buffered(0.99, buffer = 0.02), 1)
  testthat::expect_error(env$u.beta.buffered(0.5, buffer = 0.2), "outside domain")
  testthat::expect_equal(env$qc.calc(0.5, 0, 1, 1, 1, "lower", 10), 0.5)
  testthat::expect_equal(env$qc.calc(0.5, 0, 1, 0, 0, "upper", 10), 0.5)
  testthat::expect_equal(env$qc.calc.simple(0, 0, 1, 10), 0)
  testthat::expect_equal(env$qc.calc.simple(1, 0, 1, 10), 1)
  testthat::expect_equal(env$qc.calc.simple(0.25, 0, 1, 10, transform = FALSE), 0.25)
  testthat::expect_true(is.na(env$qc.calc.simple(2, 0, 1, 10)))
  testthat::expect_null(env$qc.calc.simple.notransform(NA_real_, 0, 1, 10))
  testthat::expect_true(is.na(env$qc.calc.simple.notransform(2, 0, 1, 10)))
})

testthat::test_that("beta data helpers preserve boundaries and sample sizes", {
  env <- core_functions_environment()
  x <- c(0, 0.5, 1)
  testthat::expect_equal(env$beta.tr(x), c(1 / 6, 0.5, 5 / 6))
  testthat::expect_equal(env$beta.transform(x), env$beta.tr(x))
  testthat::expect_equal(env$beta.tr(c(0.2, 0.8)), c(0.2, 0.8))
  testthat::expect_equal(env$beta.constrain(x), c(0.000001, 0.5, 0.999999))
  testthat::expect_equal(env$beta.tr.all(x, 0), list(env$beta.tr(x), 1 / 6))
  testthat::expect_error(env$beta.transform(numeric()), "zero length")
  for (name in c("beta.tr", "beta.constrain", "uni.fit.beta")) {
    testthat::expect_output(
      result <- env[[name]](c(-0.1, 1.1)), "Values >1 or <0 not allowed"
    )
    testthat::expect_match(result, "Values >1 or <0 not allowed")
  }
  testthat::expect_output(env$beta.tr.all(x, 2), "Values >1 or <0 not allowed")
  split <- env$split.zoib(c(0, 0, 0.25, 0.75, 1))
  testthat::expect_equal(c(split$n0, split$n1, split$n.beta), c(2, 1, 2))
  testthat::expect_equal(c(split$p0, split$p1, split$p.beta), c(0.4, 0.2, 0.4))
  testthat::expect_equal(split$v.beta, c(0.25, 0.75))
  testthat::expect_error(env$split.zoib(c(-1, 0)), "outside interval")
  for (value in c(0, 0.5, 1)) {
    jittered <- env$jitter.beta(rep(value, 5))
    testthat::expect_length(jittered, 5)
    testthat::expect_true(all(jittered >= 0 & jittered <= 1))
    testthat::expect_gt(length(unique(jittered)), 1)
  }
  testthat::expect_equal(env$mylogit(0.5), 0)
  testthat::expect_equal(env$noise.beta.norm(c(0.2, 0.8), 2, 0), c(0.2, 0.8))
  testthat::expect_error(env$beta.normal(c(0, 1)), "outside domain")
  set.seed(42)
  normal <- env$beta.normal(rep(0.5, 5))
  testthat::expect_length(normal, 5)
  testthat::expect_true(all(is.finite(normal)))
  set.seed(42)
  testthat::expect_identical(env$beta.normal(rep(0.5, 5)), normal)
})

testthat::test_that("quality coefficients retain fixed and distribution-based scoring", {
  env <- core_functions_environment()
  f <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.7, 0.9)
  testthat::expect_equal(env$qual.prop(f, f), 1)
  testthat::expect_error(env$qual.prop(f, 2), "not a proportion")
  testthat::expect_equal(env$qual.prop.fixed(0.25, 0.5), 0.5)
  testthat::expect_equal(env$qual.prop.fixed(0.75, 0.5), 0.5)
  testthat::expect_equal(env$qual.prop.fixed(0.5, 0.5), 1)
  testthat::expect_error(env$qual.prop.fixed(2, 0.5), "not a proportion")
  fixed <- env$qual.prop.fixed.b(f, 0.5, test.diff = FALSE)
  testthat::expect_equal(fixed$q, 1 - abs(mean(f) - 0.5) / 0.5)
  testthat::expect_true(is.na(fixed$p))
  testthat::expect_equal(
    unname(env$qual.beta.fixed(f, 0.5, test.diff = FALSE)$q), fixed$q
  )
  testthat::expect_error(env$qual.prop.fixed.f(2, f), "not proportions")
  testthat::expect_equal(env$qual.beta.zoib(c(0, 1), c(0, 1))$q, 1)
  testthat::expect_error(env$qual.beta.zoib(-1, f), "outside interval")
  for (name in c("qual.beta", "qual.beta.orig")) {
    result <- env[[name]](f, f)
    testthat::expect_equal(result$q, 1, tolerance = 1e-6)
    testthat::expect_equal(result$diff, 0)
  }
  result <- env$qual.beta.ol(f, f)
  testthat::expect_equal(result$q.ol, 1)
  testthat::expect_identical(result$beta.tr.method, "effectively identical")
  testthat::expect_error(env$qual.beta(f, f, wt.method = "unknown"), "not valid")
})

testthat::test_that("distribution fitting and overlap retain numerical results", {
  env <- core_functions_environment()
  f <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.7, 0.9)
  fit <- env$fit.beta(f)
  testthat::expect_false(fit$fail)
  testthat::expect_equal(fit$f.mean, fit$shape1 / (fit$shape1 + fit$shape2))
  testthat::expect_equal(
    fit$pdf, dbeta(seq(0, 1, length.out = 100), fit$shape1, fit$shape2)
  )
  testthat::expect_error(env$fit.beta(numeric()), "zero length")
  testthat::expect_equal(env$overlap.beta(f, f)$ol, 1, tolerance = 1e-6)
  for (name in c("best.fit.beta", "best.fit.pois", "fit.gamma")) {
    testthat::expect_output(result <- env[[name]](data.frame(Rsp = c(-1, -2))), "negative")
    testthat::expect_match(result, "negative")
  }
  testthat::expect_error(env$beta.glm(numeric(), f), "zero length")
  xs <- 0:4
  pdf.f <- c(0.1, 0.2, 0.4, 0.2, 0.1)
  pdf.b <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  testthat::expect_equal(env$q.overlap(pdf.f, pdf.b, xs, 2), 0.8)
  testthat::expect_equal(env$q.overlap(pdf.f, pdf.b, xs, 1, "upper"), 0.8)
  testthat::expect_equal(env$q.overlap(pdf.f, pdf.b, xs, 3, "lower"), 0.8)
  testthat::expect_equal(env$ol.gamma(xs, 2, 1, 2, 1), dgamma(xs, 2, 1))
  testthat::expect_equal(env$ol.nbin(xs, 2, 1, 2, 1), dnbinom(xs, size = 2, mu = 1))
  testthat::expect_equal(env$ol.beta(xs / 4, 2, 2, 2, 2), dbeta(xs / 4, 2, 2))
  testthat::expect_equal(env$prop.ol.gamma(xs, 2, 1, 2, 1)[[1]], 1, tolerance = 1e-6)
  testthat::expect_equal(env$prop.ol.beta(xs, 2, 2, 2, 2)$ol, 1, tolerance = 1e-6)
  # Continuous integration samples non-integers; preserve the result and warning.
  testthat::expect_warning(
    result <- env$prop.ol.nbin(xs, 2, 1, 2, 1),
    "non-integer"
  )
  testthat::expect_true(is.finite(result[[1]]) && result[[1]] >= 0 && result[[1]] <= 1)
  testthat::expect_error(env$q.overlap.no.cls("Bet", f = f, b = f), "wrong function")
})

testthat::test_that("bootstrap and permutation helpers preserve seeded samples", {
  env <- core_functions_environment()
  x <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.7, 0.9)
  y <- x + 0.05
  for (calculate in list(
    function() env$boot.diff(x, y, boot.reps = 12),
    function() env$boot.qual.diff(x, y, "Bet", boot.reps = 12),
    function() env$means.test.boot(x, y, boot.reps = 12),
    function() env$uni.test.boot(x, 0.5, reps = 12),
    function() env$boot.qual.beta.fixed(x, 0.5, boot.reps = 5, perm.reps = 12)
  )) {
    set.seed(42)
    first <- calculate()
    set.seed(42)
    testthat::expect_identical(calculate(), first)
    set.seed(43)
    testthat::expect_false(identical(calculate(), first))
  }
  testthat::expect_identical(env$boot.cls(rep(0.5, 5)), list(boot.lcl = 0.5, boot.ucl = 0.5))
  testthat::expect_equal(env$boot.diff(rep(1, 5), rep(2, 5), 12)$d.sd, 0)
  p <- env$means.test.perm(x, y, seed = 42)
  testthat::expect_identical(env$means.test.perm(x, y, seed = 42), p)
  testthat::expect_true(p >= 0 & p <= 1)
  for (name in c("boot.qual.beta", "boot.overlap.beta")) {
    calculate <- function(seed) env[[name]](x, y, boot.reps = 5, seed = seed, set.seed = TRUE)
    first <- calculate(42)
    testthat::expect_identical(calculate(42), first)
    testthat::expect_length(first$boot.q, 5)
    testthat::expect_false(identical(calculate(43)$boot.q, first$boot.q))
  }
  env$perm.reps <- 12
  x <- c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  y <- x + 1
  for (name in c("boot.overlap", "boot.overlap.diff")) {
    calculate <- function(seed) {
      if (name == "boot.overlap") {
        env[[name]]("NBin", x, y, xs = 0:100, boot.reps = 5, seed = seed, set.seed = TRUE)
      } else {
        env[[name]]("NBin", f = x, b = y, xs = 0:100, boot.reps = 5,
          seed = seed, set.seed = TRUE, q.tr.logit.inverse = FALSE
        )
      }
    }
    first <- calculate(42)
    testthat::expect_identical(calculate(42), first)
    testthat::expect_false(identical(calculate(43), first))
  }
  result <- env$q.overlap.no.cls("NBin", f = x, b = x, xs = 0:100)
  testthat::expect_equal(result$pdf.f, result$pdf.b)
})

testthat::test_that("numerical collaborators resolve in their defining environment", {
  env <- core_functions_environment()
  caller <- new.env(parent = env)
  caller$u.beta <- function(...) stop("Caller workspace must not supply u.beta")
  testthat::expect_equal(evalq(u.beta.buffered(0.5, buffer = 0.02), caller), 0)
  env$u.beta <- function(...) 0.25
  testthat::expect_equal(evalq(u.beta.buffered(0.5, buffer = 0.02), caller), 0.25)
  env$beta.transform <- function(...) stop("Local beta transformation")
  testthat::expect_error(env$fit.beta(c(0.2, 0.8)), "Local beta transformation")
})

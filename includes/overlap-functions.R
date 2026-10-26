q.overlap <- function(pdf.f, pdf.b, xs, b.mean, test.tail = NULL) {
  ###################################################
  # Returns quality of overlap of focal and benchmark pdfs, with correction for
  # one-tailed comparisons. If test.tail="both", q.overlap=overlap.
  # If test.tail="upper", q.overlap is 1 - area of pdf.f>pdf.b, over x>mean(b).
  # If test.tail="lower", q.overlap is 1 - area of pdf.f> pdf.b, over x<b.mean
  #
  # NOT FOR BETA DISTRIBUTION
  #
  # Parameters:
  # 		pdf.f			focal pdf vector
  # 		pdf.b		benchmark pdf vector
  # 		xs				vector of x values of the pdfs
  # 		b.mean	Mean bm value
  # 		test.tail	Tails to be compared: "both", "upper", "lower"
  #
  # Returns: area of overlap  or f>b, as described above
  #
  # Requirements: pdf.f, pdf.b, and xs must be vectors of equal length,
  # 		with identical indexes, such that pdf.f[i] represents the value of
  # 		pdf.f at xs[i], and same for pdf.b
  ###################################################

  # get default value of test tail
  if (is.null(test.tail)) test.tail <- "both"

  # Calculate overlap between actual pdfs
  if (test.tail == "upper") {
    upper <- which(xs > b.mean)
    xs.up <- xs[upper]
    pdf.b.up <- pdf.b[upper]
    pdf.f.up <- pdf.f[upper]
    pdf.diff <- pdf.f.up - pdf.b.up
    pdf.diff[pdf.diff < 0] <- 0
    area.diff <- sum(pdf.diff * mean(diff(xs.up)), na.rm = T)
    q.overlap <- 1 - area.diff
  } else if (test.tail == "lower") {
    lower <- which(xs < b.mean)
    xs.low <- xs[lower]
    pdf.b.low <- pdf.b[lower]
    pdf.f.low <- pdf.f[lower]
    pdf.diff <- pdf.f.low - pdf.b.low
    pdf.diff[pdf.diff < 0] <- 0
    area.diff <- sum(pdf.diff * mean(diff(xs.low)), na.rm = T)
    q.overlap <- 1 - area.diff
  } else {
    # test.tail=="both"
    q.overlap <- sum(pmin(pdf.f, pdf.b) * mean(diff(xs)), na.rm = T)
  }

  return(q.overlap)
}

q.overlap.no.cls <- function(distn, test.tail = NULL, f, b, xs = NULL, ...) {
  ###################################################
  # Calculates quality of focal vs. benchmark vector, based on the
  # overlap (ol) between two probability distributions. Includes correction
  # for one-tailed comparisions. NOT YET READY FOR BETA DISTRIBUTION
  #
  # Parameters:
  # 		1. distn				Distribution family ('NBin','gamma','Bet')
  # 		2. f					focal vector
  # 		3. b				benchmark vector
  # 		4. xs					Vector of x values for generating predicted probabilities
  # 									[Default=0 -> highest value of f, b, plus 10]
  # 		5. Standard optional arguments specific to fitdistr():
  # 			(a) nbin: no additional arguments needed
  # 			(b) beta: shape1 & shape2 (starting parameters ), lowlim (lower limit)
  # 			(c) gamma: shape & rate (starting parameters ), lowlim (lower limit)
  #
  # Returns list object:
  # 		1. Observed ol
  # 		2. Saved x values (xs)
  # 		3. pdf.f: Fitted focal pdf
  # 		4. pdf.b: Fitted benchmark pdf
  ###################################################

  maxTries <- 10 # Max time to try fit before reporting error

  # Set default values for optional parameters & check for errors
  # Set default values for optional parameters & check for errors
  if (is.null(test.tail)) test.tail <- "both"
  if (is.null(xs)) xs <- seq(0, 1, length.out = 100)
  # if (is.null(perm.reps)) 	perm.reps <- 10000

  # Compose fitdistr optional arguments
  opt.args <- ""
  if (distn == "Bet") {
    stop(paste0("ERROR: wrong function for beta distribution!"))
  } else if (distn == "gamma") {
    # Set default gamma parameters
    lowlim <- 0.0000001
    shape <- 1
    rate <- 1

    # Distribution fitting parameters
    opt.args <- paste0(
      ", lower = c(", lowlim, ", ", lowlim, "), start=list(shape=", shape,
      ", rate=", rate, ")"
    )
    # opt.args <- paste0( ", lower = c(", lowlim, ", ",  lowlim, "), upper = c(", uplim, ", ",
    # uplim, "),  start=list(shape=", shape, ", rate=", rate, ")" )
    densfun <- "gamma"

    # Predicted pdf parameters
    predfun <- "dgamma"
    predpar1 <- "shape"
    predpar2 <- "rate"
  } else if (distn == "NBin") {
    # Distribution fitting parameters
    densfun <- "negative binomial"

    # Predicted pdf parameters
    predfun <- "dnbinom"
    predpar1 <- "size"
    predpar2 <- "mu"
  } else {
    stop(paste0("ERROR: distribution '", distn, "' not recognized"))
  }

  # Construct the final commands
  fit.f <- paste0("fitdistr(f, densfun='", densfun, "'", opt.args, ")")
  fit.b <- paste0("fitdistr(b, densfun='", densfun, "'", opt.args, ")")

  pdf <- paste0(predfun, "(xs, ", predpar1, "= mod$estimate[1], ", predpar2, "= mod$estimate[2])")

  # Get benchmark mean, used for one-tail overlap
  b.mean <- mean(b, na.rm = T)

  fail <- F # Assume success to start

  #########################
  # Fit focal pdf
  #########################
  tries <- 0
  done <- F
  while (done == F) {
    tries <- tries + 1
    mod <- tryCatch(eval(parse(text = fit.f)),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )
    if (mod[1] == "FAIL") {
      if (tries >= maxTries) {
        fail <- T
        break
      }
    } else {
      pdf.f <- eval(parse(text = pdf))
      pdf.f[pdf.f == Inf] <- NA
      done <- T
    }
  }

  #########################
  # Fit benchmark pdf
  #########################
  tries <- 0
  done <- F
  while (done == F && fail == F) {
    tries <- tries + 1
    mod <- tryCatch(eval(parse(text = fit.b)),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )
    if (mod[1] == "FAIL") {
      if (tries >= maxTries) {
        fail <- T
        break
      }
    } else {
      pdf.b <- eval(parse(text = pdf))
      pdf.b[pdf.b == Inf] <- NA
      done <- T
    }
  }

  if (fail == T) {
    ol <- NA
    pdf.f <- NA
    pdf.b <- NA
  } else {
    # Calculate overlap between actual pdfs
    ol <- q.overlap(
      pdf.f = pdf.f, pdf.b = pdf.b, xs = xs,
      b.mean = b.mean, test.tail = test.tail
    )
  }

  # Assemble results list & return
  ol.results <- list(ol, pdf.f, pdf.b)
  names(ol.results) <- c("ol.obs", "pdf.f", "pdf.b")
  return(ol.results)
}

ol.gamma <- function(x, s1, r1, s2, r2) {
  ###########################
  # Overlap vector - Gamma distribution
  ###########################

  f1 <- dgamma(x, shape = s1, rate = r1)
  f2 <- dgamma(x, shape = s2, rate = r2)
  return(pmin(f1, f2))
}

ol.nbin <- function(x, s1, mu1, s2, mu2) {
  ###########################
  # Overlap vector - Negative binomial
  #
  # DOESN'T WORK - USE GAMMA APPROX.
  ###########################

  f1 <- dnbinom(x, mu = mu1, size = s1)
  f2 <- dnbinom(x, mu = mu2, size = s2)
  return(pmin(f1, f2))
}

ol.beta <- function(x, s1, r1, s2, r2) {
  ###########################
  # Overlap vector - Beta distribution
  ###########################

  f1 <- dbeta(x, shape1 = s1, shape2 = r1)
  f2 <- dbeta(x, shape1 = s2, shape2 = r2)
  return(pmin(f1, f2))
}

prop.ol.gamma <- function(xs, s1, r1, s2, r2) {
  #####################################
  # Proportional overlap - Gamma distribution
  #
  # Requires: function ol.gamma
  #####################################

  # Calculate the area of overlap
  ol <- integrate(ol.gamma, 0, Inf, s1 = s1, r1 = r1, s2 = s2, r2 = r2)
  # ol <- integrate(ol.gamma, 0, Inf, s1=s1, s2=s2, r1=r1, r2=r2)
  ol.val <- ol$value
  ol.err <- ol$abs.error

  # Return the results as list
  result <- list(ol.val, ol.err)
  return(result)
}

prop.ol.nbin <- function(xs, s1, mu1, s2, mu2) {
  #####################################
  # Proportional overlap - Negative binomial distribution
  #
  # DOESN'T WORK - USE GAMMA APPROX.
  # Requires: function ol.gamma
  #####################################

  # Calculate the area of overlap
  ol <- integrate(ol.nbin, 0, Inf, s1 = s1, mu1 = mu1, s2 = s2, mu2 = mu2)
  ol.val <- ol$value
  ol.err <- ol$abs.error

  # Return the results as list
  result <- list(ol.val, ol.err)
  return(result)
}

prop.ol.beta <- function(xs, s1, r1, s2, r2) {
  #####################################
  # Proportional overlap - Beta distribution
  #
  # Requires: function ol.beta
  #####################################

  # Calculate the area of overlap
  ol <- integrate(ol.beta, 0, 1, s1 = s1, r1 = r1, s2 = s2, r2 = r2)
  ol.val <- ol$value
  ol.err <- ol$abs.error

  # Return the result
  result <- list(ol.val, ol.err)
  names(result) <- c("ol", "ol.err")
  return(result)
}

overlap.beta <- function(v1, v2, xs = NULL, shape1 = 0.1, shape2 = 0.1, lowlim = 0.0001) {
  ###################################################
  # Calculates overlap of two beta-distributed vectors
  #
  # Details: Calculates overlap only if both vectors are successfully
  # 		fit. If not fit, sets overlap=0, returns ol.fail=TRUE, and remaining
  # 		objects are returned as NA.
  #
  # Requires: fitdistr function from MASS package required by function
  # 		fit.beta, called by this one.
  #
  # Parameters:
  # 		1. v1				Vector 1
  # 		2. v2				Vector 2
  # 		3. xs				Vector of x values for generating predicted probabilities
  # 								over interval [0,1]. [Default supplied below]
  # 		4. shape1		Beta shape parameter 1
  # 		5. shape1		Beta shape parameter 1
  # 		6. lowlim			Lower limit to prevent optimization going out of bounds
  #
  # Returns:
  # 		List object containing:
  # 		ol			Proportional overlap between the fitted distributions
  # 		pdf1		Fitted distribution 1
  # 		pdf2		Fitted distribution 2
  ###################################################

  # Transform if any values==1 or 0
  # Throws error if (a) outside interval [0,1], (b) vector is null,
  # or (c) vector has <2 values
  # v1 <- beta.transform( v1 )
  # v2 <- beta.transform( v2 )

  # # Check for too few values
  # msg_err <- "ERROR: Vector has only one value!"
  # if ( length( unique(v1) )<2 || length( unique(v2) )<2 ) stop(msg_err)

  # Set default values for optional parameters & check for errors
  if (is.null(xs)) xs <- seq(0, 1, length.out = 100)

  ###########################################
  # Fit v1 pdf & get shape parameters for overlap
  ###########################################

  # Start by assuming everything works
  fit.fail <- FALSE
  ol.fail <- FALSE

  # Initialize fitted pdfs and their parameters
  pdf1 <- NA
  v1.shp1 <- NA
  v1.shp2 <- NA

  pdf2 <- NA
  v2.shp1 <- NA
  v2.shp2 <- NA

  fit <- fit.beta(v1, xs = xs, shape1 = shape1, shape2 = shape2, lowlim = lowlim, boot.cls = F)

  if (fit$fail == TRUE) {
    fit.fail <- TRUE
    ol.fail <- TRUE
  } else {
    pdf1 <- fit$pdf
    v1.shp1 <- fit$shape1
    v1.shp2 <- fit$shape2

    ###################################
    # Fit v2 pdf
    ###################################

    fit <- fit.beta(v2, xs = xs, shape1 = shape1, shape2 = shape2, lowlim = lowlim, boot.cls = F)

    if (fit$fail == T) {
      fit.fail <- TRUE
      ol.fail <- TRUE
    } else {
      pdf2 <- fit$pdf
      v2.shp1 <- fit$shape1
      v2.shp2 <- fit$shape2

      ###################################
      # Calculate overlap between pdfs using
      # exact calculus method
      # Wrapped in error catcher as well
      ###################################
      # ols <- prop.ol.beta( xs=xs, s1=v1.shp1, r1=v1.shp2, s2=v2.shp1, r2=v2.shp2 )
      ols <- tryCatch(
        prop.ol.beta(xs = xs, s1 = v1.shp1, r1 = v1.shp2, s2 = v2.shp1, r2 = v2.shp2),
        error = function(cond) {
          return("FAIL")
        },
        warning = function(cond) {
          return("FAIL")
        }
      )
      if (ols[1] == "FAIL") {
        ol.fail <- T
      } else {
        ol <- ols$ol
      }
    }
  }

  if (ol.fail == T) {
    ol <- 0
  }

  result <- list(ol, ol.fail, fit.fail, pdf1, pdf2, v1.shp1, v1.shp2, v2.shp1, v2.shp2)
  names(result) <- c(
    "ol", "ol.fail", "fit.fail",
    "pdf1", "pdf2",
    "v1.shp1", "v1.shp2",
    "v2.shp1", "v2.shp2"
  )

  return(result)
}

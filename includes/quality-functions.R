q.linear <- function(x, x1, x0) {
  #############################################
  # Converts x to q (quality) along domain ( x0 ... x1 ), where
  # x0 is the value of x @ q=0, and x1 is the value of x @ q=1.
  # x must fall between x0 and x1
  #
  # This is a simple linear conversion; send to function
  # q.logistic to apply sigmoid scaling
  ##############################################

  # Error checks
  if (x0 == x1) {
    stop("Error in function q.raw: x0 and x1 cannot be equal!")
  } else if (x0 < x1) {
    if (x < x0 || x > x1) stop("Error in function q.raw: x must be between x0 and x1!")
  } else {
    # x0 > x1
    if (x > x0 || x < x1) stop("Error in function q.raw: x must be between x0 and x1!")
  }

  # Do the calculations
  diff <- abs(x - x1)
  maxdiff <- abs(x1 - x0)
  q <- 1 - diff / maxdiff

  return(q)
}

q.logistic <- function(q.raw, k = 10) {
  ###################################################
  # Converts linear score q.raw on domain [0,1]  to transformed
  # logistic score on same domain.
  #
  # q.raw MUST be in domain [0,1]
  # k is steepness parameter for the logistic curve. Optimum
  # setting depend on the domain. Default of k=10 works well
  # over domain [0,1], but feel free to tinker.
  ###################################################

  # Error checks
  if (q.raw > 1 || q.raw < 0) stop("Error in function q.logistic: q.raw must be <=1 & >=0")

  L <- 1 # Upper limit of logistic growth curve
  # By definition=1 for quality coefficient
  x0 <- 0.5 # Midpoint of the curve; by definition=0.5 over domain [0,1]

  # The logistic equation
  q <- L / (1 + exp(-k * (q.raw - x0)))

  return(q) # Return the quality coefficient
}

q.inverse.logit <- function(q, beta = 3) {
  ###################################################
  # Similar to q.lostistic but output is anchored on domain [0,1] (i.e.,
  # input-->output: 0-->0 and 1-->1)
  #
  # q MUST be in domain [0,1], beta must be >0
  # Source:
  # https://stats.stackexchange.com/a/289477/30044
  ###################################################

  if (is.null(beta)) beta <- 3

  # Error checks
  if (beta <= 0) stop("Error in function q.inverse.logit: beta must be >0")
  if (q > 1 || q < 0) stop("Error in function q.inverse.logit: q must be <=1 & >=0")

  q <- 1 / (1 + (q / (1 - q))^(beta * -1))

  return(q) # Return the quality coefficient
}

q.scaled.beta <- function(q, s1 = 0.3, s2 = 1) {
  ###################################################
  # Converts linear score q on domain [0,1]  to transformed score q.tr
  # on same domain using beta function
  #
  # q MUST be in domain [0,1]
  # Default values of s1 and s2 cause q.tr to drop steeply
  # from 1. Feel free to tinker
  ###################################################

  # Error checks
  if (q > 1 || q < 0) stop("Error in function q.scaled.beta: q must be <=1 & >=0")

  q.tr <- qbeta(q, s1, s2)

  return(q.tr)
}

beta.wt <- function(x, sh1 = 0.2, sh2 = 2) {
  ######################################
  # Transforms value on [0,1] to new values on [0,1]
  # equal to 1 at limits (0 or 1) and dropping rapidly to zero
  # in middle of range. Use for weighting means-based
  # quality, which should only be used when one or
  # both means are close to the edge of the distribution
  ######################################

  if (x >= 0.5 && x <= 1) {
    x.tr <- x - (1 - x)
  } else if (x < 0.5 && x >= 0) {
    x.tr <- (1 - x) - (1 - (1 - x))
  } else {
    stop("input value outside domain [0,1]!")
  }
  half.wt <- qbeta(x.tr, sh1, sh2)

  return(half.wt)
}

u.beta <- function(x, sh1 = 0.2, sh2 = 1.25) {
  ######################################
  # Transforms value on [0,1] to new values on [0,1]
  # which equals 1 at 0 or 1 and drops rapidly to zero
  # in middle of range. Use for weighting means-based
  # quality, which should only be used when one or
  # both means are close to the edge of the distribution
  ######################################

  if (x >= 0.5 && x <= 1) {
    x.tr <- x - (1 - x)
  } else if (x < 0.5 && x >= 0) {
    x.tr <- (1 - x) - (1 - (1 - x))
  } else {
    stop("input value outside domain [0,1]!")
  }
  u.beta <- qbeta(x.tr, sh1, sh2)
  return(u.beta)
}

u.beta.buffered <- function(x, buffer = null, sh1 = 0.2, sh2 = 1.25) {
  ######################################
  # Transforms input value x using u.beta function
  # (see above for details), but compressed on either
  # end such that x.tr reaches max value of one at
  # values of x < buffer or > 1-buffer
  ######################################

  if (buffer < 0 || buffer > 0.1) stop("Value of 'buffer' outside domain of [0,0.1]!")

  if (is.null(buffer)) {
    buffer <- 0.02
  } else if (buffer < 0 || buffer > 0.5) {
    stop("Value of 'buffer' outside domain of [0,0.5]!")
  }
  lowlim <- buffer
  uplim <- 1 - lowlim

  if (x >= 0 && x < lowlim) {
    x.tr <- 0
  } else if (x >= lowlim && x <= uplim) {
    x.tr <- 0.5 + (((x - 0.5) / (uplim - 0.5)) * 0.5)

    # Fix rare cases where transformation goes
    # slightly out of bounds
    if (x.tr < 0) {
      x.tr <- 0
    } else if (x.tr > 1) {
      x.tr <- 1
    }
  } else if (x > uplim && x <= 1) {
    x.tr <- 1
  } else {
    stop("input value outside domain [0,1]!")
  }

  u.beta.buffered <- u.beta(x.tr, sh1 = sh1, sh2 = sh2)
  return(u.beta.buffered)
}

qc.calc <- function(x, x.min, x.max, lcl, ucl, tail, k) {
  ###################################################
  # Converts value x into a score between 0 and 1 using logistic function.
  # Value x must fall between x.min and x.max.
  # If tail='lower', qc drops from 1 at x.max to (near) zero at x.min
  # If tail='upper', qc drops from 1 at x.min to (near) zero at x.max
  # k is the steepness function for the logistic curve. The optimum
  # setting depend on the domain. Until I can figure out how to
  # determine this algorithmically, this will have to be tinkered
  # with and passed as a parameter
  ###################################################

  y <- NA # The quality coefficient
  L <- 1 # Upper limit of logistic growth curve
  # By definition=1 for quality coefficient

  if (tail == "upper" | (tail == "both" & x > ucl)) {
    # Calculate score based on upper tail
    if (x > x.max) x == x.max # Shouldn't be necessary; just in case
    x0 <- (x.max - ucl) / 2 # Midpoint of the curve
    x.diff <- x - ucl # x distance from ucl
    y <- L / (1 + exp(-k * (x.diff - x0))) # The logistic equation
    y <- 1 - y # So curve increases toward ucl
  } else if (tail == "lower" | (tail == "both" & x < lcl)) {
    # Calculate score based on lower tail
    x0 <- (lcl - x.min) / 2 # Midpoint of the curve

    y <- L / (1 + exp(-k * (x - x0))) # The logistic equation
  }

  return(y) # Return the quality coefficient
}

qc.calc.simple <- function(x, x.min, x.max, k, transform = T) {
  ###################################################
  # Converts value x into a score between 0 and 1 using logistic function.
  # Value x must fall between x.min and x.max.
  # k is the steepness function for the logistic curve. The optimum
  # setting depend on the domain. For x=(0:1), k=10 works well
  # Optional parameter transform=F does simple linear transform
  ###################################################

  if (!is.na(x)) {
    if (x > x.max | x < x.min) {
      y <- NA
    } else if (transform == F) {
      # Simple linear transformation
      # Convert scale y linearly with x
      y <- (x - x.min) / (x.max - x.min)
    } else if (x == x.min) {
      y <- 0
    } else if (x == x.max) {
      y <- 1
    } else {
      # Logistic transformation

      # x >= x.min & x <= x.max
      L <- 1 # Upper limit of logistic growth curve
      # By definition=1 for quality coefficient

      # Calculate score based on lower tail
      x0 <- (x.max - x.min) / 2 # Midpoint of the curve

      y <- L / (1 + exp(-k * (x - x0))) # The logistic equation
    }

    return(y) # Return the quality coefficient
  }
}

qc.calc.simple.notransform <- function(x, x.min, x.max, k) {
  ###################################################
  # No conversion!
  ###################################################

  if (!is.na(x)) {
    if (x > x.max | x < x.min) {
      y <- NA
    } else {
      Y <- x
    }

    return(y) # Return the quality coefficient
  }
}

qual.prop <- function(f, b) {
  ###################################################
  # Calculates quality based on difference between means of two
  # vectors of proportions
  #
  # Parameters:
  # 		f: 	focal vector
  # 		b: benchmark vector
  #
  # Returns: Quality (proportion) determined by 1 minus difference
  # 	between 	f and b divided by distance between b and limit
  # (either 0 or 1) over interval bounded by b and limit and
  # containing f. Discount method is thus linear.
  ###################################################

  b.max <- max(b)
  b.min <- min(b)
  f.max <- max(f)
  f.min <- min(f)

  # Check f and b in bounds
  if (b.min < 0 || b.max > 1 || f.min < 0 || f.max > 1) {
    stop("ERROR: f and/or b not a proportion!")
  }

  f.mean <- mean(f, na.rm = T)
  b.mean <- mean(b, na.rm = T)

  if (f.mean == b.mean) {
    q <- 1
  } else {
    # linear discount method
    d.abs <- abs(f.mean - b.mean)

    # Max distance between b.mean and distn. boundary
    d.max <- max(b.mean, 1 - b.mean)
    q <- 1 - (d.abs / d.max)
  }

  return(q)
}

qual.prop.fixed.b <- function(
  f, b.fixed, perm.reps = 1000, test.diff = NULL,
  use.q.diff.raw = FALSE, discount.method = "linear"
) {
  ###################################################
  # Calculates quality based on difference between means of focal
  # vector and fixed bm value
  #
  # Parameters:
  # 		f: 	focal vector
  # 		b.fixed: fixed benchmark value
  # 		discount.method: 	linear (default; 1:1) or
  # 										scaled (q declines exponentially toward
  # 										zero)
  #
  # Returns: Quality (proportion) determined by 1 minus difference
  # 	between 	f and b divided by distance between b and limit
  # (either 0 or 1) over interval bounded by b and limit and
  # containing f.
  ###################################################

  if (is.null(test.diff)) test.diff <- TRUE

  f.mean <- mean(f, na.rm = T)
  f.max <- max(f, na.rm = T)
  f.min <- min(f, na.rm = T)
  d.abs <- abs(f.mean - b.fixed)
  p <- NA

  # Check f and b in bounds
  if (b.fixed < 0 || b.fixed > 1 || f.min < 0 || f.max > 1) {
    stop("ERROR: f and/or b.fixed not a proportion!")
  }

  # Get p value of test H0: f.mean==b.fixed
  if (test.diff == TRUE) {
    test <- uni.test.boot(v = f, val = b.fixed, reps = perm.reps)
    p <- test$p
    if (d.abs == 0) p <- 1 # Adjust for erroneous p when f.mean=b.fixed
  }

  if (f.mean == b.fixed) {
    q <- 1
  } else {
    # Max distance between b.mean and distn. boundary
    d.max <- max(b.fixed, 1 - b.fixed)
    q <- 1 - (d.abs / d.max)
  }

  # Save q before any adjustments
  q.raw <- q

  # Reset q to 1 if diff not significant and not forcing use of raw value
  if (test.diff == TRUE && use.q.diff.raw == FALSE && p >= 0.05) q <- 1

  # Rescale q if requested
  if (discount.method == "scaled") q <- q.scaled.beta(q)

  # Assemble results & return
  results <- list(q, q.raw, p, d.abs)
  names(results) <- c("q", "q.raw", "p", "d.abs")
  return(results)
}

qual.prop.fixed <- function(f, b) {
  ###################################################
  # Calculates quality of a single focal value relative to a fixed bm
  # value, where both are proportions
  #
  # Parameters: two proportions (over interval [0,1])
  # Returns: Quality (proportion) determined by 1 minus difference
  # 	between 	f and b divided by distance between b and limit
  # (either 0 or 1) over interval bounded by b and limit and
  # containing f.
  ###################################################

  # First check f and b in bounds
  if (b < 0 || b > 1 || f < 0 || f > 1) stop("ERROR: f and/or b not a proportion!")

  if (f == b) {
    q <- 1
  } else if (f > b) {
    q <- 1 - (f - b) / (1 - b)
  } else {
    q <- f / b
  }

  return(q)
}

qual.prop.fixed.f <- function(f, b) {
  ###################################################
  # Calculates quality of a fixed focal value (f) relative to a beta-
  # distribution vector of benchmark values (b). f must be
  # a proportion over [0,1]
  #
  # Quality is calculated based on bootstrapped 95% CLs. Q=q00%
  # when lcl >= f <= ucl. Outside the CLs, Q decreases linearly to
  # 0 at lower limit (0) or upper limit (1).
  #
  # Parameters: two proportions (over interval [0,1])
  #
  # Returns: Quality (proportion) determined by 1 minus difference
  # 	between 	f and b divided by distance between b and limit
  # (either 0 or 1) over interval bounded by b and limit and
  # containing f.
  ###################################################

  # First check f and b in bounds
  if (min(b) < 0 || max(b) > 1 || f < 0 || f > 1) stop("ERROR: f and/or b not proportions!")

  # Get CLs of b dist
  dist.bm <- fit.beta(b,
    xs = xs, shape1 = shape1, shape2 = shape2,
    lowlim = lowlim, boot.cls = T
  )
  ucl <- dist.bm$boot.ucl
  lcl <- dist.bm$boot.lcl

  if (f >= lcl && f <= ucl) {
    q <- 1
  } else if (f > ucl) {
    q <- 1 - (f - ucl) / (1 - ucl)
  } else {
    q <- f / lcl
  }

  return(q)
}

qual.beta.zoib <- function(f, b, xs = NULL, shape1 = 0.1, shape2 = 0.1, lowlim = 0.0001) {
  ###################################################
  # Calculates quality of a beta-distributed indicator, with support for
  # zero- and one-inflation.
  #
  # Accepts vectors of focal (f) and benchmark (b) values. These
  # are split into separate vectors of zeros, ones and the remainder.
  # Quality of the zero fraction compares the f proportion of zeros relative
  # to the (expected) b fraction of zeros. Quality of the one fraction is
  # analogous. Quality of the beta fraction is the overlap of the fitted beta
  # pdfs. Final overal quality is the sum of the zero, one and beta qualities,
  # with each weighted by the proportion of beta values in that fraction.
  #
  # Parameters:
  # 		1. f					Vector of focal EI values
  # 		2. b				Vector of benchmark EI
  # 		3. xs				Vector of x values for generating predicted probabilities
  # 								over interval [0,1]. [Default supplied below]
  # 		4. shape1		Beta shape parameter 1
  # 		5. shape1		Beta shape parameter 1
  # 		6. lowlim			Lower limit to prevent optimization going out of bounds
  #
  # Returns list object consisting of final quality, overlap, the fitted
  # 		pdfs, plus the split vectors and all intermediate values.
  ###################################################

  # First check vectors are distributed on [0,1]
  msg_err <- "ERROR: Vector has values outside interval [0,1]!"
  if (any(f > 1) || any(f < 0)) stop(msg_err)
  # First check that vector is distributed on [0,1]
  if (any(b > 1) || any(b < 0)) stop(msg_err)

  # Set default values for optional parameters & check for errors
  if (is.null(xs)) xs <- seq(0, 1, length.out = 100)

  ###################################
  # Get quality and weightings for zero- and
  # one- fractions, if any
  ###################################

  # Split the focal vector
  split <- split.zoib(f)
  f0 <- split$v0
  f1 <- split$v1
  f.bet <- split$v.beta
  p.f0 <- split$p0
  p.f1 <- split$p1
  p.f.bet <- split$p.beta

  # Split the benchmark vector
  split <- split.zoib(b)
  b0 <- split$v0
  b1 <- split$v1
  b.bet <- split$v.beta
  p.b0 <- split$p0
  p.b1 <- split$p1
  p.b.bet <- split$p.beta

  ####################################
  # Calculate raw quality for each focal vector
  #
  # Raw quality is proportional overlap with the
  # benchmark fractions
  ####################################

  q.f0 <- min(p.b0, p.f0)
  q.f1 <- min(p.b1, p.f1)
  f.bet.w <- min(p.b.bet, p.f.bet) # i.e., beta weighting factor

  ################################
  # Quality of beta fraction
  #
  # First handle special cases where one or
  # both of the vectors are zero length or
  # have < 2 values. Then apply overlap method.
  ###################################

  # Sizes and values of beta vectors, for handling special cases
  n.f.bet <- length(f.bet)
  n.b.bet <- length(b.bet)
  vals.f.bet <- length(unique(f.bet))
  vals.b.bet <- length(unique(b.bet))

  # Start by assuming overlap method not used
  # Therefore following variables will be NA
  ol <- NA
  pdf.f <- NA
  pdf.b <- NA

  if (n.f.bet == 0 || n.b.bet == 0) { # START Beta quality
    q.f.bet <- 0
  } else if (vals.f.bet == 1) {
    # Fixed focal value
    f.fixed <- as.numeric(unique(f.bet))
    if (vals.b.bet == 1) {
      b.fixed <- as.numeric(unique(b.bet))
      q.f.bet <- qual.prop.fixed(f.fixed, b.fixed)
    } else {
      q.f.bet <- qual.prop.fixed.f(f.fixed, b.bet)
    }
  } else if (vals.b.bet == 1) {
    # Fixed beta value
    b.fixed <- as.numeric(unique(b.bet))
    q.f.bet <- qual.prop.fixed.b(f.bet, b.fixed)
  } else {
    #######################################
    # Use overlap method
    #######################################

    fit.ol <- overlap.beta(f.bet, b.bet,
      xs = xs,
      shape1 = shape1, shape2 = shape2, lowlim = lowlim
    )
    ol <- fit.ol$ol
    pdf.f <- fit.ol$pdf1
    pdf.b <- fit.ol$pdf2
    q.f.bet <- ol
  } # END Beta quality

  # Calculate weighted beta fraction quality
  q.f.bet.w <- q.f.bet * f.bet.w

  # Calculate overall quality
  q <- q.f0 + q.f1 + q.f.bet.w

  # Assemble list of results & return
  results <- list(
    q, ol,
    b0, p.b0, f0, p.f0, q.f0,
    b1, p.b1, f1, p.f1, q.f1,
    b.bet, p.b.bet, f.bet, p.f.bet, q.f.bet, f.bet.w, q.f.bet.w,
    pdf.f, pdf.b
  )
  names(results) <- c(
    "q", "ol",
    "b0", "p.b0", "f0", "p.f0", "q.f0",
    "b1", "p.b1", "f1", "p.f1", "q.f1",
    "b.bet", "p.b.bet", "f.bet", "p.f.bet", "q.f.bet", "f.bet.w", "q.f.bet.w",
    "pdf.f", "pdf.b"
  )
  return(results)
}

qual.beta.orig <- function(
  f, b, xs = NULL, test.diff = FALSE, perm.reps = NULL, shape1 = 0.1,
  shape2 = 0.1, lowlim = 0.0001, use.q.diff.raw = FALSE, discount.method = NULL
) {
  ###################################################
  # Calculates overall quality for beta-distributed focal indicator vector,
  # relatve to benchmark vector, based on difference of means &
  # weighted by distribution overlap
  #
  # Vectors f & b must be distributed on [0,1]. Runs permutation test
  # only if test.diff=T. Recommend test.diff=FALSE if running inside
  # bootstrap to avoid excessive run times.
  #
  # Returns list object: f.mean, b.mean, diff, diff.p, q.diff, q.ol, q
  ###################################################

  # Set default values for optional parameters & check for errors
  if (is.null(xs)) xs <- seq(0, 1, length.out = 100)
  if (is.null(perm.reps)) perm.reps <- 10000
  if (is.null(discount.method)) discount.method <- "linear"

  f.mean <- mean(f, na.rm = T)
  f.w <- min(f.mean, 1 - f.mean) # Focal weight
  b.mean <- mean(b, na.rm = T)
  b.w <- min(b.mean, 1 - b.mean) # Benchmark weight
  w <- f.w + b.w # final weighting factor

  diff <- f.mean - b.mean

  q.diff.raw <- qual.prop(f, b) # Difference-based quality

  # Set default values of adjusted quality and means test p-value
  q.diff.adj <- q.diff.raw
  diff.p <- NULL

  # Get p value from perm test & adjust q.diff if needed
  if (test.diff == T) {
    diff.p <- means.test.perm(x = f, y = b, iterations = perm.reps)
    if (diff.p >= 0.05) q.diff.adj <- 1 # set adj. q to 100% if no sig diff
  }

  # Set final value of q.diff
  if (use.q.diff.raw == TRUE) {
    q.diff <- q.diff.raw
  } else {
    q.diff <- q.diff.adj
  }

  # Get overlap if possible
  q.ol <- NA

  # Transform if any values==1 or 0
  # Throws error if (a) outside interval [0,1], (b) vector is null,
  # or (c) vector has <2 values
  f.tr <- beta.transform(f)
  b.tr <- beta.transform(b)
  len.u.f <- length(unique(f.tr))
  len.u.b <- length(unique(b.tr))

  # Add small amount of jitter on interval (0,1)
  # to one or both vectors to allow fitting
  if (len.u.f == 1) f.tr <- jitter.beta(f)
  if (len.u.b == 1) b.tr <- jitter.beta(b)

  ol.result <- overlap.beta(
    v1 = f.tr, v2 = b.tr, xs = xs,
    shape1 = shape1, shape2 = shape2, lowlim = lowlim
  )
  q.ol <- ol.result$ol
  pdf.f <- ol.result$pdf1
  pdf.b <- ol.result$pdf2
  fit.fail <- ol.result$fit.fail

  # Scale distance-based quality, if requested
  if (discount.method == "scaled") {
    q.diff <- q.scaled.beta(q.diff)
  }

  # Overall quality
  q <- (q.ol * w) + (q.diff * (1 - w))

  results <- list(
    f.mean, b.mean, diff, diff.p, q.diff.raw, q.diff, q.ol, q, w, pdf.f, pdf.b
  )
  names(results) <- c(
    "f.mean", "b.mean", "diff", "diff.p", "q.diff.raw", "q.diff", "q.ol", "q", "ol.w",
    "pdf.f", "pdf.b"
  )

  return(results)
}

qual.beta <- function(
  f, b, xs = NULL, test.diff = FALSE, perm.reps = NULL, shape1 = 0.1,
  shape2 = 0.1, lowlim = 0.0001, use.q.diff.raw = FALSE, discount.method = "linear",
  wt.method = "u.beta.buffered", buffer = 0.01
) {
  ###################################################
  # Calculates overall quality for beta-distributed focal indicator vector,
  # relatve to benchmark vector, based on difference of means &
  # weighted by distribution overlap
  #
  # Vectors f & b must be distributed on [0,1]. Runs permutation test
  # only if test.diff=T. Recommend test.diff=FALSE if running inside
  # bootstrap to avoid excessive run times.
  #
  # Returns list object: f.mean, b.mean, diff, diff.p, q.diff, q.ol, q
  ###################################################

  # Set default values for optional parameters & check for errors
  if (is.null(xs)) xs <- seq(0, 1, length.out = 100)
  if (is.null(perm.reps)) perm.reps <- 10000

  f.mean <- mean(f, na.rm = T)
  b.mean <- mean(b, na.rm = T)
  diff.mean.abs <- abs(f.mean - b.mean)

  # Calcuate half-weights for each mean
  if (wt.method == "linear") {
    # Linear decay (original method)
    # 100% overlap at 0.5, 100% means at 0 and 1, transitions
    # linearly between these points

    f.w <- min(f.mean, 1 - f.mean) # Focal weight
    b.w <- min(b.mean, 1 - b.mean) # Benchmark weight
    w <- f.w + b.w
  } else if (wt.method == "u.beta") {
    #  Reflected (U-shaped) beta weighting function
    # 100% overlap for most of beta mid-range
    # 100% means-based at 0 and 1 only
    # Smooth but steep transition at extreme values only
    # determined by beta function ([0.5:1]) and
    # reflected beta function ([0.5:0])

    f.w <- beta.wt(f.mean) / 2
    b.w <- beta.wt(b.mean) / 2
  } else if (wt.method == "u.beta.buffered") {
    # Edge-buffered reflected (U-shaped) beta weighting function
    # Similar to wt.method='u.beta', but beta U-function is pushed
    # inward, reaching 100% not at 0 and 1, but instead at lower limit
    # buffer and upper limit 1-buffer. Beyond buffer (values <= buffer
    # and >=1-buffer) stays at 100%.

    f.w <- u.beta.buffered(f.mean, buffer = buffer, sh1 = 0.2, sh2 = 2) / 2
    b.w <- u.beta.buffered(b.mean, buffer = buffer, sh1 = 0.2, sh2 = 2) / 2
  } else {
    stop("wt.method not valid (func: qual.beta)")
  }

  # Step correction for extreme values
  # Sets means weight to 100% if at least one mean very close to 0 or 1
  # and the other mean within 1% of the more extreme mean
  if ((f.mean < 0.001 || f.mean > 0.999) && diff.mean.abs < 0.01) f.w <- 0.5
  if ((b.mean < 0.001 || b.mean > 0.999) && diff.mean.abs < 0.01) b.w <- 0.5

  # Combine half-weights into final weight
  w <- 1 - (f.w + b.w)

  # Get means-based quality
  diff <- f.mean - b.mean
  q.diff.raw <- qual.prop(f, b) # Difference-based quality

  # Set default values of adjusted quality and means test p-value
  q.diff.adj <- q.diff.raw
  diff.p <- NULL

  # Get p value from perm test & reset q.diff to 1 if difference not sig.
  if (test.diff == T) {
    diff.p <- means.test.perm(x = f, y = b, iterations = perm.reps)
    if (diff.p >= 0.05) q.diff.adj <- 1 # set adj. q to 100% if no sig diff
  }

  # Set final value of q.diff
  if (use.q.diff.raw == TRUE) {
    q.diff <- q.diff.raw
  } else {
    q.diff <- q.diff.adj
  }

  # Get overlap if possible
  q.ol <- NA

  # Transform if any values==1 or 0
  # Throws error if (a) outside interval [0,1], (b) vector is null,
  # or (c) vector has <2 values
  f.tr <- beta.transform(f)
  b.tr <- beta.transform(b)
  len.u.f <- length(unique(f.tr))
  len.u.b <- length(unique(b.tr))

  # Add small amount of jitter on interval (0,1)
  # to one or both vectors to allow fitting
  if (len.u.f == 1) f.tr <- jitter.beta(f)
  if (len.u.b == 1) b.tr <- jitter.beta(b)

  ol.result <- overlap.beta(
    v1 = f.tr, v2 = b.tr, xs = xs,
    shape1 = shape1, shape2 = shape2, lowlim = lowlim
  )
  q.ol <- ol.result$ol
  pdf.f <- ol.result$pdf1
  pdf.b <- ol.result$pdf2
  fit.fail <- ol.result$fit.fail

  # Scale distance-based quality, if requested
  if (discount.method == "scaled") {
    q.diff <- q.scaled.beta(q.diff)
  }

  # Overall quality
  q <- (q.ol * w) + (q.diff * (1 - w))

  results <- list(
    f.mean, b.mean, diff, diff.p, q.diff.raw, q.diff, q.ol, q, w, pdf.f, pdf.b
  )
  names(results) <- c(
    "f.mean", "b.mean", "diff", "diff.p", "q.diff.raw", "q.diff", "q.ol", "q", "ol.w",
    "pdf.f", "pdf.b"
  )

  return(results)
}

qual.beta.ol <- function(f, b, xs = NULL, shape1 = 0.1, shape2 = 0.1, lowlim = 0.0001) {
  ###################################################
  # Calculates overall quality for beta-distributed focal indicator vector,
  # relatve to benchmark vector, based distribution overlap only.
  #
  # Vectors f & b must be distributed on [0,1].
  #
  # Returns list:
  # 	f.mean		Focal mean
  # b.mean		Benchmark mean
  # diff			Abs. difference f.mean, b.mean
  # q.ol			Overlap-based quality
  # pdf.f			Fitted focal distribution
  # pdf.b			Fitted benchmark distribution
  ###################################################

  # Variable for recording transformation method
  beta.tr.method <- ""

  # Set default values for optional parameters & check for errors
  if (is.null(xs)) xs <- seq(0, 1, length.out = 100)

  f.mean <- mean(f, na.rm = T)
  b.mean <- mean(b, na.rm = T)
  diff <- f.mean - b.mean

  # Get overlap
  q.ol <- NA

  # Get count of unique values
  len.u.f <- length(unique(f))
  len.u.b <- length(unique(b))

  #####################################
  # Handle special cases and calculate overlap
  #
  # Checks:
  # 	(a)	Values outside [0,1] (error)
  # 	(b)	b & f effectively identical
  # 	(c) 	Vector consists entirely of 0 or 1
  # 	(d)	Some values 0 or 1
  # 	(e) 	Mean very low (<0.01) but not all zeros
  # 	(f)		Single-value vectors on domain (0,1)
  #####################################

  f.tr <- f
  b.tr <- b

  if (any(f < 0) || any(f > 1) || any(b < 0) || any(b > 1)) {
    # (a) Outside beta domain
    # Report error and stop
    f.bad <- ""
    b.bad <- ""
    if (any(f < 0) || any(f > 1)) f.bad <- "f"
    if (any(b < 0) || any(b > 1)) b.bad <- "b"
    bad.vecs <- smartpaste(f.bad, b.bad, sep = ", ")
    msg.err <- paste0("ERROR: One or more value of ", bad.vecs, " outside domain [0,1]")
    stop(msg.err)
  } else if ((len.u.f == 1 && len.u.b == 1 && unique(f) == unique(b)) || (all(length(
    f
  ) == length(b)) && all(f == b))) {
    # (b) f & b are identical, or are effectively identical vectors of one or
    # more identical values
    # Can't be fit so just set overlap (quality) to 100% and skip the rest
    q.ol <- 1
    f.tr <- f
    pdf.f <- NA
    pdf.b <- NA
    beta.tr.method <- "effectively identical"
  } else {
    # Calculate overlap for the remainder, transforming as needed

    # Use these variable to stop further transformations as needed
    f.finished <- FALSE
    b.finished <- FALSE

    #########################################
    # (c) Transform all-zero and all-one vectors
    #
    # Notes:
    # 1. Converts to highly skewed true beta distribution with
    # 	expected value very close to 0 or 1, but on the domain
    # 	(0,1) to allow fitting and calculation of overlap.
    #########################################

    # Skew, mean & stdev parameters
    sh1_rskew <- 8
    sh2_rskew <- 0.01 # Right skewed mean near zero
    sh1_lskew <- 0.01
    sh2_lskew <- 8 # Left skewed mean near one
    sd_lep <- 0.01 # Highly leptokurtic
    sd_lep2 <- 0.05 # Less leptokurtic
    sd_lep3 <- 0.1 # Less leptokurtic
    mean_zero <- 0.0001 # Close to zero
    mean_one <- 0.9999 # Close to one
    s1r_01 <- 10
    s2r_01 <- 0.01
    s1l_01 <- 0.01
    s2l_01 <- 10

    # f all zeros or ones
    if (all(unique(f) == 0)) {
      # f all zeros; convert to right skewed, mean near 0.01
      f.tr <- pbeta(xs, s1r_01, s2r_01)
      beta.tr.method <- "focal all zeros"
      f.finished <- TRUE
    } else if (all(unique(f) == 1)) {
      # f all ones; convert to left skewed, mean near 0.99
      f.tr <- pbeta(xs, s1l_01, s2l_01)
      # f.tr <- beta.constrain(f.tr)
      # # f.tr <- beta.normal( f.tr, mn=mean_one,
      # # sdev=sd_lep, sh1=sh1_lskew, sh2=sh2_lskew
      # # )
      # f.tr <- noise.beta.norm(f.tr,length(f.tr),sd_lep)
      beta.tr.method <- "focal all ones"
      f.finished <- TRUE
    }

    # b all zeros or ones
    if (all(unique(b) == 0)) {
      # b all zeros; convert to right skewed, mean near 0.01
      b.tr <- pbeta(xs, s1r_01, s2r_01)
      beta.tr.method <- smartpaste(beta.tr.method, "bm all zeros", sep = ",")
      b.finished <- TRUE
    } else if (all(unique(b) == 1)) {
      # b all ones; convert to left skewed, mean near 0.99
      b.tr <- pbeta(xs, s1l_01, s2l_01)
      # b.tr <- beta.constrain(b.tr)
      # b.tr <- beta.normal( b.tr, mn=mean_one,
      # sh1=sh1_lskew, sh2=sh2_lskew, sdev=sd_lep
      # )
      beta.tr.method <- smartpaste(beta.tr.method, "bm all ones", sep = ",")
      b.finished <- TRUE
    }

    #########################################
    # (d) Some zero or ones (but not all)
    #########################################

    if (
      (f.finished == FALSE && (any(f.tr == 0) || any(f.tr == 1))) ||
        (b.finished <- FALSE && (any(b.tr == 0) || any(b.tr == 1)))
    ) {
      if (any(f.tr == 0) || any(f.tr == 1)) {
        # f.tr <- beta.transform( f.tr )	# traditional beta transform
        f.tr <- beta.constrain(f.tr) # Just squeezes 0s and 1s inside (0,1)
        beta.tr.method <- smartpaste(beta.tr.method, "f some zeros/ones", sep = ",")
      }
      if (any(b.tr == 0) || any(b.tr == 1)) {
        # b.tr <- beta.transform( b.tr )
        b.tr <- beta.constrain(b.tr)
        beta.tr.method <- smartpaste(beta.tr.method, "b some zeros/ones", sep = ",")
      }
    }

    ####################################################
    # (e) Mean very low (<0.01) but not all zeros
    # Convert to highly right-skewed distn as fitted distribution may be
    # highly leptokurtic and not overlapping smaller values closer to
    # zero
    ####################################################

    f.tr.mean <- mean(f.tr, na.rm = TRUE)
    b.tr.mean <- mean(b.tr, na.rm = TRUE)

    if (f.finished == FALSE && f.tr.mean < 0.1) {
      s1 <- 0.5
      s2 <- f.tr.mean
      f.tr <- pbeta(xs, s1, s2)
      beta.tr.method <- smartpaste(beta.tr.method, "f.mean<0.01", sep = ",")
      f.finished <- TRUE
    }
    if (b.finished == FALSE && b.tr.mean < 0.1) {
      s1 <- 0.5
      s2 <- b.tr.mean
      b.tr <- pbeta(xs, s1, s2)
      beta.tr.method <- smartpaste(beta.tr.method, "b.mean<0.01", sep = ",")
      b.finished <- TRUE
    }

    #########################################
    # (f) Single valued vectors on (0,1)
    #
    # Method:
    # 1.	Mean <=0.1: right-skewed beta dist with mean
    # 		equal to original mean
    # 2. 	Mean >=0.9: left-skewed beta dist with mean
    # 		equal to original mean
    # 3. 	The rest: add small amount of noise to allow fitting
    #########################################

    # Shape and sdev params for leptokurtic distn on
    # domain (0.1,0.9)
    sh1_mid <- 0.2
    sh2_mid <- 0.1
    sd_mid <- 0.01

    # Recalculate count of unique values
    len.u.f <- length(unique(f.tr))
    len.u.b <- length(unique(b.tr))

    # Focal singled-valued on (0,1)
    if (f.finished == FALSE && len.u.f == 1) {
      f.tr.mean <- mean(f.tr, na.rm = TRUE)
      if (f.tr.mean <= 0.1) {
        s1 <- 0.5
        s2 <- f.tr.mean
        f.tr <- pbeta(xs, s1, s2)
        beta.tr.method <- smartpaste(beta.tr.method, "f single value<=0.1", sep = ",")
      } else if (f.tr.mean >= 0.9) {
        s2 <- f.tr.mean
        s1 <- 0.01 + ((1 - f.tr.mean) * 0.1)
        f.tr <- pbeta(xs, s1, s2)
        beta.tr.method <- smartpaste(beta.tr.method, "f single value>=0.9", sep = ",")
      } else {
        # On domain (0.1,0.9)
        f.tr <- beta.normal(f.tr,
          mn = f.tr.mean,
          sh1 = sh1_mid, sh2 = sh2_mid, sdev = sd_mid
        )
        beta.tr.method <- smartpaste(beta.tr.method, "f single value on (0.1,0.9)", sep = ",")
      }
    }

    # Bm singled-valued on (0,1)
    if (b.finished == FALSE && len.u.b == 1) {
      b.tr.mean <- mean(b.tr, na.rm = TRUE)
      if (b.tr.mean <= 0.1) {
        s1 <- 0.5
        s2 <- b.tr.mean
        b.tr <- pbeta(xs, s1, s2)
        beta.tr.method <- smartpaste(beta.tr.method, "b single value<=0.1", sep = ",")
      } else if (b.tr.mean >= 0.9) {
        s2 <- b.tr.mean
        s1 <- 0.01 + ((1 - b.tr.mean) * 0.1)
        b.tr <- pbeta(xs, s1, s2)
        beta.tr.method <- smartpaste(beta.tr.method, "b single value>=0.9", sep = ",")
      } else {
        # On domain (0.1,0.9)
        b.tr <- beta.normal(b.tr,
          mn = b.tr.mean,
          sh1 = sh1_mid, sh2 = sh2_mid, sdev = sd_mid
        )
        beta.tr.method <- smartpaste(beta.tr.method, "b single value on (0.1,0.9)", sep = ",")
      }
    }

    # just to be safe
    f.tr <- beta.constrain(f.tr)
    b.tr <- beta.constrain(b.tr)

    #########################################
    # Calculate overlap (finally!)
    #########################################

    # Get overlap results
    ol.result <- overlap.beta(
      v1 = f.tr, v2 = b.tr, xs = xs,
      shape1 = shape1, shape2 = shape2, lowlim = lowlim
    )
    q.ol <- ol.result$ol
    pdf.f <- ol.result$pdf1
    pdf.b <- ol.result$pdf2
    # fit.fail <- ol.result$fit.fail
  }

  results <- list(
    f.mean, b.mean, diff, q.ol, pdf.f, pdf.b, beta.tr.method, f.tr, b.tr
  )
  names(results) <- c(
    "f.mean", "b.mean", "diff", "q.ol", "pdf.f", "pdf.b", "beta.tr.method", "f.tr", "b.tr"
  )

  return(results)
}

qual.beta.fixed <- function(f, b.fixed, boot.reps = NULL, test.diff = NULL) {
  ###################################################
  # Calculates overall quality for beta-distributed focal indicator
  # vector (f) relative to fixed benchmark value
  #
  # Vector f must be distributed on [0,1]. Runs permutation test
  # only if test.diff=T.
  #
  # Returns list object: f.mean, b.fixed, diff, diff.p, q.diff, q.ol, q
  ###################################################

  # Set defaults for optional parameters
  if (is.null(boot.reps)) boot.reps <- 1000
  if (is.null(test.diff)) test.diff <- TRUE

  f.mean <- mean(f, na.rm = T)
  f.w <- min(f.mean, 1 - f.mean) # Focal weight
  diff <- f.mean - b.fixed

  # Get p value of H0: f.mean==b.fixed
  if (test.diff == TRUE) {
    test <- uni.test.boot(v = f, val = b.fixed, reps = boot.reps)
    p.boot <- test$p
  } else {
    # Assume difference is significant, setting p to < 0.05
    # Only use this option to speed up bootstrap
    p.boot <- 0.05
  }

  if (diff == 0) p.boot <- 1 # Adjust for fact that bootstrap returns
  # erroneous p-value when f.mean=b.fixed

  # get difference-based quality
  qual <- qual.prop.fixed.b(f = f, b.fixed = b.fixed, test.diff = test.diff)
  q <- unlist(qual[1])

  # No adjustment applied, so q.diff.raw=q (adjusted)
  q.diff.raw <- q

  results <- list(
    f.mean, b.fixed, diff, p.boot, q, q.diff.raw
  )
  names(results) <- c(
    "f.mean", "b.fixed", "diff", "p.boot", "q", "q.diff.raw"
  )

  return(results)
}

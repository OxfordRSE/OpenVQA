boot.overlap <- function(
  distn, v1, v2, xs = NULL, boot.reps = NULL, seed = NULL,
  set.seed = F, ...
) {
  ###################################################
  # Calculates overlap (ol) & bootstrapped 95% CLs between two
  # probability distributions, as fit to two input vectors
  #
  # Parameters:
  # 		1. distn				Distribution family ('NBin','gamma','Bet')
  # 		2. v1					Vector 1
  # 		3. v2					Vector 2
  # 		4. xs					Vector of x values for generating predicted probabilities
  # 									[Default=0 -> highest value of v1, v2, plus 10]
  # 		5. boot.reps		Number of bootstap replicates [Default=1000]
  # 		6. seed				Randomization seed for fixed results [Default=no seed]
  # 		7. Standard optional arguments specific to fitdistr():
  # 			(a) nbin: no additional arguments needed
  # 			(b) beta: shape1 & shape2 (starting parameters ), lowlim (lower limit)
  # 			(c) gamma: shape & rate (starting parameters ), lowlim (lower limit)
  #
  # Returns list object:
  # 		1. Observed ol
  # 		2. Lower and upper 95% CLs for observed ol
  # 		3. Saved x values (xs)
  # 		4. Fitted pdf1
  # 		5. Fitted pdf2
  # 		6. Bootrapped prob density function values for vector1
  # 		7. Bootrapped prob density function values for vector2
  # 		8. Bootstrapped ol values
  #
  ###################################################

  # Set default values for optional parameters & check for errors
  if (is.null(xs)) {
    xs <- seq(0, max(max(f), max(b)) + 10)
  }

  if (is.null(boot.reps)) {
    boot.reps <- 100
  } else {
    if (!(boot.reps == round(boot.reps))) {
      stop("ERROR: Parameter 'boot.reps' must be an integer!")
    }
  }

  if (set.seed == T) {
    if (!(is.null(seed))) {
      if (!(seed == round(seed))) stop("ERROR: Parameter 'seed' must be an integer!")
      set.seed(seed)
    }
  }

  # Compose fitdistr optional arguments
  opt.args <- ""
  if (distn == "Bet") {
    # Distribution fitting parameters
    opt.args <- paste0(
      ", lower = c(", lowlim, ", ", lowlim, "), start=list(shape1=", shape1,
      ", shape2=", shape2, ")"
    )
    densfun <- "beta"

    # Predicted pdf parameters
    predfun <- "dbeta"
    predpar1 <- "shape1"
    predpar2 <- "shape2"
  } else if (distn == "gamma") {
    # Distribution fitting parameters
    densfun <- "gamma"

    # Predicted pdf parameters
    predfun <- "dgamma"
    predpar1 <- "shape"
    predpar2 <- "rate"

    # Optional arguments, including lower and upper bounds
    opt.args <- paste0(
      ", ",
      "lower = c(", lowlim, ", ", lowlim, "), ",
      "upper = c(", uplim, ", ", uplim, "), ",
      "start=list(shape=", shape, ", ",
      "rate=", rate, ")"
    )
  } else if (distn == "NBin") {
    # Distribution fitting parameters
    densfun <- "negative binomial"

    # Predicted pdf parameters
    predfun <- "dnbinom"
    predpar1 <- "size"
    predpar2 <- "mu"

    # Optional arguments, plus alternative lower bounds
    # if unbounded version fails
    opt.args <- ""
    opt.args.nbin.lb1 <- ", lower=c(1)" # alt lower bound=1
    opt.args.nbin.lb0 <- ", lower=c(0.1)" # alt lower bound~0
  } else {
    stop(paste0("ERROR: distribution '", distn, "' not recognized"))
  }

  # Construct the final commands
  fit.v1 <- paste0("fitdistr(v1, densfun='", densfun, "'", opt.args, ")")
  fit.v2 <- paste0("fitdistr(v2, densfun='", densfun, "'", opt.args, ")")

  pdf <- paste0(predfun, "(xs, ", predpar1, "= mod$estimate[1], ", predpar2, "= mod$estimate[2])")

  fit.boot <- paste0("fitdistr(boot.samp, densfun='", densfun, "'", opt.args, ")")

  ################################
  # Transformations
  ################################

  if (distn == "gamma") {
    # Set zeros to very small value > 1 to avoid crash
    v1[v1 == 0] <- 0.0000001
    v2[v2 == 0] <- 0.0000001
  }

  ##############################
  # Fit actual pdfs and get pdfs
  # Note use of eval(parse...) to force
  # evaluation string as a command
  ##############################

  # Focal pdf
  mod <- tryCatch(eval(parse(text = fit.v1)),
    error = function(cond) {
      return("FAIL")
    },
    warning = function(cond) {
      return("FAIL")
    }
  )
  # Fix for special case of NBin only
  # Adds explicit lower bound to avoid negative value errors
  if (mod[1] == "FAIL" && distn == "NBin") {
    if (min(unique(b)) >= 1) {
      fit.v1 <- paste0("fitdistr(v1, densfun='", densfun, "'", opt.args.nbin.lb1, ")")
    } else {
      fit.v1 <- paste0("fitdistr(v1, densfun='", densfun, "'", opt.args.nbin.lb0, ")")
    }

    # Try again
    mod <- tryCatch(eval(parse(text = fit.v1)),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )
  }

  if (mod[1] == "FAIL") {
    return(list("FAIL"))
  }
  pdf1 <- eval(parse(text = pdf))
  pdf1[pdf1 == Inf] <- NA

  # Get model parameters for alternative overlap method
  # if this is beta dist
  if (distn == "Bet") {
    mod1.shp1 <- mod$estimate[[1]]
    mod1.shp2 <- mod$estimate[[2]]
  }

  # Benchmark pdf
  mod <- tryCatch(eval(parse(text = fit.v2)),
    error = function(cond) {
      return("FAIL")
    },
    warning = function(cond) {
      return("FAIL")
    }
  )
  # Fix for special case of NBin only
  # Adds explicit lower bound to avoid negative value errors
  if (mod[1] == "FAIL" && distn == "NBin") {
    if (min(unique(b)) >= 1) {
      fit.v2 <- paste0("fitdistr(v2, densfun='", densfun, "'", opt.args.nbin.lb1, ")")
    } else {
      fit.v2 <- paste0("fitdistr(2, densfun='", densfun, "'", opt.args.nbin.lb0, ")")
    }

    # Try again
    mod <- tryCatch(eval(parse(text = fit.v2)),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )
  }

  if (mod[1] == "FAIL") {
    return(list("FAIL"))
  }
  pdf2 <- eval(parse(text = pdf))
  pdf2[pdf2 == Inf] <- NA

  # Get model parameters for alternative overlap method
  # if this is beta dist
  if (distn == "Bet") {
    mod2.shp1 <- mod$estimate[[1]]
    mod2.shp2 <- mod$estimate[[2]]
  }

  # Calculate overlap between actual pdfs
  # ol <- sum( pmin( pdf1, pdf2 ) * mean( diff( xs ) ), na.rm=T )
  if (distn == "Bet") {
    ol <- prop.ol.beta(
      xs = xs,
      s1 = mod1.shp1, r1 = mod1.shp2,
      s2 = mod2.shp1, r2 = mod2.shp2
    )
    ol <- ol[[1]]
  } else {
    ol <- sum(pmin(pdf1, pdf2) * mean(diff(xs)), na.rm = T)
  }

  # UNDER CONSTRUCTION!
  # NEED TO FIGURE OUT WAY TO DO BETA OVERLAP FOR
  # BOOTSTRAPS !!!!!!!!!!

  boot.pdf1 <- sapply(1:boot.reps, function(i) {
    boot.samp <- sample(v1, length(v1), replace = T)
    mod <- tryCatch(eval(parse(text = fit.boot)),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )
    # Fix for special case of NBin only
    # Adds explicit lower bound to avoid negative value errors
    if (mod[1] == "FAIL" && distn == "NBin") {
      if (min(unique(v1)) >= 1) {
        fit.boot <- paste0(
          "fitdistr(boot.samp, densfun='",
          densfun, "'", opt.args.nbin.lb1, ")"
        )
      } else {
        fit.boot <- paste0(
          "fitdistr(boot.samp, densfun='",
          densfun, "'", opt.args.nbin.lb0, ")"
        )
      }

      # Try again
      mod <- tryCatch(eval(parse(text = fit.boot)),
        error = function(cond) {
          return("FAIL")
        },
        warning = function(cond) {
          return("FAIL")
        }
      )
    }

    if (mod[1] == "FAIL") {
      rep(NA, length(xs))
    } else {
      eval(parse(text = pdf))
    }
  })
  boot.pdf1[boot.pdf1 == Inf] <- NA

  boot.pdf2 <- sapply(1:boot.reps, function(i) {
    boot.samp <- sample(v2, length(v2), replace = T)
    # mod <- suppressWarnings(eval(parse(text = fit.boot ) ) )
    # eval( parse( text= pdf ) )
    mod <- tryCatch(eval(parse(text = fit.boot)),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )

    # Fix for special case of NBin only
    # Adds explicit lower bound to avoid negative value errors
    if (mod[1] == "FAIL" && distn == "NBin") {
      if (min(unique(v2)) >= 1) {
        fit.boot <- paste0(
          "fitdistr(boot.samp, densfun='",
          densfun, "'", opt.args.nbin.lb1, ")"
        )
      } else {
        fit.boot <- paste0(
          "fitdistr(boot.samp, densfun='",
          densfun, "'", opt.args.nbin.lb0, ")"
        )
      }

      # Try again
      mod <- tryCatch(eval(parse(text = fit.boot)),
        error = function(cond) {
          return("FAIL")
        },
        warning = function(cond) {
          return("FAIL")
        }
      )
    }

    if (mod[1] == "FAIL") {
      rep(NA, length(xs))
    } else {
      eval(parse(text = pdf))
    }
  })
  boot.pdf2[boot.pdf2 == Inf] <- NA

  # Create vector of bootstrapped overlap values
  boot.ol <- rep(NA, boot.reps)
  for (i in 1:boot.reps) {
    boot.ol[i] <- sum(pmin(boot.pdf1[, i], boot.pdf2[, i]) * mean(diff(xs)), na.rm = T)
  }

  # sapply method. SLOWER!
  # boot.ol <- sapply(1:boot.reps, function(i) {
  # sum(pmin(boot.pdf1[ ,i], boot.pdf2[ ,i]))*mean(diff(xs))
  # } )

  # Get 95% CLs of the overlaps of the bootstrapped distributions
  # using empirical bootstrap method. Uses bootstrap deviations from
  # bootstrap mean, and adds/subtracts these from observed value
  boot.ol.mean <- mean(boot.ol, na.rm = T)
  boot.ol.dev <- boot.ol - boot.ol.mean # Deviations from bootstrap mean
  ol.dev <- ol + boot.ol.dev
  ol.sd <- sd(ol.dev) # Bootstrap SD
  ol.cls <- quantile(ol.dev, c(0.025, 0.975), na.rm = T)
  ol.cls
  ol.lcl <- ol.cls[[1]]
  ol.ucl <- ol.cls[[2]]

  # Assemble list object & return
  ol.list <- list(
    ol, ol.cls, ol.lcl, ol.ucl,
    xs, pdf1, pdf2, boot.pdf1, boot.pdf2, boot.ol, ol.sd
  )
  names(ol.list) <- c(
    "ol.obs", "ol.cls", "ol.lcl", "ol.ucl",
    "xs", "pdf1", "pdf2",
    "boot.pdf1", "boot.pdf2", "boot.ol", "ol.sd"
  )
  return(ol.list)
}

boot.overlap.diff <- function(distn, test.tail = NULL, f, b, xs = NULL,
                              q.tr.logit.inverse = TRUE, logit.inverse.beta = 2,
                              boot.reps = NULL, seed = NULL, set.seed = F, ...) {
  ###################################################
  # Calculates overlap (ol) & bootstrapped 95% CLs between two
  # probability distributions, as fit to two input vectors. Includes correction
  # for one-tailed comparisions.
  #
  # NOT FOR BETA DISTRIBUTION
  #
  # Parameters:
  # 		1. distn				Distribution family ('NBin','gamma','Bet')
  # 		2. f					focal vector
  # 		3. b				benchmark vector
  # 		4. xs					Vector of x values for generating predicted probabilities
  # 									[Default=0 -> highest value of f, b, plus 10]
  # 		5. boot.reps		Number of bootstap replicates [Default=1000]
  # 		6. seed				Randomization seed for fixed results [Default=no seed]
  # 		7. Standard optional arguments specific to fitdistr():
  # 			(a) nbin: no additional arguments needed
  # 			(b) beta: shape1 & shape2 (starting parameters ), lowlim (lower limit)
  # 			(c) gamma: shape & rate (starting parameters ), lowlim (lower limit)
  #
  # Returns list object:
  # 		1. Observed ol
  # 		2. Lower and upper 95% CLs for observed ol
  # 		3. Saved x values (xs)
  # 		4. pdf.f: Fitted focal pdf
  # 		5. pdf.b: Fitted benchmark pdf
  # 		6. Bootrapped prob density function values for pdf.f
  # 		7. Bootrapped prob density function values for pdf.b
  # 		8. Bootstrapped ol values
  #
  #  Optional arguments (...):
  # 		lowlim
  # 		uplim
  # 		rate
  # 		shape
  ###################################################

  # Set default values for optional parameters & check for errors
  # Set default values for optional parameters & check for errors
  if (is.null(test.tail)) test.tail <- "both"
  if (is.null(xs)) xs <- seq(0, 1, length.out = 100)
  if (is.null(boot.reps)) boot.reps <- 1000
  if (is.null(perm.reps)) perm.reps <- 10000
  if ((set.seed == T) && !(is.null(seed))) set.seed(seed)

  # Compose fitdistr optional arguments
  opt.args <- ""
  if (distn == "Bet") {
    stop(paste0("ERROR: wrong function for beta distribution!"))
  } else if (distn == "gamma") {
    densfun <- "gamma"

    # Predicted pdf parameters
    predfun <- "dgamma"
    predpar1 <- "shape"
    predpar2 <- "rate"

    # Upper and lower bounds, rate, shape
    opt.args <- paste0(
      ", ",
      "lower = c(", lowlim, ", ", lowlim, "), ",
      "upper = c(", uplim, ", ", uplim, "), ",
      "start=list(shape=", shape, ", rate=", rate, ")"
    )
    # Alternate args if fails: lower bounds, rate, shape, but no upper bounds
    opt.args.no.ub <- paste0(
      ", ",
      "lower = c(", lowlim, ", ", lowlim, "), ",
      "start=list(shape=", shape, ", rate=", rate, ")"
    )
  } else if (distn == "NBin") {
    densfun <- "negative binomial"

    # Predicted pdf parameters
    predfun <- "dnbinom"
    predpar1 <- "size"
    predpar2 <- "mu"

    opt.args <- ""

    # Alternative lower bounds
    # Use if unbounded version fails
    opt.args.nbin.lb1 <- ", lower=c(1)" # alt lower bound=1
    opt.args.nbin.lb0 <- ", lower=c(0.1)" # alt lower bound~0
  } else {
    stop(paste0("ERROR: distribution '", distn, "' not recognized"))
  }

  # Construct the final commands
  fit.f <- paste0("fitdistr(f, densfun='", densfun, "'", opt.args, ")")
  fit.b <- paste0("fitdistr(b, densfun='", densfun, "'", opt.args, ")")

  pdf <- paste0(predfun, "(xs, ", predpar1, "= mod$estimate[1], ", predpar2, "= mod$estimate[2])")

  fit.boot <- paste0("fitdistr(boot.samp, densfun='", densfun, "'", opt.args, ")")

  # Get benchmark mean, used for one-tail overlap
  b.mean <- mean(b, na.rm = T)

  ################################
  # Transformations
  ################################

  if (distn == "gamma") {
    # Set zeros to very small value > 1 to avoid crash
    f[f == 0] <- 0.0000001
    b[b == 0] <- 0.0000001
  }

  ##############################
  # Fit actual pdfs and get pdfs
  # Note use of eval(parse...) to force
  # evaluation string as a command
  ##############################

  # Focal pdf
  mod <- tryCatch(eval(parse(text = fit.f)),
    error = function(cond) {
      return("FAIL")
    },
    warning = function(cond) {
      return("FAIL")
    }
  )
  if (mod[1] == "FAIL" && distn == "NBin") {
    # Fix for special case of NBin only
    # Adds explicit lower bound to avoid negative value errors

    if (min(unique(f)) >= 1) {
      fit.f <- paste0("fitdistr(f, densfun='", densfun, "'", opt.args.nbin.lb1, ")")
    } else {
      fit.f <- paste0("fitdistr(f, densfun='", densfun, "'", opt.args.nbin.lb0, ")")
    }

    # Try again
    mod <- tryCatch(eval(parse(text = fit.f)),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )
  } else if (mod[1] == "FAIL" && distn == "gamma") {
    # Fix for gamma: remove upper bounds
    fit.f <- paste0("fitdistr(f, densfun='", densfun, "'", opt.args.no.ub, ")")

    cat("    [fitdistr(f) failed, repeating without upper bounds...")

    # Try again
    mod <- tryCatch(eval(parse(text = fit.f)),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )

    if (mod[1] == "FAIL") {
      # Uncomment to echo details
      cat("failed]\n")
      cat("Failed vectors:\n")
      cat("f:\n")
      print(f)
      cat("b:\n")
      print(b)
    } else {
      cat("success]\n")
    }
  }

  if (mod[1] == "FAIL") {
    return(list("FAIL"))
  }
  pdf.f <- eval(parse(text = pdf))
  pdf.f[pdf.f == Inf] <- NA

  # Benchmark pdf
  mod <- tryCatch(eval(parse(text = fit.b)),
    error = function(cond) {
      return("FAIL")
    },
    warning = function(cond) {
      return("FAIL")
    }
  )

  if (mod[1] == "FAIL" && distn == "NBin") {
    if (min(unique(b)) >= 1) {
      fit.b <- paste0("fitdistr(b, densfun='", densfun, "'", opt.args.nbin.lb1, ")")
    } else {
      fit.b <- paste0("fitdistr(b, densfun='", densfun, "'", opt.args.nbin.lb0, ")")
    }

    # Try again
    mod <- tryCatch(eval(parse(text = fit.b)),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )
  } else if (mod[1] == "FAIL" && distn == "gamma") {
    fit.b <- paste0("fitdistr(b, densfun='", densfun, "'", opt.args.no.ub, ")")

    cat("b failed (gamma): Using opt.args.no.ub\n")


    # Try again
    mod <- tryCatch(eval(parse(text = fit.b)),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )
  }

  if (mod[1] == "FAIL") {
    return(list("FAIL"))
  }
  pdf.b <- eval(parse(text = pdf))
  pdf.b[pdf.b == Inf] <- NA

  # Calculate overlap between actual pdfs
  ol <- q.overlap(pdf.f = pdf.f, pdf.b = pdf.b, xs = xs, b.mean = b.mean, test.tail = test.tail)
  q <- ol

  # Transform indicator quality if required (see global params file
  q.raw <- q # Save original value, even if don't change
  if (q.tr.logit.inverse == TRUE) {
    q <- q.inverse.logit(q, logit.inverse.beta)
  }

  boot.pdf.f <- sapply(1:boot.reps, function(i) {
    boot.samp <- sample(f, length(f), replace = T)
    mod <- tryCatch(eval(parse(text = fit.boot)),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )
    # Fix for special case of NBin only
    # Adds explicit lower bound to avoid negative value errors
    if (mod[1] == "FAIL" && distn == "NBin") {
      if (min(unique(f)) >= 1) {
        fit.boot <- paste0(
          "fitdistr(boot.samp, densfun='",
          densfun, "'", opt.args.nbin.lb1, ")"
        )
      } else {
        fit.boot <- paste0(
          "fitdistr(boot.samp, densfun='",
          densfun, "'", opt.args.nbin.lb0, ")"
        )
      }

      # Try again
      mod <- tryCatch(eval(parse(text = fit.boot)),
        error = function(cond) {
          return("FAIL")
        },
        warning = function(cond) {
          return("FAIL")
        }
      )
    }

    if (mod[1] == "FAIL") {
      rep(NA, length(xs))
    } else {
      eval(parse(text = pdf))
    }
  })
  boot.pdf.f[boot.pdf.f == Inf] <- NA

  boot.pdf.b <- sapply(1:boot.reps, function(i) {
    boot.samp <- sample(b, length(b), replace = T)
    mod <- tryCatch(eval(parse(text = fit.boot)),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )
    # Fix for special case of NBin only
    # Adds explicit lower bound to avoid negative value errors
    if (mod[1] == "FAIL" && distn == "NBin") {
      if (min(unique(b)) >= 1) {
        fit.boot <- paste0(
          "fitdistr(boot.samp, densfun='",
          densfun, "'", opt.args.nbin.lb1, ")"
        )
      } else {
        fit.boot <- paste0(
          "fitdistr(boot.samp, densfun='",
          densfun, "'", opt.args.nbin.lb0, ")"
        )
      }

      # Try again
      mod <- tryCatch(eval(parse(text = fit.boot)),
        error = function(cond) {
          return("FAIL")
        },
        warning = function(cond) {
          return("FAIL")
        }
      )
    }
    if (mod[1] == "FAIL") {
      # Length of failed vector must match length of successful
      # vector, which is xs +1 (pdf mean plus the x values, see below)
      rep(NA, length(xs) + 1)
    } else {
      # Get expected values (mean of x) for pdf
      if (distn == "NBin") {
        curr.b.mean <- mod$estimate[[2]]
      } else if (distn == "gamma") {
        curr.b.mean <- mod$estimate[[1]] / mod$estimate[[2]]
      } else {
        stop(paste0("ERROR in function boot.overlap.diff: Not supported for distribution ", distn))
      }
      curr.boot.pdf <- eval(parse(text = pdf))
      curr.boot.pdf[curr.boot.pdf == Inf] <- NA
      # append b.mean to front of bootstrap vector
      curr.boot.pdf <- as.vector(c(curr.b.mean, curr.boot.pdf))
      curr.boot.pdf
    }
  })
  boot.pdf.b[boot.pdf.b == Inf] <- NA

  # Create vector of bootstrapped overlap values
  boot.ol <- rep(NA, boot.reps)
  for (i in 1:boot.reps) {
    curr.pdf.f <- boot.pdf.f[, i]
    curr.b.mean <- boot.pdf.b[, i][1] # first element=mean
    curr.pdf.b <- boot.pdf.b[, i][2 - length(boot.pdf.b[, i])] # remaining elements=the pdf
    boot.ol[i] <- q.overlap(
      pdf.f = curr.pdf.f, pdf.b = curr.pdf.b, xs = xs,
      b.mean = curr.b.mean, test.tail = test.tail
    )
  }
  boot.q <- boot.ol

  # Transform bootstrapped quality (overlap) if requested
  if (q.tr.logit.inverse == TRUE) {
    boot.q <- q.inverse.logit(boot.q, logit.inverse.beta)
  }

  # Get 95% CLs of the overlaps of the bootstrapped distributions
  # using empirical bootstrap method. Uses bootstrap deviations from
  # bootstrap mean, and adds/subtracts these from observed value
  boot.q.mean <- mean(boot.q, na.rm = T)
  boot.q.dev <- boot.q - boot.q.mean # Deviations from bootstrap mean
  q.dev <- q + boot.q.dev
  q.sd <- sd(q.dev) # Bootstrap SD
  q.cls <- quantile(q.dev, c(0.025, 0.975), na.rm = T)
  q.cls
  q.lcl <- q.cls[[1]]
  q.ucl <- q.cls[[2]]

  # Assemble list object & return
  ol.list <- list(
    ol, q.raw, q, q.cls, q.lcl, q.ucl, q.sd,
    xs, pdf.f, pdf.b, boot.pdf.f, boot.pdf.b, boot.q
  )
  names(ol.list) <- c(
    "ol", "q.raw", "q", "q.cls", "q.lcl", "q.ucl", "q.sd",
    "xs", "pdf.f", "pdf.b", "boot.pdf.f", "boot.pdf.b", "boot.q"
  )

  return(ol.list)
}

boot.diff <- function(x, y, boot.reps = NULL) {
  ###################################################
  # Calculates bootstrap 95% CLs of difference between means of
  # two groups.
  #
  # Parameters: two vectors, x & y
  # Returns list object:
  # 		x.mean	observed mean of x
  # 		y.mean		observed mean of y
  # 		d.obs		observed mean difference
  # 		d.lcl			lower 95% CL
  # 		d.ucl		upper 95% CL
  ###################################################

  # Set boot.reps if not supplied
  if (is.null(boot.reps)) boot.reps <- 10000

  x.mean <- mean(x, na.rm = T)
  y.mean <- mean(y, na.rm = T)
  d.obs <- abs(x.mean - y.mean)

  # # Create vector of bootstrapped overlap values
  # boot.d <- rep(NA, boot.reps)
  # for (i in 1: boot.reps) {
  # boot.samp.x <- sample(x, length(x), replace =T )
  # boot.samp.y <- sample(y, length(y), replace =T )
  # boot.d[i] <- abs(mean(boot.samp.x)-mean(boot.samp.y)), na.rm=T)
  # }

  boot.d <- sapply(1:boot.reps, function(i) {
    boot.samp.x <- sample(x, length(x), replace = T)
    boot.samp.y <- sample(y, length(y), replace = T)
    d.samp <- abs(mean(boot.samp.x, na.rm = T) - mean(boot.samp.y, na.rm = T))
  })

  # Get 95% CLs of the bootstrapped differences
  # Empirical bootstrap method
  boot.mean <- mean(boot.d, na.rm = T)
  boot.dev <- boot.d - boot.mean # Deviations from bootstrap mean
  d.dev <- d.obs + boot.dev
  d.cls <- quantile(d.dev, c(0.025, 0.975), na.rm = T) # bootstrap CLs
  d.sd <- sd(d.dev) # Bootstrap SD

  results <- list(x.mean, y.mean, d.obs, d.sd, d.cls)
  names(results) <- c("x.mean", "y.mean", "d.obs", "d.sd", "d.cls")
  return(results)
}

boot.qual.diff <- function(f, b, dist, boot.reps = NULL) {
  ###################################################
  # Calculates bootstrap 95% CLs of quality based on difference
  # between means of focal (f) and benchmark (b) vectors
  #
  # Parameters:
  # 		f					focal vectors
  # 		b					benchmark vectors
  # 		dist				distribution of the vectors c("Bet","NBin","gamma")
  # 		boot.reps	[Optional; default=10000)
  #
  # Returns list object:
  # 		f.mean		observed mean of f
  # 		b.mean	observed mean of b
  # 		d.obs		observed mean difference
  # 		q.obs		observed mean quality
  # 		q.cls			bootstrap 95% CLs
  # 		p.diff		p-value for perm test of H0: f.mean=b.mean
  # 		boot.q		bootstrapped qualities
  # 		boot.reps	Number of replications used
  ###################################################

  # Set boot.reps if not supplied
  if (is.null(boot.reps)) boot.reps <- 10000

  f.mean <- mean(f, na.rm = T)
  b.mean <- mean(b, na.rm = T)
  d.obs <- abs(f.mean - b.mean)

  # Test if means different
  p.diff <- means.test.perm(f, b, iterations = boot.reps)

  # Calculate quality according to distribution
  if (dist == "Bet") {
    q.obs <- qual.prop.fixed(f.mean, b.mean)
  } else {
    stop("UNDER CONSTRUCTION (function: boot.qual.diff)")
  }

  boot.q <- sapply(1:boot.reps, function(i) {
    boot.samp.f <- sample(f, length(f), replace = T)
    boot.samp.b <- sample(b, length(b), replace = T)
    boot.samp.f.mean <- mean(boot.samp.f)
    boot.samp.b.mean <- mean(boot.samp.b)

    if (dist == "Bet") {
      q.samp <- qual.prop.fixed(boot.samp.f.mean, boot.samp.b.mean)
    } else {
      stop("UNDER CONSTRUCTION (function: boot.qual.diff)")
    }
  })

  # Get 95% CLs of the bootstrapped differences
  # Empirical bootstrap method
  boot.mean <- mean(boot.q, na.rm = T)
  boot.dev <- boot.q - boot.mean # Deviations from bootstrap mean
  q.dev <- q.obs + boot.dev # Bootstrap mean deviances
  q.cls <- quantile(q.dev, c(0.025, 0.975), na.rm = T) # bootstrap CLs
  q.sd <- sd(q.dev) # Bootstrap SD
  q.lcl <- q.cls[[1]]
  q.ucl <- q.cls[[2]]

  results <- list(
    f.mean, b.mean, d.obs,
    q.obs, q.sd, q.lcl, q.ucl,
    p.diff
  )
  names(results) <- c(
    "f.mean", "b.mean", "d.obs",
    "q.obs", "q.sd", "q.lcl", "q.ucl",
    "p.diff"
  )
  # results <- list(f.mean, b.mean, d.obs, q.obs, q.sd, q.lcl, q.ucl, p.diff, boot.q, boot.reps)
  # names(results) <- c("f.mean", "b.mean", "d.obs", "q.obs", "q.sd", "q.lcl", "q.ucl", "p.diff",
  # "boot.q", "boot.reps")

  return(results)
}

boot.qual.beta <- function(f, b, xs = NULL,
                           boot.reps = NULL, perm.reps = NULL, seed = NULL, set.seed = F,
                           shape1 = 0.1, shape2 = 0.1, lowlim = 0.0001,
                           use.q.diff.raw = FALSE, discount.method = "linear", buffer = 0.01,
                           wt.method = "u.beta.buffered", test.diff.boot = FALSE,
                           q.tr.logit.inverse = TRUE, logit.inverse.beta = 2) {
  ###################################################
  # Calculates mean and bootstrap 95% CLs of quality for beta-
  # distribution vectors on interval [0,1] using mixed overlap- and
  # means-based quality algorithm
  #
  # Calculates quality based on both (a) difference of means and (b)
  # overlap, and applies correction that weights overlap for means near
  # middle of distribution, and distance for means near limits (0 and 1)
  #
  # Parameters:
  # 		f					focal vectors
  # 		b					benchmark vectors
  # 		dist				distribution of the vectors c("Bet","NBin","gamma")
  # 		boot.reps	[Optional] Bootstrap replicates; default=1000
  # 		perm.reps	[Optional] Permutation iterations; default=10000
  # 		use.q.diff.raw [Optional]	If FALSE, q.diff=1 when p.diff>=0.05, otherwiwse
  # 													q.diff = actual means-based quality in all cases
  # 		discount.method 	[Optional] Discount algorithm for difference-based
  # 										quality. Default="linear". See params file for details.
  #
  # Returns list object:
  # 		f.mean		observed mean of f
  # 		b.mean	observed mean of b
  # 		d.obs		observed mean difference
  # 		q.obs		observed mean quality
  # 		q.cls			bootstrap 95% CLs
  # 		p.diff		p-value for perm test of H0: f.mean=b.mean
  # 		boot.q		bootstrapped qualities
  # 		boot.reps	Number of replications used
  # 		boot.mean	Boostrap mean quality
  # 		ol.w			overlap-vs-means test weighting factor
  # 		pdf.f, pdf.b	The fitted f & b pdfs
  # 		xs				X values used to fit pdfs
  ###################################################

  # Set default values for optional parameters & check for errors
  if (is.null(xs)) xs <- seq(0, 1, length.out = 100)
  if (is.null(boot.reps)) boot.reps <- 1000
  if (is.null(perm.reps)) perm.reps <- 10000
  if ((set.seed == T) && !(is.null(seed))) set.seed(seed)

  # Get observed quality
  # Note that perm test always performed (test.diff=T) even if q.diff.raw not reset
  q.list <- qual.beta(
    f = f, b = b, xs = xs, perm.reps = perm.reps,
    test.diff = TRUE, use.q.diff.raw = use.q.diff.raw,
    discount.method = discount.method, wt.method = wt.method, buffer = buffer,
    shape1 = shape1, shape2 = shape2, lowlim = lowlim
  )

  f.mean <- q.list$f.mean
  b.mean <- q.list$b.mean
  diff <- q.list$diff
  diff.p <- q.list$diff.p
  q.diff <- q.list$q.diff
  q.diff.raw <- q.list$q.diff.raw
  q.ol <- q.list$q.ol
  q <- q.list$q
  ol.w <- q.list$ol.w
  pdf.f <- q.list$pdf.f
  pdf.b <- q.list$pdf.b

  # Transform indicator quality if required (see global params file
  q.raw <- q # Save original value, even if don't change
  if (q.tr.logit.inverse == TRUE) {
    q <- q.inverse.logit(q, logit.inverse.beta)
  }

  # Set remaining values to NA
  q.sd <- NA # Bootstrap SD
  q.lcl <- NA
  q.ucl <- NA
  boot.q <- NA
  boot.mean <- NA

  # Get bootstrap qualities
  # Note that set.seed must be false
  if (ol.w > 0) {
    i <- 0
    boot.q <- sapply(1:boot.reps, function(i) {
      f.samp <- sample(f, length(f), replace = T)
      b.samp <- sample(b, length(b), replace = T)

      # get quality for this iteration
      # Note that perm. test for q.diff can be turned off by test.diff.boot
      q.list.boot <- qual.beta(
        f = f.samp, b = b.samp, xs = xs, perm.reps = perm.reps,
        test.diff = test.diff.boot, use.q.diff.raw = use.q.diff.raw,
        discount.method = discount.method, wt.method = wt.method, buffer = buffer,
        shape1 = shape1, shape2 = shape2, lowlim = lowlim
      )
      q.boot <- q.list.boot$q
      if (q.tr.logit.inverse == TRUE) q.boot <- q.inverse.logit(q.boot, logit.inverse.beta)
      q.boot
    })
    # boot.q <- boot.q[!is.na(boot.q)]

    # Get 95% CLs of the bootstrapped differences
    # Empirical bootstrap method
    boot.mean <- mean(boot.q, na.rm = T)
    boot.dev <- boot.q - boot.mean # Deviations from bootstrap mean
    q.dev <- q + boot.dev # Bootstrap mean deviances
    q.cls <- quantile(q.dev, c(0.025, 0.975), na.rm = T) # bootstrap CLs
    q.sd <- sd(q.dev) # Bootstrap SD
    q.lcl <- q.cls[[1]]
    q.ucl <- q.cls[[2]]
  }

  results <- list(
    f.mean, b.mean, diff, diff.p, q.diff, q.diff.raw,
    q.ol, q.raw, q, q.sd, q.lcl, q.ucl,
    boot.q, boot.reps, boot.mean, ol.w,
    pdf.f, pdf.b, xs
  )
  names(results) <- c(
    "f.mean", "b.mean", "diff", "p.diff", "q.diff", "q.diff.raw",
    "q.ol", "q.raw", "q", "q.sd", "q.lcl", "q.ucl",
    "boot.q", "boot.reps", "boot.mean", "ol.w",
    "pdf.f", "pdf.b", "xs"
  )

  return(results)
}

boot.overlap.beta <- function(f, b, xs = NULL,
                              boot.reps = NULL, seed = NULL, set.seed = F,
                              shape1 = 0.1, shape2 = 0.1, lowlim = 0.0001,
                              discount.method = "linear", test.diff.boot = FALSE,
                              q.tr.logit.inverse = TRUE, logit.inverse.beta = 2) {
  ###################################################
  # Calculates mean and bootstrap 95% CLs of quality for beta-
  # distribution vectors on interval [0,1], using overlap method only
  #
  # Includes correction for species case where one or both indicators are
  # all 0s or all 1s.
  #
  # Parameters:
  # 		f					focal vectors
  # 		b					benchmark vectors
  # 		dist				distribution of the vectors c("Bet","NBin","gamma")
  # 		boot.reps	[Optional] Bootstrap replicates; default=1000
  # 		perm.reps	[Optional] Permutation iterations; default=10000
  # 		use.q.diff.raw [Optional]	If FALSE, q.diff=1 when p.diff>=0.05, otherwiwse
  # 													q.diff = actual means-based quality in all cases
  # 		discount.method 	[Optional] Discount algorithm for difference-based
  # 										quality. Default="linear". See params file for details.
  # 		q.tr.logit.inverse	Transform final indicator quality using q.logit.inverse fn?
  # 		logit.inverse.beta	Steepness parameter for fn q.logit.inverse.
  #
  # Returns list object:
  # 		f.mean		observed mean of f
  # 		b.mean	observed mean of b
  # 		d.obs		observed mean difference
  # 		q.obs		observed mean quality
  # 		q.cls			bootstrap 95% CLs
  # 		p.diff		p-value for perm test of H0: f.mean=b.mean
  # 		boot.q		bootstrapped qualities
  # 		boot.reps	Number of replications used
  # 		boot.mean	Boostrap mean quality
  # 		ol.w			overlap-vs-means test weighting factor
  # 		pdf.f, pdf.b	The fitted f & b pdfs
  # 		xs				X values used to fit pdfs
  ###################################################

  # Set default values for optional parameters & check for errors
  if (is.null(xs)) xs <- seq(0, 1, length.out = 100)
  if (is.null(boot.reps)) boot.reps <- 1000
  if ((set.seed == T) && !(is.null(seed))) set.seed(seed)

  # Get observed quality
  q.list <- qual.beta.ol(
    f = f, b = b, xs = xs,
    shape1 = shape1, shape2 = shape2, lowlim = lowlim
  )

  f.mean <- q.list$f.mean
  b.mean <- q.list$b.mean
  diff <- q.list$diff
  q.ol <- q.list$q.ol
  q <- q.ol
  pdf.f <- q.list$pdf.f
  pdf.b <- q.list$pdf.b
  beta.tr.method <- q.list$beta.tr.method
  f.tr <- q.list$f.tr
  b.tr <- q.list$b.tr

  # Transform indicator quality if required (see global params file
  q.raw <- q # Save original value, even if don't change
  if (q.tr.logit.inverse == TRUE) {
    q <- q.inverse.logit(q, logit.inverse.beta)
  }

  # Set remaining values to NA
  q.sd <- NA # Bootstrap SD
  q.lcl <- NA
  q.ucl <- NA
  boot.q <- NA
  boot.mean <- NA

  # Get bootstrap qualities
  # Note that set.seed must be false
  i <- 0
  boot.q <- sapply(1:boot.reps, function(i) {
    f.samp <- sample(f, length(f), replace = T)
    b.samp <- sample(b, length(b), replace = T)

    # get quality for this iteration
    # Note that perm. test for q.diff can be turned off by test.diff.boot
    q.list.boot <- qual.beta.ol(
      f = f.samp, b = b.samp, xs = xs,
      shape1 = shape1, shape2 = shape2, lowlim = lowlim
    )
    q.boot <- q.list.boot$q
    if (q.tr.logit.inverse == TRUE) q.boot <- q.inverse.logit(q.boot, logit.inverse.beta)
    q.boot
  })

  # Get 95% CLs of the bootstrapped differences
  # Empirical bootstrap method
  boot.mean <- mean(boot.q, na.rm = T)
  boot.dev <- boot.q - boot.mean # Deviations from bootstrap mean
  q.dev <- q + boot.dev # Bootstrap mean deviances
  q.cls <- quantile(q.dev, c(0.025, 0.975), na.rm = T) # bootstrap CLs
  q.sd <- sd(q.dev) # Bootstrap SD
  q.lcl <- q.cls[[1]]
  q.ucl <- q.cls[[2]]

  results <- list(
    f.mean, b.mean, diff,
    q.ol, q.raw, q, q.sd, q.lcl, q.ucl,
    boot.q, boot.reps, boot.mean,
    pdf.f, pdf.b, xs, beta.tr.method, b.tr, f.tr
  )
  names(results) <- c(
    "f.mean", "b.mean", "diff",
    "q.ol", "q.raw", "q", "q.sd", "q.lcl", "q.ucl",
    "boot.q", "boot.reps", "boot.mean",
    "pdf.f", "pdf.b", "xs", "beta.tr.method", "b.tr", "f.tr"
  )

  return(results)
}

boot.qual.beta.fixed <- function(f, b.fixed,
                                 boot.reps = NULL, perm.reps = NULL, use.q.diff.raw = FALSE,
                                 discount.method = "linear", test.diff.boot = FALSE,
                                 q.tr.logit.inverse = TRUE, logit.inverse.beta = 2) {
  ###################################################
  # Calculates overall quality for beta-distributed focal indicator
  # vector (f) relatve to fixed benchmark value
  #
  # Vector f must be distributed on [0,1]. Runs permutation test
  # only if test.diff=T.
  #
  # Returns list object: f.mean, b.fixed, diff, diff.p, q.diff, q.ol, q
  ###################################################

  # Set defaults for optional parameters
  if (is.null(boot.reps)) boot.reps <- 1000
  if (is.null(perm.reps)) perm.reps <- 1000

  f.mean <- mean(f, na.rm = T)
  f.w <- min(f.mean, 1 - f.mean) # Focal weight

  # get difference-based quality
  q.results <- qual.prop.fixed.b(
    f = f, b.fixed = b.fixed, perm.reps = perm.reps,
    test.diff = TRUE, use.q.diff.raw = use.q.diff.raw,
    discount.method = discount.method
  )
  q <- q.results$q
  q.diff.raw <- q.results$q.raw
  p <- q.results$p
  diff <- q.results$d.abs

  # Transform indicator quality if required (see global params file
  q.raw <- q # Save original value, even if don't change
  if (q.tr.logit.inverse == TRUE) {
    q <- q.inverse.logit(q, logit.inverse.beta)
  }

  # Get bootstrap quality
  boot.fixed.q <- sapply(1:boot.reps, function(i) {
    f.samp <- sample(f, length(f), replace = T)
    q.results.boot <- qual.prop.fixed.b(
      f = f.samp, b.fixed = b.fixed,
      perm.reps = perm.reps, test.diff = test.diff.boot,
      use.q.diff.raw = use.q.diff.raw, discount.method = discount.method
    )
    q.boot <- q.results.boot$q
    if (q.tr.logit.inverse == TRUE) q.boot <- q.inverse.logit(q.boot, logit.inverse.beta)
    q.boot
  })

  # Get 95% CLs of the bootstrap quality
  boot.mean <- mean(boot.fixed.q, na.rm = T)
  boot.dev <- boot.fixed.q - boot.mean # Deviations from bootstrap mean
  q.dev <- q + boot.dev # Bootstrap mean deviances
  q.cls <- quantile(q.dev, c(0.025, 0.975), na.rm = T) # bootstrap CLs
  q.sd <- sd(q.dev) # Bootstrap SD
  q.lcl <- q.cls[[1]]
  q.ucl <- q.cls[[2]]

  results <- list(
    f.mean, b.fixed, diff, p, q.diff.raw,
    q.raw, q, q.lcl, q.ucl,
    boot.fixed.q, boot.reps, boot.mean
  )
  names(results) <- c(
    "f.mean", "b.fixed", "diff", "p", "q.diff.raw",
    "q.raw", "q", "q.lcl", "q.ucl",
    "boot.q", "boot.reps", "boot.mean"
  )
  return(results)
}

boot.cls <- function(x) {
  ###################################################
  # Calculates (asymmetric) bootstrap 95% confidence limits of a
  # vector
  #
  # Returns list object:
  # 		lcl 	Lower 95% confidence limit
  # 		ucl	Upper 95% confidence limit
  #
  # Requires:
  # 		Package simpleboot
  ###################################################

  # Check required packages
  if (!require(simpleboot)) stop("ERROR: package 'simpleboot' not loaded")

  vec.mean <- mean(x, na.rm = T)

  if (length(unique(x)) == 1) {
    boot.lcl <- vec.mean
    boot.ucl <- vec.mean
  } else {
    # Get bootstrap confidence limits
    # Using simpleboot package
    x.boot <- one.boot(x, mean, R = 10000)
    boot <- boot.ci(x.boot, type = "bca")
    boot.lcl <- boot$bca[4]
    boot.ucl <- boot$bca[5]
  }

  results <- list(boot.lcl, boot.ucl)
  names(results) <- c("boot.lcl", "boot.ucl")
  return(results)
}

uni.test.boot <- function(v, val, reps = NULL) {
  ###################################################
  # Performs bootstrap univariate means test of H0: mean(v)=val
  #
  # Accepts: vector v and fixed value val, and optional number of
  # 		bootstrap replicates.
  #
  # Returns list object:
  # 		v.mean		Mean of v
  # 		p				Two-tailed p-value
  # 		lcl				Bootstrap lower 95% CL of v
  # 		ucl			Bootstrap upper 95% CL of v
  ###################################################

  # Set default value
  if (is.null(reps)) reps <- 1000

  n <- length(v)
  v.mean <- mean(v, na.rm = T)
  dev <- abs(v.mean - val)
  ulim <- v.mean + dev
  llim <- v.mean - dev
  bstrap <- c()

  for (i in 1:reps) {
    v.samp <- sample(v, n, replace = T)
    bstrap <- c(bstrap, mean(v.samp))
  }

  p.H0 <- (sum(bstrap < llim) + sum(bstrap > ulim)) / reps

  cls <- boot.cls(v)
  lcl <- cls$boot.lcl
  ucl <- cls$boot.ucl

  result <- list(v.mean, p.H0, lcl, ucl)
  names(result) <- c("v.mean", "p", "lcl", "ucl")
  return(result)
}

means.test.boot <- function(x, y, boot.reps = NULL) {
  ###################################################
  # Performs non-parametric bootstrap test of null hypothesis that
  # the means of two groups are the same
  #
  # Parameters: two vectors, x & y
  # Returns: 		p-value of test
  # Source: 		https://tinyurl.com/klzcjy4
  ###################################################

  # Set boot.reps if not supplied
  if (is.null(boot.reps)) boot.reps <- 10000

  # sample from H0 separately, no assumption about equal variance
  pooled <- c(x, y)
  xt <- x - mean(x) + mean(pooled)
  yt <- y - mean(y) + mean(pooled)

  boot.t <- c(1:boot.reps)
  for (i in 1:boot.reps) {
    sample.x <- sample(xt, replace = TRUE)
    sample.y <- sample(yt, replace = TRUE)
    boot.t[i] <- t.test(sample.x, sample.y)$statistic
  }
  p.h0 <- (1 + sum(abs(boot.t) > abs(t.test(x, y)$statistic))) / (boot.reps + 1)

  return(p.h0)
}

means.test.perm <- function(x, y, iterations = NULL, seed = NULL) {
  ###################################################
  # Performs non-parametric permutation test of null hypothesis that
  # the means of two groups are the same
  #
  # Parameters: two vectors, x & y, and optional # of iterations for test
  # Returns: 		p-value of test
  # Source: 		https://tinyurl.com/klzcjy4
  ###################################################

  if (!is.null(seed)) set.seed(seed)

  # Set iterations if not supplied
  if (is.null(iterations)) iterations <- 1000

  nx <- length(x)
  ny <- length(y)

  pool <- c(x, y)
  obs.diff.p <- mean(x) - mean(y)
  iterations <- 10000
  sampl.dist.p <- NULL

  for (i in 1:iterations) {
    resample <- sample(c(1:length(pool)), length(pool))

    x.perm <- pool[resample][1:nx]
    # Is this next one right or should it be (ny + 1)?
    y.perm <- pool[resample][(nx + 1):length(pool)]
    sampl.dist.p[i] <- mean(x.perm) - mean(y.perm)
  }
  p.permute <- (sum(abs(sampl.dist.p) >= obs.diff.p) + 1) / (iterations + 1)

  return(p.permute)
}

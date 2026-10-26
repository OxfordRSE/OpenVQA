best.fit.beta <- function(df) {
  ###################################################
  # Select best fitting one-way GLM regression model using Beta family of
  # distributions.
  #
  # Appropriate for proportional data (continuous, bounded by zero and one).
  # 	Returns simplest model with significant fit. Order of testing:
  # 		Beta (Bet) --> Zero-inflated beta (ZIBet) --> One-inflated Beta (OIBet)
  # 		--> Zero- and one-inflated Beta (ZOIBet)
  # Accepts: data frame which *MUST* have the following columns:
  # 		Trt: factor with two values (coding doesn't matter)
  #  	Rsp: Response. Must be integers >=0
  # Returns: List containing:
  # 		error: FALSE if function fails, else TRUE
  # 		fit.found: TRUE if at least one best model found, otherwise FALSE
  # 		dist.best: Abbreviation for one of four models, as shown aboveb
  # 		dist.best.text: Spelled out name of the best model
  # 		mod.best: the best model (model results, itself a list)
  # 		gf.mod.best: p-value of goodness-of-fit test for best-fitting model
  # 	If no model passes, returns above values for model Bet
  # Requires: betareg package
  ###################################################

  # Some error testing
  min.rsp <- min(df$Rsp)
  max.rsp <- max(df$Rsp)

  if (min.rsp < 0) {
    error <- "Error: negative values of response not allowed"
    print(error)
    return(error)
  } else if (max.rsp > 1) {
    error <- "Error: values of response >1 not allowed"
    print(error)
    return(error)
  }

  ########################
  # Transformations
  ########################

  # Add extra columns to hold original and transformed
  # response values
  df$Rsp.tr <- NA
  df$Rsp.orig <- df$Rsp

  if (min.rsp == 0 || max.rsp == 1) {
    # Apply standard beta transformation
    # if zeros or ones present
    n <- nrow(df)
    df$Rsp.tr <- (df$Rsp * (n - 1) + 0.5) / n
  } else {
    df$Rsp.tr <- df$Rsp
  }

  ########################
  # Set defaults & initial values
  ########################

  # Set standard distribution codes
  dist.code.bet <- "Bet"
  dist.code.zibet <- "ZIBet"
  dist.code.oibet <- "OIBet"
  dist.code.zoibet <- "ZOIBet"

  # Start by assuming all models don't fit
  mod.fits.bet <- F
  mod.fits.zibet <- F
  mod.fits.oibet <- F
  mod.fits.zoibet <- F

  ########################
  # Basic Beta (Bet)
  ########################

  # Use transformed value
  df$Rsp <- df$Rsp.tr

  # Basic model, equal dispersion, logit link
  mod <- betareg(Rsp ~ Trt, data = df)

  # Basic model, log-log link function instead of (default) logit
  mod2 <- betareg(Rsp ~ Trt, data = df, link = "loglog")

  # Use model 3 only if have >2 values in each response category
  trt1.vals <- length(unique(df$Rsp[df$Trt == 0]))
  trt2.vals <- length(unique(df$Rsp[df$Trt == 1]))
  if (trt1.vals > 2 & trt2.vals > 2) {
    # Two parameter model, variable dispersion, logit link
    mod3 <- betareg(Rsp ~ Trt | Trt, data = df)
  }

  # Two parameter model, variable dispersion, log-log link
  mod4 <- betareg(Rsp ~ Trt, data = df, link = "loglog")

  # Compare the models using AIC
  if (trt1.vals > 2 & trt2.vals > 2) {
    mods.aic <- AIC(mod, mod2, mod3, mod4)
  } else {
    mods.aic <- AIC(mod, mod2, mod4)
  }
  mods.aic$model <- rownames(mods.aic) # add model name as column
  # Select best model; if ties, will select simplest as a result of index
  best.mod <- mods.aic$model[mods.aic$AIC == min(mods.aic$AIC)][1]

  # Save best model and variable indicating if one or two parameters
  if (best.mod == "mod") {
    mod.bet <- mod
    mod.bet.type <- "fixed"
  } else if (best.mod == "mod2") {
    mod.bet <- mod2
    mod.bet.type <- "fixed"
  } else if (best.mod == "mod3") {
    mod.bet <- mod3
    mod.bet.type <- "variable"
  } else {
    # best.mod=='mod4'
    mod.bet <- mod4
    mod.bet.type <- "variable"
  }

  # Run goodness of fit based on analysis of deviance
  # nb. function Anova from package car (NOT base "anova")
  gf.mod <- Anova(mod.bet, type = "II")
  gf <- gf.mod[[3]]

  # Save model pseudo-R2
  r2 <- mod.bet$pseudo.r.squared

  # Update success variable if model fits
  if (gf >= 0.05) {
    mod.fits.bet <- T
  }

  # Save results for this distribution/model
  mod.bet <- mod
  gf.bet <- gf


  # Reset to non-transformed value for running
  # inflated models
  df$Rsp <- df$Rsp.orig

  ########################
  # Zero-inflated Beta (ZIBet)
  ########################

  if (FALSE) { # Deactivating for now
    # if ( min(df$Rsp)==0 ) { START Zero-inflated
    # Only do if zeros present

    # UNDER CONSTRUCTION! NOT READY

    # Run the model
    mod <- gamlss(Rsp ~ Trt,
      data = df, family = BEINF,
      sigma.formula = ~1,
      nu.formula = ~ x1 + x3 + x4,
      tau.formula = ~x5
    )

    mod.sum <- summary(mod)

    # Run goodness of fit measure is zero-inflation coefficient
    gf <- coef(mod.sum)[[2]][4]

    # Update success variable if model fits
    if (gf >= 0.05) {
      mod.fits.zibet <- T
    }

    # Save results for this distribution/model
    mod.zibet <- mod
    gf.zibet <- gf
  } # END Zero-inflated

  ########################
  # One-inflated Beta (OIBet)
  ########################

  if (FALSE) { # Deactivating for now
    # if ( max(df$Rsp)==1 ) { START One-inflated beta
    # Only do if ones present

    # UNDER CONSTRUCTION
  } # END One-inflated beta

  ########################
  # Zero-One-inflated Beta (ZOIBet)
  ########################

  if (FALSE) { # Deactivating for now
    # if ( min(df$Rsp)==0 & max(df$Rsp)==1 ) { START One-inflated beta
    # Only do if zeros and ones present

    # UNDER CONSTRUCTION
  } # END One-inflated beta

  ###########################
  # Select the best model
  ###########################

  # Stop after first successful model
  fit.found <- T # Assume at least one model fits

  if (mod.fits.bet) {
    mod.best <- mod.bet
    dist.best.text <- "Beta"
    gf.mod.best <- gf.bet
    dist.best <- dist.code.bet
  } else if (mod.fits.zibet) {
    mod.best <- mod.zibet
    dist.best.text <- "Zero-inflated Beta"
    gf.mod.best <- gf.zibet
    dist.best <- dist.code.zibet
  } else if (mod.fits.oibet) {
    mod.best <- mod.oibet
    dist.best.text <- "One-Inflated Beta"
    gf.mod.best <- gf.oibet
    dist.best <- dist.code.oibet
  } else if (mod.fits.zoibet) {
    mod.best <- mod.zoibet
    dist.best.text <- "Zero-one-inflated Beta"
    gf.mod.best <- gf.zoibet
    dist.best <- dist.code.zoibet
  } else {
    # Return non-inflated model if no fit
    fit.found <- F
    mod.best <- mod.bet
    dist.best.text <- "Beta"
    gf.mod.best <- gf.bet
    dist.best <- dist.code.bet
  }

  best.fit.results <- list(fit.found, dist.best, dist.best.text, mod.best, gf.mod.best, r2)
  return(best.fit.results)
}

beta.glm <- function(f, b) {
  ###################################################
  # Select best fitting one-way GLM regression model using Beta
  # distribution.
  #
  # Appropriate for proportional data (continuous, bounded by zero and one).
  # 	Returns simplest model with significant fit. Order of testing:
  # 		Beta (Bet) --> Zero-inflated beta (ZIBet) --> One-inflated Beta (OIBet)
  # 		--> Zero- and one-inflated Beta (ZOIBet)
  # Accepts: data frame which *MUST* have the following columns:
  # 		Trt: factor with two values (coding doesn't matter)
  #  	Rsp: Response. Must be integers >=0
  # Returns: List containing:
  # 		error: FALSE if function fails, else TRUE
  # 		fit.found: TRUE if at least one best model found, otherwise FALSE
  # 		dist.best: Abbreviation for one of four models, as shown aboveb
  # 		dist.best.text: Spelled out name of the best model
  # 		mod.best: the best model (model results, itself a list)
  # 		gf.mod.best: p-value of goodness-of-fit test for best-fitting model
  # 	If no model passes, returns above values for model Bet
  # Requires: betareg package
  ###################################################

  # transform if necessary
  f <- beta.transform(f)
  b <- beta.transform(b)

  # Make data frame
  df.f <- as.data.frame(f)
  names(df.f) <- c("Rsp")
  df.f$Trt <- 1
  df.b <- as.data.frame(b)
  names(df.b) <- c("Rsp")
  df.b$Trt <- 0
  df <- rbind(df.f, df.b)

  ########################
  # Basic Beta
  ########################

  # Basic model, equal dispersion, logit link
  mod <- betareg(Rsp ~ Trt, data = df)

  # Basic model, log-log link function instead of (default) logit
  mod2 <- betareg(Rsp ~ Trt, data = df, link = "loglog")

  # Use model 3 only if have >2 values in each response category
  trt1.vals <- length(unique(df$Rsp[df$Trt == 0]))
  trt2.vals <- length(unique(df$Rsp[df$Trt == 1]))
  if (trt1.vals > 2 & trt2.vals > 2) {
    # Two parameter model, variable dispersion, logit link
    mod3 <- betareg(Rsp ~ Trt | Trt, data = df)
  }

  # Two parameter model, variable dispersion, log-log link
  mod4 <- betareg(Rsp ~ Trt, data = df, link = "loglog")

  # Compare the models using AIC
  if (trt1.vals > 2 & trt2.vals > 2) {
    mods.aic <- AIC(mod, mod2, mod3, mod4)
  } else {
    mods.aic <- AIC(mod, mod2, mod4)
  }
  mods.aic$model <- rownames(mods.aic) # add model name as column
  # Select best model; if ties, will select simplest as a result of index
  best.mod <- mods.aic$model[mods.aic$AIC == min(mods.aic$AIC)][1]

  # Save best model and variable indicating if one or two parameters
  if (best.mod == "mod1") {
    mod.bet <- mod
    mod.bet.type <- "fixed logit"
  } else if (best.mod == "mod2") {
    mod.bet <- mod2
    mod.bet.type <- "fixed loglog"
  } else if (best.mod == "mod3") {
    mod.bet <- mod3
    mod.bet.type <- "variable logit"
  } else {
    # best.mod=='mod4'
    mod.bet <- mod4
    mod.bet.type <- "variable loglog"
  }

  # Run goodness of fit based on analysis of deviance
  # nb. function Anova from package car (NOT base "anova")
  gf.mod <- Anova(mod.bet, type = "II")
  gf <- gf.mod[[3]]

  # Save model pseudo-R2
  r2 <- mod.bet$pseudo.r.squared

  # Update success variable if model fits
  mod.fits.bet <- F
  if (gf >= 0.05) {
    mod.fits.bet <- T
  }

  mod.type <- mod.bet.type
  mod <- mod.bet

  ###########################
  # Save the model results
  ###########################

  results <- list(mod, best.mod, mod.type, mod.fits.bet, gf, r2)
  names(results) <- c("mod", "best.mod", "mod.type", "mod.fits.bet", "gf", "r2")
  return(results)
}

beta.tr <- function(vec) {
  ###################################################
  # Performs standard beta transformation on vector
  #
  # Accepts vector, returns transformed vector.
  # if vec>=0 & vec<=1, then transformation is vec>0 & vec<1
  # If vec>0 & vec<1 (no values equal to 1 or 0) then returns original
  # untransformed vector.
  # Exits with error if any values >1 or <0
  ###################################################
  # Some preliminary tests
  if (max(vec) > 1 || min(vec) < 0) {
    error <- "Error: Values >1 or <0 not allowed for beta distribution!"
    print(error)
    return(error)
  }

  if (max(vec) == 1 || min(vec) == 0) {
    # Apply standard beta transformation
    # if zeros or ones present
    n <- length(vec)
    vec <- (vec * (n - 1) + 0.5) / n
  }

  return(vec)
}

beta.constrain <- function(vec) {
  ###################################################
  # Constrains vector distributed on [0,1] to beta interval (0,1)
  #
  # Accepts vector, returns transformed vector.
  # Sets zeros only to very small value >0 & sets one to value very
  # slightly < 1. Values on interval (0,1) are unchanged.
  # Exits with error if any values >1 or <0
  ###################################################
  # Some preliminary tests
  if (max(vec) > 1 || min(vec) < 0) {
    error <- "Error: Values >1 or <0 not allowed for beta distribution!"
    print(error)
    return(error)
  }

  if (max(vec) == 1 || min(vec) == 0) {
    vec <- replace(vec, vec == 0, 0.000001)
    vec <- replace(vec, vec == 1, 0.999999)
  }

  return(vec)
}

beta.tr.all <- function(vec, val) {
  ###################################################
  # Performs standard beta transformation on vector (vec) plus
  # single fixed value (val) which is not part of vector, based on
  # sample size of vector.
  #
  # Purpose: transforms focal IE vector & benchmark reference
  # value, for EIs which used fixed scoring method (q.method='fixed')
  #
  # Accepts: vector & fixed value. Returns transformed vector & value.
  # 	if vec>=0 & vec<=1 OR val>=0 & val<=1 then transforms both such
  # 		that vec>0 & vec<1 AND val>0 & val<1
  # 	If vec>0 & vec<1 AND val>0 & vale<1 (no values equal to 1 or 0) then
  # 		returns original untransformed vector & value
  #
  # Returns: List object containing transformed vector and value
  #
  # Exits with error if any values >1 or <0
  ###################################################

  # Check for illegal values
  if (max(vec) > 1 || min(vec) < 0 || max(val) > 1 || min(val) < 0) {
    error <- "Error: Values >1 or <0 not allowed for beta distribution!"
    print(error)
    return(error)
  }

  if (max(vec) == 1 || min(vec) == 0) {
    # Apply standard beta transformation
    # if zeros or ones present
    n <- length(vec)
    vec <- (vec * (n - 1) + 0.5) / n
    val <- (val * (n - 1) + 0.5) / n
  }

  trans <- list(vec, val)
  return(trans)
}

uni.fit.beta <- function(vec, shape1 = NULL, shape2 = NULL, lowlim = NULL) {
  ###################################################
  # Fits a univariate beta distribution to a vector
  #
  # Parameters:
  # 		vec			The beta-distributed vector
  # 		shape1	[Optional]. Starting value for first beta shape parameter
  # 		shape2	[Optional]. Starting value for second beta shape parameter
  # 		lowlim		[Optional]. Starting value for optimization lower limit
  #
  # Return list object consistion of model parameters and bootstrap
  # confidence limits.
  #
  # Vector must fall in the range 0 <--> 1. Will transform exact
  # 0s and 1s, but values <0 or >1 will cause it to fail
  ###################################################

  # Check required packages
  if (!require(simpleboot)) stop("ERROR: package 'simpleboot' not loaded")
  if (!require(MASS)) stop("ERROR: package 'MASS' not loaded")

  # Some preliminary tests
  if (max(vec) > 1 || min(vec) < 0) {
    error <- "Error: Values >1 or <0 not allowed for beta distribution!"
    print(error)
    return(error)
  }

  if (max(vec) == 1 || min(vec) == 0) {
    # Apply standard beta transformation
    # if zeros or ones present
    n <- length(vec)
    vec <- (vec * (n - 1) + 0.5) / n
  }

  # Set optional parameters if not supplied
  if (is.null(shape1)) shape1 <- 0.1
  if (is.null(shape2)) shape2 <- 0.1
  if (is.null(lowlim)) lowlim <- 0.0001

  # Fit the distribution
  mod <- fitdistr(vec, "beta",
    lower = c(lowlim, lowlim),
    start = list(shape1 = 0.1, shape2 = 0.1)
  )

  # Get parameter estimates
  alpha <- mod$estimate[1]
  beta <- mod$estimate[2]
  mod.mean <- alpha / (alpha + beta)

  # Get bootstrap confidence limits
  # Using simpleboot package
  # Can't use CLs from fitdistr as they are asymptotic (symmetrical) confidence
  # limits; we need the actual asymmetrical CLs
  x.boot <- one.boot(vec, mean, R = 10000)
  mod.boot <- boot.ci(x.boot, type = "bca")
  mod.boot.lcl <- mod.boot$bca[4]
  mod.boot.ucl <- mod.boot$bca[5]

  # Get the half-confidence intervals
  # Can use as measure of effect size
  mod.boot.lci <- as.numeric(mod.mean - mod.boot.lcl)
  mod.boot.uci <- as.numeric(mod.boot.ucl - mod.mean)

  uni.fit.beta.results <- list(
    mod, mod.mean, mod.boot.lcl, mod.boot.ucl, mod.boot.lci,
    mod.boot.uci
  )
  names(uni.fit.beta.results) <- c(
    "mod", "mod.mean", "mod.boot.lcl", "mod.boot.ucl",
    "mod.boot.lci", "mod.boot.uci"
  )
  return(uni.fit.beta.results)
}

best.fit.pois <- function(df) {
  ###################################################
  # Select best fitting one-way GLM regression model for Poisson family.
  # Appropriate for count data.
  # 	Returns best model with significant fit. Order of testing:
  # Poisson (P) --> Negative Binomial (NBin) --> Zero-inflated Poisson (ZIP)
  # --> Zero-inflated Negative Binomial (ZINB)
  # Accepts: data frame which *MUST* have the following columns:
  # 		Trt: factor with two values (coding doesn't matter)
  #  	Rsp: Response. Must be integers >=0
  # Returns: List containing:
  # 			error: FALSE if function fails, else TRUE
  # 			fit.found: TRUE if at least one best model found, otherwise FALSE
  # 			dist.best: Abbreviation for one of four models, as shown aboveb
  # 			dist.best.text: Spelled out name of the best model
  # 			mod.best: the best model (model results, itself a list)
  # 			gf.mod.best: p-value of goodness-of-fit test for best-fitting model
  # 	If no model passes, returns above values for negative binomial model
  ###################################################

  # Some error testing
  if (min(df$Rsp < 0)) {
    error <- "Error: negative values of response not allowed"
    print(error)
    return(error)
  }

  # Set standard distribution codes
  dist.code.p <- "P"
  dist.code.nbin <- "NBin"
  dist.code.zip <- "ZIP"
  dist.code.zinb <- "ZINB"

  # Start by assuming all models don't fit
  mod.fits.p <- F
  mod.fits.nbin <- F
  mod.fits.zip <- F
  mod.fits.zinb <- F

  ########################
  # Test Poisson (P)
  ########################

  # Run the model
  mod <- glm(Rsp ~ Trt, data = df, family = poisson)

  # Run goodness of fit X2 test
  gf <- 1 - pchisq(summary(mod)$deviance, summary(mod)$df.residual)

  # Update success variable if model fits
  if (gf >= 0.05) {
    mod.fits.p <- T
  }

  # Save results for this distribution/model
  mod.p <- mod
  gf.p <- gf

  ########################
  # Test Negative Binomial (NBin)
  ########################

  # Run the model
  mod <- glm.nb(Rsp ~ Trt, data = df)

  # Run goodness of fit X2 test
  gf <- 1 - pchisq(summary(mod)$deviance, summary(mod)$df.residual)

  # Update success variable if model fits
  if (gf >= 0.05) {
    mod.fits.nbin <- T
  }

  # Save results for this distribution/model
  mod.nbin <- mod
  gf.nbin <- gf

  ####################################
  # Zero-inflated models
  # Only test if response contains at least one zero!
  ####################################

  if (FALSE) { # Disabling for now
    # if ( min(df$Rsp)==0 ) {

    ########################
    # Test Zero-inflated Poisson (ZIP)
    ########################

    # Run the model
    mod <- zeroinfl(Rsp ~ Trt | 1, data = df, dist = "poisson")
    mod.sum <- summary(mod)

    # Run goodness of fit measure on zero-inflation coefficient
    gf <- coef(mod.sum)[[2]][4]

    # Update success variable if model fits
    if (gf >= 0.05) {
      mod.fits.zip <- T
    }

    # # Additional test  in case both this and plain P model pass
    # # *** UNDER CONSTRUCTION ****
    # if (mod.fits && mod.fits.p) {
    # vuong(mod.p, mod)
    # # Don't yet know how to save coefficients

    # # Reset mod.fits.p to FAIL if vuong AIC test shows that
    # # ZIP model is superior to P

    # }

    # Save results for this distribution/model
    mod.zip <- mod
    gf.zip <- gf

    ########################
    # Test Zero-inflated Negative Binomial (ZINB)
    ########################

    # Run the model
    # Try first with logit function, if that doesn't work, try probit,
    # if still fail, count model as failed

    mod <- tryCatch(zeroinfl(Rsp ~ Trt | 1, data = df, dist = "negbin"),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )
    if (mod[1] == "FAIL") {
      mod <- tryCatch(zeroinfl(Rsp ~ Trt | 1, data = df, dist = "negbin", link = "probit"),
        error = function(cond) {
          return("FAIL")
        },
        warning = function(cond) {
          return("FAIL")
        }
      )
    }
    if (mod[1] != "FAIL") {
      mod.sum <- summary(mod)

      # Run goodness of fit measure on zero-inflation coefficient
      gf <- coef(mod.sum)[[2]][4]

      # Update success variable if model fits
      if (gf >= 0.05) {
        mod.fits.zinb <- T
      }

      # Save results for this distribution/model
      mod.zinb <- mod
      gf.zinb <- gf
    }
  }
  ###########################
  # Select the best model
  ###########################

  # Stop after first successful model
  fit.found <- T # Assume at least one model fits

  if (mod.fits.p) {
    mod.best <- mod.p
    dist.best.text <- "Poisson"
    gf.mod.best <- gf.p
    dist.best <- dist.code.p
  } else if (mod.fits.nbin) {
    mod.best <- mod.nbin
    dist.best.text <- "Negative Binomial"
    gf.mod.best <- gf.nbin
    dist.best <- dist.code.nbin
  } else if (mod.fits.zip) {
    mod.best <- mod.zip
    dist.best.text <- "Zero-Inflated Poisson"
    gf.mod.best <- gf.zip
    dist.best <- dist.code.zip
  } else if (mod.fits.zinb) {
    mod.best <- mod.zinb
    dist.best.text <- "Zero-inflated Negative Binomial"
    gf.mod.best <- gf.zinb
    dist.best <- dist.code.zinb
  } else {
    # Return NBin model if none fit
    fit.found <- F
    mod.best <- mod.nbin
    dist.best.text <- "Negative Binomial"
    gf.mod.best <- gf.nbin
    dist.best <- dist.code.nbin
  }

  # Save best model pseudo-R2, in this case McFadden's
  r2 <- 1 - (mod.best$null.deviance / mod.best$null.deviance)

  best.fit.results <- list(fit.found, dist.best, dist.best.text, mod.best, gf.mod.best, r2)
  return(best.fit.results)
}

fit.gamma <- function(df) {
  ###################################################
  # Fits one-way GLM regression model using gamma distn
  #
  # Appropriate for continuous variables 0, Inf
  # Accepts: data frame which *MUST* have the following columns:
  # 		Trt: factor with two values (coding doesn't matter)
  #  	Rsp: Response. Must be integers >=0
  # Returns: List containing:
  # 			error: FALSE if function fails, else TRUE
  # 			fit.found: TRUE if at least one best model found, otherwise FALSE
  # 			dist.best: Abbreviation for one of four models, as shown aboveb
  # 			dist.best.text: Spelled out name of the best model
  # 			mod.best: the best model (model results, itself a list)
  # 			gf.mod.best: p-value of goodness-of-fit test for best-fitting model
  # 	If no model passes, returns above values for negative binomial model
  ###################################################

  # Some error testing
  if (min(df$Rsp < 0)) {
    error <- "Error: negative values of response not allowed"
    print(error)
    return(error)
  }

  # Set standard distribution codes
  dist.code.gamma <- "gamma"

  # Start by assuming all models don't fit
  mod.fits.gamma <- F

  ########################
  # Fit model
  ########################

  # Run the model
  # mod <- glm(Rsp ~ Trt, data=df, family = Gamma(link=identity) )
  mod <- glm(Rsp ~ Trt, data = df, family = Gamma(link = "identity")) # log-link

  # Run goodness of fit X2 test
  gf <- 1 - pchisq(summary(mod)$deviance, summary(mod)$df.residual)

  # Update success variable if model fits
  if (gf >= 0.05) mod.fits.gamma <- T

  # Save results for this distribution/model
  mod.gamma <- mod
  gf.gamma <- gf

  ###########################
  # Select the best model
  #
  # Keeping this structure for now, although only
  # fit one model, so nothing to select
  ###########################

  # Stop after first successful model
  fit.found <- T # Assume at least one model fits

  if (mod.fits.gamma) {
    mod.best <- mod.gamma
    dist.best.text <- "Gamma"
    gf.mod.best <- gf.gamma
    dist.best <- dist.code.gamma
  } else {
    # Return NBin model if none fit
    fit.found <- F
    mod.best <- mod.gamma
    dist.best.text <- "Gamma"
    gf.mod.best <- gf.gamma
    dist.best <- dist.code.gamma
  }

  # Save best model pseudo-R2, in this case McFadden's
  r2 <- 1 - (mod.best$null.deviance / mod.best$null.deviance)

  best.fit.results <- list(fit.found, dist.best, dist.best.text, mod.best, gf.mod.best, r2)
  return(best.fit.results)
}

split.zoib <- function(v) {
  ###################################################
  # Split zero-inflated beta vector
  #
  # Purpose:
  # 	Splits a vector of proportions into separate vectors of zeros,
  # 	ones and a pure beta-distributed remainder. Returns
  # 	three vectors: v0 (zeros only), v1 (ones only), and v.beta (the
  # 	remainder distributed on interval (0,1) ), their lengths
  # 	and proportions of the original vector vector length.
  #
  # Returns:
  # 	List object containing the split vectors, their lengths and
  # 	probabilities (proportions of the original vector length)
  ###################################################

  # First check that vector is distributed on [0,1]
  if (any(v > 1) || any(v < 0)) {
    stop("ERROR: Vector has values outside interval [0,1]!")
  }

  # Split the vector
  v0 <- v[v == 0]
  v1 <- v[v == 1]
  v.beta <- v[v != 1 & v != 0]

  # Get sample sizes of ech vector
  n <- length(v)
  n0 <- length(v0)
  n1 <- length(v1)
  n.beta <- length(v.beta)

  # Get proportions of each fraction relative to original vector
  p0 <- n0 / n
  p1 <- n1 / n
  p.beta <- n.beta / n

  v.split <- list(
    v0, v1, v.beta,
    n0, n1, n.beta,
    p0, p1, p.beta
  )
  names(v.split) <- c(
    "v0", "v1", "v.beta",
    "n0", "n1", "n.beta",
    "p0", "p1", "p.beta"
  )

  return(v.split)
}

beta.transform <- function(vec) {
  ###################################################
  # Performs beta transformation on vector distributed on [0.1),
  # (0,1] or [0,1], or returns original vector if already distributed
  # on (0,1). Also checks for zero-length vector.
  ###################################################

  # Check vector is distributed on [0,1]
  msg_err <- "ERROR: Vector has values outside interval [0,1]!"
  # if ( any( vec > 1 ) || any( vec < 0) )  stop(msg_err)

  # Check vector not zero-length
  msg_err <- "ERROR: Vector has zero length!"
  if (length(vec) == 0) stop(msg_err)

  if (max(vec) == 1 || min(vec) == 0) {
    n <- length(vec)
    vec <- (vec * (n - 1) + 0.5) / n
  }

  return(vec)
}

jitter.beta <- function(v, tinybit = NULL) {
  ###################################################
  # Adds small amount of noise to single-valued vector distributed on
  # interval [0,1]. Enables vector to be fit by beta distribution with
  # mean identical or very close to original vaue
  #
  # Vectors which are all 0s will have tiny increase in mean because
  # can only add, not subtract. For analogous reason, vectors which
  # are all 1s will have small decrease in mean.
  #
  # Accepts:
  # 		v			Single-valued vector v
  # 		itsybit	Diplacement (+/-) to be added to vector
  #
  # 	Returns:	The jittered vector
  ###################################################

  if (is.null(tinybit)) tinybit <- 0.0001
  n.v <- length(v)
  v.unique <- unique(v)
  v.numvals <- length(v.unique)
  v.mean <- mean(v, na.rm = T)

  if (v.numvals == 1 && v.unique == 0) {
    e1 <- max(0, v.mean + tinybit / 2)
    e2 <- max(0, v.mean + tinybit)
  } else if (v.numvals == 1 && v.unique == 1) {
    e1 <- min(1, v.mean - tinybit / 2)
    e2 <- min(1, v.mean - tinybit)
  } else {
    e1 <- max(0, v.mean - tinybit)
    e2 <- min(1, v.mean + tinybit)
  }

  v.jittered <- c(e1, e2, v[3:n.v])
  return(v.jittered)
}

fit.beta <- function(
  vec, xs = NULL, shape1 = 0.1, shape2 = 0.1, lowlim = 0.0001,
  boot.cls = FALSE
) {
  ###################################################
  # Fits beta distribution to vector distributed on (0,1)
  #
  # Requires: function fitdistr from package MASS
  #
  # Parameters:
  # 		vec				The vector
  # 		xs					The range of x values over which pdf predicted
  # 		shape1		Starting shape parameter 1
  # 		shape2		Starting shape parameter 2
  # 		lowlim			Lower limit for optimization
  # 		boot.cls		[Optional] Return boostrap 95% CLs, if desired
  #
  # Returns: List object containing:
  # 		pdf			The fitted pdf
  # 		f.shape1	First shape parameter of fitted pdf
  # 		f.shape2	Second shape parameter of fitted pdf
  # 		f.mean		Mean of the fitted distribution
  # 		boot.lcl	Bootstrap lower 95% CL of the vector
  # 		boot.lcl	Bootstrap upper 95% CL of the vector
  #
  # On fail returns message 'FAIL'
  #
  # Note: vector must be distributed on (0,1) and have >1 value
  #####################################################

  maxTries <- 10 # Max time to try fit before reporting error

  if (!require(MASS)) stop("Required package MASS not installed!")

  # Transform if any values==1 or 0
  # Throws error if (a) outside interval [0,1], (b) vector is null,
  # or (c) vector has <2 values
  vec <- vec[!is.na(vec)]
  vec <- beta.transform(vec)

  # # Check for too few values
  # msg_err <- "Vector has only one value!"
  # if ( length( unique(vec) ) <= 1 ) 	stop(msg_err)

  # Set default values for optional parameters & check for errors
  if (is.null(xs)) xs <- seq(0, 1, length.out = 100)

  # Compose fitdistr optional arguments
  opt.args <- paste0(
    ", lower = c(", lowlim, ", ", lowlim, "), start=list(shape1=", shape1,
    ", shape2=", shape2, ")"
  )

  # Construct the fitdistr command
  fit <- paste0("fitdistr(vec, densfun='beta'", opt.args, ")")

  ###########################################
  # Fit pdf
  #
  # Note:
  # (1) Use of eval(parse...) to force evaluation of string
  # 		as a command
  # (2) Tries repeatedly, up to a maximum of maxTries, if
  # 		if optimization fails
  ###########################################

  tries <- 0
  done <- F
  fail <- F
  while (done == F) {
    tries <- tries + 1
    mod <- tryCatch(eval(parse(text = fit)),
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
      pdf <- dbeta(xs, shape1 = mod$estimate[1], shape2 = mod$estimate[2])
      pdf[pdf == Inf] <- NA

      # Save model parameters for overlap calculation
      f.shape1 <- mod[[1]][[1]]
      f.shape2 <- mod[[1]][[2]]
      done <- T
    }
  }

  if (fail == T) {
    pdf <- NA
    f.shape1 <- NA
    f.shape2 <- NA
    f.mean <- NA
    boot.lcl <- NA
    boot.ucl <- NA
  } else {
    # Get model mean
    f.mean <- f.shape1 / (f.shape1 + f.shape2)

    # Set CLs to null in case not calculating
    boot.lcl <- NA
    boot.ucl <- NA

    if (boot.cls == TRUE) {
      ################################################
      # Optionally calculate bootstrap confience limits
      # Using simpleboot package
      # Can't use CLs from fitdistr as they are asymptotic
      # (= symmetrical). We need the asymmetrical CLs
      ################################################

      # Check required package
      if (!require(simpleboot)) stop("ERROR: package 'simpleboot' not loaded")

      # Calculate CLs
      x.boot <- one.boot(vec, mean, R = 10000)
      mod.boot <- boot.ci(x.boot, type = "bca")
      boot.lcl <- mod.boot$bca[4]
      boot.ucl <- mod.boot$bca[5]
    }
  }

  result <- list(pdf, f.shape1, f.shape2, f.mean, boot.lcl, boot.ucl, fail, tries)
  names(result) <- c("pdf", "shape1", "shape2", "f.mean", "boot.lcl", "boot.ucl", "fail", "tries")

  return(result)
}

mylogit <- function(x) {
  # logit transform
  log(x / (1 - x))
}

noise.beta.norm <- function(v, n, sd) {
  ###########################################
  # Accepts beta-distributed vector, logit transforms,
  # adds random noise, then back-transforms
  #
  # v:		The vector
  # n:		Desired sample size of output vector
  # 			doesn't have to be same a input vector
  # sd: 	SD of the normal dist used to introduce noise
  ###########################################

  1 / (1 + exp(-rnorm(n, mean = mylogit(v), sd = sd)))
}

beta.normal <- function(v, n = NULL, sh1 = 0.6, sh2 = 0.1, mn = NULL, sdev = 1) {
  ###########################################
  # Accepts vector on (0,1) and converts to new beta
  # distributed vector with new shape and mean.
  #
  # Parameters:
  # v:			The vector
  # n:			Desired sample size of output vector. Same as
  # 				original vector if not supplied
  # sh1: 		Beta shape parameter 1
  # sh2: 		Beta shape parameter 2
  # mn: 		Expected value of new vector. Uses arithmetic
  # 				mean of original vector if not supplied.
  # sdev: 	SD of the normal dist used to introduce noise
  ###########################################

  # Check for illegal values
  if (any(v <= 0) || any(v >= 1)) {
    msg.err <- paste0("ERROR: Values outside domain (0,1) (func. beta.normal)")
    stop(msg.err)
  }

  require(VGAM) # For betanorm functions

  if (is.null(mn)) mn <- mean(v, na.rm = TRUE)
  # v.no.na <- v[!is.na(v)]
  # if (is.null(n)) n=length(v.no.na)
  if (is.null(n)) n <- length(v)

  # Output new betanormal vector
  v2 <- rbetanorm(n, shape1 = sh1, shape2 = sh2, mean = mn, sd = sdev)

  # v2<- 1 / ( 1+exp(v2) )

  return(v2)
}

#################################################
# Raw data transformations
#
# Expects the following from calling script:
# 		dat: data frame of raw data, with at least the following two columns:
# 				EI: 		Raw ecological indicator
# 			EI.tr: 	NA column to hold the transformed EI
# 		proportions: 		T if data are proportions or percent, otherwise F
#
# 		Expect the following if proportions==TRUE:
# 				covert.percent:   Convert EI from percent to proportion? (T|F)
# 			scale.abund:		  Scale cover to fall between zero and one (T|F)
# 				truncate.at.one:  Set val>1 to 1 and val<0 to 0 (T|F); ignored if scale.abund=T
# 			logit:						Logit transform (T|F)
#################################################

no.trans <- 0

if (proportions == TRUE) {
  # Transformations relevant to proportions

  if (convert.percent == TRUE) {
    no.trans <- no.trans + 1
    if (no.trans == 1) cat(":\n")
    # convert percents to proportions, replacing raw values
    cat("- WARNING: converting percent to proportions...")
    dat$EI <- dat$EI / 100
    cat("done\n")
  }

  dat$EI.tr <- dat$EI
  x <- dat$EI.tr

  if (scale.abund == TRUE) {
    no.trans <- no.trans + 1
    if (no.trans == 1) {
      cat("- WARNING: scaling cover values over 0:1 (scale.abund==TRUE)...")
    }
    # scale cover values so they range between 0-1
    # this may be necessary if cover sums to >100%)
    maxCurr <- max(x)
    minCurr <- min(x)
    maxNew <- 1
    minNew <- 0
    x.scaled <- (((maxNew - minNew) * (x - minCurr)) / (maxCurr - minCurr)) + minNew
    x <- x.scaled
    dat$EI.tr <- x
    x <- dat$EI.tr # Reset vector x to the scaled values
    cat("done\n")
  } else if (truncate.at.one == TRUE) {
    no.trans <- no.trans + 1
    if (no.trans == 1) cat(":\n")
    cat("- WARNING: truncating cover values at 1 (#1)...")
    # Set any values > 1 to 1
    dat$EI.tr[dat$EI.tr > 1] <- 1
    dat$EI[dat$EI > 1] <- 1
    cat("done\n")
  }

  if (logit == TRUE) {
    no.trans <- no.trans + 1
    if (no.trans == 1) cat(":\n")
    cat("- WARNING: applying logit transformation...")
    # logit transform proportions
    minVal <- min(x[x > 0])

    for (i in 1:length(x)) {
      propCover <- x[i]
      if (propCover == 0) {
        dat[i, "EI.tr"] <- log(minVal * 0.000001)
      } else if (propCover >= 1) {
        dat[i, "EI.tr"] <- 0
      } else {
        dat[i, "EI.tr"] <- log(propCover / (1 - propCover))
      }
    }
    cat("done\n")
  }

  # Truncate above 1
  # Shouldn't be necessary if scale.abund==TRHE
  if (truncate.at.one == TRUE) {
    no.trans <- no.trans + 1
    if (no.trans == 1) cat(":\n")
    cat("- WARNING: truncating cover values at 1 (#2)...")
    dat$EI[dat$EI > 1] <- 1
    cat("done\n")
  }
} # End proportions

if (no.trans == 0) cat("...done\n")

# Opening bullshit brace to make stop() work.
# Uncomment to enable use of stop() for debugging.
# If activating, be sure to uncomment last line of this script as well.
# {

#######################################################
# Indicator value preparation and quality calculations
# for ecological indicator (EI) Basal Area (BA): total
# basal area in cm2 per plot
#
# cm2 used instead of m2 due to poor fit of gamma
# distribution to values<1
#
# By: Brad Boyle
# Email: boyle@hg-llc.com
# Created: 23 February 2025
#######################################################

############################################
# Load functions if not already loaded.
############################################

if (!exists("functions.loaded", mode = "function")) source(paste0(SRCDIR, "includes/functions.R"))

##############################
# Ecological indicator (EI) to calculate
##############################

# EI code
curr.EI <- "BA"

##############################
# Names, locations of input file (raw data)
##############################

# Get attributes for this indicator (set in params file)
ei.pars <- ei.params(curr.EI)
ei.name <- ei.pars$ei.name # Full name of this EI, for graphics
EI.name <- ei.name # Temporary, for backwards-compatibility
source.file <- ei.pars$source.file # Name of input file ('speciesCover.csv' or 'speciesStems.csv')
prepare.raw <- ei.pars$prepare.raw # Prepare raw data from scratch?
distn <- ei.pars$distn # Distribution of the EI
q.method <- ei.pars$q.method # Quality method: "fixed" (fixed bm) | "empirical" (overlap)
bm.val <- ei.pars$bm.val # Fixed bm value. Applies only if q.method="fixed"
test.tail <- ei.pars$test.tail # Test tail for this EI
has.strata <- ei.pars$has.stratum # True if this EI has strata, otherwise F
if (has.strata == F) curr.stratum <- ""
exclude.exotics <- ei.pars$exclude.exotics
ei.multiplier <- ei.pars$ei.multiplier
normalize.m2 <- ei.pars$normalize.m2
na.to.zero <- ei.pars$NA.TO.ZERO

###############################
# Outlier options
###############################

# Default values for backwards compatibility.
if (!exists("REMOVE.BA.OUTLIERS")) REMOVE.BA.OUTLIERS <- TRUE
if (!exists("BA.OUTLIER.STDEVS")) BA.OUTLIER.STDEVS <- 4
if (!exists("BA.OUTLIERS.APPEND")) BA.OUTLIERS.APPEND <- TRUE
if (!exists("BA.OUTLIER.UPPER.THRESHOLD")) BA.OUTLIER.UPPER.THRESHOLD <- 500
if (!exists("F.BA.OUTLIERS")) F.BA.OUTLIERS <- "BA_outliers.csv"

# Rename for greater generality of later code
remove.outliers <- REMOVE.BA.OUTLIERS
outliers.stdevs <- BA.OUTLIERS.STDEVS
outliers.append <- BA.OUTLIERS.APPEND
outliers.upper.threshold <- BA.OUTLIERS.UPPER.THRESHOLD
fname.outliers <- F.BA.OUTLIERS

##############################
# Indicator-specific graphing parameters
##############################

# Group strata in single figure?
# Set to true to make extra set of figures grouping
# figures for different strata of same vegetation in
# single figure. MUST be FALSE if no strata for this
# indicator
allow.group.strata <- FALSE

# Minimum value for y axis
# Leave as empty string ("") to allow to vary automatically
y.axis.fixed.min <- 0

# Include landcover in graph title?
# If true, title = landcover + stratum
# If false, title = stratum only
# Applies to single stratum graphs only
lc.in.title <- TRUE

# Single figure legend additional offset
# Positive: move to right, negative: move to left
# Comment out or set to "" or NA to use defaults
leg.x.extra <- -0.1
leg.y.extra <- NA

# Grouped figure legend additional offset NOT YET IMPLEMENTED!
# Positive: move to right, negative: move to left
# Comment out or set to "" or NA to use defaults
leg.x.extra.g <- -0.2
leg.y.extra.g <- NA

##############################
# Names & locations of output files
##############################

rawFileName <- paste(curr.EI, "_raw.csv", sep = "") # Validated raw data
rawFileName.bak <- str_replace(rawFileName, ".csv", ".bak.csv")
rawFile.bak <- paste0(RESULTSDIR, rawFileName.bak)
resultsFocal <- paste(curr.EI, "_focal.csv", sep = "") # Focal indicator file
resultsBm <- paste(curr.EI, "_benchmark.csv", sep = "") # Bm indicator file

#######################################################
# Main
#######################################################

cat("\n")
cat("###########################################################\n")
cat("Prepare raw indicator values for indicator ", curr.EI, " (", ei.name, ")\n", sep = "")
cat("###########################################################\n\n")

if (PLOT.FIGS.ONLY == FALSE) {
  cat("**************************\n")
  cat("Preparing data\n")
  cat("**************************\n")

  if (prepare.raw == TRUE || force.prepare.raw == TRUE) { # START prepare.raw

    cat("Loading input files:\n")

    # Import plot metadata
    cat("- Loading plot metadata from file '", PLOTMETADATA.FILE, "'...", sep = "")
    plotFile <- paste(INPUTDIR, PLOTMETADATA.FILE, sep = "")
    plot.dat <- read.csv(plotFile, header = T)
    cat("done\n")

    # Import stem data
    cat("- Loading stem data from file '", source.file, "'...", sep = "")
    inputFile <- paste(INPUTDIR, source.file, sep = "")
    dat <- read.csv(inputFile, header = T)
    dat <- dat
    cat("done\n")

    # Import species list and update species data with native status
    cat("- Loading species attributes from file '", SPECIES.FILE, "'...", sep = "")
    species.attr.file <- paste(INPUTDIR, SPECIES.FILE, sep = "")
    df.species <- read.csv(species.attr.file, header = T)
    names(df.species)[names(df.species) == "isExotic"] <- "is_exotic" # rename just in case
    dat <- merge(dat, df.species[, c("species", "is_exotic")], by = "species", all.x = TRUE)
    cat("- Checking for missing species native status...", sep = "")

    if (exclude.exotics == TRUE) {
      cat("- Checking for missing species native status...", sep = "")

      if (IS.EXOTIC.MISSING.ASSUME.NATIVE == TRUE) {
        cat("setting unknown native status to 'is_exotic=0'...", sep = "")
        # Will also mark "indet_sp." as native, but not a problem
        dat$is_exotic[is.na(dat$is_exotic)] <- 0
        cat("done\n")
      } else {
        # Quit and warn about missing values
        stop_quietly("ERROR: value of 'is_exotic' missing for one or more species. Please correct.\n")
      }
    }

    ######################################
    # Preliminary validations & standardizations
    ######################################

    # Check that plotCodes are unique across entire dataset.
    # If not, throw error message and stop
    plots <- unique(dat$plotCode)
    allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])
    if (length(plots) != nrow(allPlots)) stop("Plot codes not unique!")

    ######################################
    # Sum basal area per plot
    ######################################

    cat("Calculating  total BA per plot:\n")

    # For backward compatibility, in case this parameter not declared
    if (!exists("normalize.m2")) normalize.m2 <- FALSE

    cat("- Normalizing raw BA values...")
    if (!normalize.m2 == FALSE) {
      ##########################################
      # Use raw DBH values to calculate BA from
      # scratch, normalizing the different size
      # class subsamples to the same standard
      # sampling area (generally, per 1 ha).
      # Parameter 'normalize.m2' set in project-
      # specific parameter file.
      ##########################################
      targetArea.ha <- normalize.m2 / 10000
      cat("normalizing to sampling area of ", targetArea.ha, " ha...", sep = "")

      # Calculate BA from DBH, scaling by the sampling area
      # used for the given dbh class
      dat$ba.scaled <- dbh.ba.scaled(dat$dbh_cm, normalize.m2)
      dat$ba <- dat$ba.scaled
      cat("done\n")
    } else {
      # Use the calculated value of BA already present in input data frame
      # Apply ei.multiplier (this may be 1)
      cat("skipping this step\n")
      dat$ba <- dat$ba_m2 * ei.multiplier
    }

    cat("- Checking for missing values of BA...")
    n.missing <- nrow(dat[is.na(dat$ba), ])
    if (n.missing > 0) {
      cat("WARNING: ", n.missing, " rows are NA for column 'BA'!\n", sep = "")

      if (na.to.zero) {
        cat("-- Setting NAs to zero...done\n")
        dat$ba[is.na(dat$ba)] <- 0
      } else {
        stop_quietly("NA basal area not allowed!\n")
      }
    } else {
      cat("done\n")
    }

    # Calculate total BA per plot
    cat("- Calculating total BA per plot...")
    ba.plot <- aggregate(
      ba ~ plotCode,
      data = dat[, c("plotCode", "ba")],
      FUN = sum
    )
    colnames(ba.plot) <- c("plotCode", "ba")
    cat("done\n")

    # Add plot metadata separately from full plot df

    cat("- Merging in plot metadata...")
    # *** NOTE: deactivating INCLUDE.PLOTS.NODATA==FALSE option
    # Otherwise `source("includes/prepare.results.R")` will crash if
    # no focal plots in stems data set.
    # if (INCLUDE.PLOTS.NODATA) {
    # This approach ensures that all plots end up in final matrix,
    # even those with zero basal area
    cat("including plots with zero basal area...")
    dat <- merge(
      plot.dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")],
      ba.plot,
      by = "plotCode",
      all.x = TRUE
    )
    # Newly added plots, if any, will have BA==NA
    # Set these new NAs to zero
    dat$ba[is.na(dat$ba)] <- 0
    # } else {
    #   cat("excluding plots with no basal area data, if any...")
    #   # This approach includes only plots which have
    #   # at least one BA measurement
    #   dat <- merge(
    #     plot.dat[ , c('plotCode','focalOrBenchmark','landCover','vegClass')],
    #     ba.plot,
    #     by='plotCode'
    #   )
    # }
    cat("done\n")

    if (remove.outliers == TRUE) { # BEGIN remove.outliers2
      #######################################################
      # Remove previously-identified outlier plots, if any.
      # Only happens if this indicator script has been run at
      # least once before and file fname.outliers exists in
      # the results directory
      #######################################################

      f.outliers <- paste0(RESULTSDIR, fname.outliers)

      if (file.exists(f.outliers)) {
        cat("\nChecking for previously-saved ", curr.EI, " outliers:\n")
        df.outliers <- read.csv(f.outliers, header = TRUE)
        outlier.plots <- unique(df.outliers$plotCode)
        n.outliers <- length(outlier.plots)
        cat(paste0("- Found ", n.outliers, " outlier plots in file '", fname.outliers, "'\n"))

        # Delete the offending plots
        cat(paste0("- Deleting ", n.outliers, " ", curr.EI, " outliers..."))
        n.plots.before <- length(unique(dat$plotCode))
        dat <- dat[!dat$plotCode %in% outlier.plots, ]
        cat("done\n")

        n.plots.after <- length(unique(dat$plotCode))
        cat("- Plots before: ", n.plots.before, ", plots after: ", n.plots.after, "\n")
      }
    } # END remove.outliers2

    ################################
    # Validations
    ################################

    cat("Performing validations and transformations...")
    # Check plotCodes are unique across both benchmark and focal data.
    # If not, stop and edit plot codes in original data matrix to ensure uniqueness
    plots <- unique(dat$plotCode)
    allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])
    if (length(plots) != nrow(allPlots)) stop("Plot codes not unique!")
    cat("done\n")

    #######################################################
    # Add pseudo-stratum column
    #######################################################

    # Get column names
    cols <- colnames(dat)

    # Reorder columns, keeping stratum column if present,
    # and keeping the transformed EI column
    if (!any("stratum" %in% cols)) {
      has.stratum <- FALSE
      dat$stratum <- "nostrata"
    }

    #######################################################
    # Rename indicator column to generic "EI" & tidy up
    #######################################################

    # Rename indicator column
    names(dat)[names(dat) == "ba"] <- "EI"

    # Tidy up
    dat <- dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI")]
    dat$EI.tr <- dat$EI # No transformations needed; EI.tr same as EI

    if (remove.outliers == TRUE) { # BEGIN remove.outliers2
      #######################################################
      # Flag indicator outlier plots for this indicator and
      # save to file. This file can be used to exclude outlier
      # plots by repeating the raw data import after setting
      #######################################################

      cat("Checking for ", curr.EI, " outliers...")

      # Extract landCover & bm vegClasses again as these may have changed
      # Focal land cover classes only
      landCoverClasses <- unique(dat[dat$focalOrBenchmark == "f", c("landCover")])
      tot.landCoverClasses <- length(landCoverClasses)
      # bm veg for each land cover
      landCoverVegClasses <- unique(dat[dat$focalOrBenchmark == "f", c("landCover", "vegClass")])
      dat$outlier <- FALSE

      for (i in 1:tot.landCoverClasses) { # BEGIN outlier landCover loop
        # Get current landCover and bm vegetation codes
        curr.landCover <- landCoverClasses[i]
        curr.vegClass <- landCoverVegClasses[landCoverVegClasses$landCover == curr.landCover, c("vegClass")]

        # Extract matrix of plots for this landCover
        # Filter focal and bm separately, then combine
        f.dat <- dat[dat$landCover == curr.landCover & dat$focalOrBenchmark == "f", ]
        b.dat <- dat[dat$vegClass == curr.vegClass & dat$focalOrBenchmark == "b", ]
        ind.vals <- rbind(f.dat, b.dat)
        fb.vals <- c("f", "b")

        for (fb in fb.vals) { # BEGIN for ( fb in fb.vals )
          curr.ind.vals <- ind.vals[ind.vals$focalOrBenchmark == fb, ]

          # Test #1:
          # ind.val > fixed maximum (=outliers.upper.threshold)
          # Extreme outliers must be removed as they can prevent Test #2
          # from working
          curr.ind.vals$outlier[
            curr.ind.vals$EI.tr > outliers.upper.threshold
          ] <- TRUE

          # Test #2:
          # ind.val outside outliers.stdevs standard
          # deviations of mean(ind.val).
          mean <- mean(curr.ind.vals$EI.tr)
          sds <- sd(curr.ind.vals$EI.tr) * outliers.stdevs
          upper <- mean + sds
          lower <- mean - sds

          # Flag the outliers
          curr.ind.vals$outlier[
            curr.ind.vals$EI.tr > upper | curr.ind.vals$EI.tr < lower
          ] <- TRUE

          # Update outlier column in dat for this set of plots
          curr.plots <- curr.ind.vals$plotCode
          dat$outlier[dat$plotCode %in% curr.plots] <- curr.ind.vals$outlier
        } # END for ( fb in fb.vals )
      } # END outlier landCover loop

      if (!file.exists(f.outliers)) {
        # Save backup of raw data file if this is the first run
        # Backup can be used to plot outliers
        write.csv(dat, file = rawFile.bak, row.names = FALSE)
      }

      # count outliers
      n.outliers <- length(dat$plotCode[dat$outlier == TRUE])

      if (n.outliers == 0) {
        cat("no new outliers found\n")
      } else {
        cat(n.outliers, " new outliers found!\n", sep = "")

        cat("Saving ", curr.EI, " outliers:\n", sep = "")
        outliers <- dat[dat$outlier == TRUE, ]
        outliers <- unique(outliers[, c("plotCode", "landCover", "vegClass", "focalOrBenchmark", "EI", "EI.tr")])
        # dat <- dat[ dat$outlier==FALSE, ]

        if (!file.exists(f.outliers)) {
          # Save the outliers to new file
          cat("- Saving new file '", fname.outliers, "'...", sep = "")
        } else {
          if (outliers.append == TRUE) {
            cat("- Appending to existing file '", fname.outliers, "'...", sep = "")
            outliers.orig <- read.csv(f.outliers, header = TRUE)
            outliers <- rbind(outliers.orig, outliers)
            outliers <- unique(outliers)
          } else {
            # Warn and replace
            cat("- Replacing existing file '", fname.outliers, "'...", sep = "")
          }
        }
        write.csv(outliers, file = f.outliers, row.names = FALSE)
        cat("done\n")
      }
    } # END remove.outliers2

    # Save raw indicator data
    cat("Saving prepared raw indicator values to 'results/", rawFileName, "'...", sep = "")
    rawFile <- paste(RESULTSDIR, rawFileName, sep = "")
    write.csv(dat, file = rawFile, row.names = FALSE)
    cat("done\n\n")
  } # END prepare.raw

  #######################################################
  # Calculate and save benchmark values
  #######################################################

  # Re-import prepared raw data
  inputFile <- paste(RESULTSDIR, rawFileName, sep = "")
  # cat("inputFile=",inputFile, "\n", sep="")
  dat <- read.csv(inputFile, header = T)
  dat <- dat

  # Make data frame of moments for transformed cover
  # aggregated by vegclass + stratum
  EI.bm <- subset(dat, dat$focalOrBenchmark == "b")

  EI.bm <- with(EI.bm, aggregate(EI.tr, list(vegClass = vegClass),
    FUN = function(x) {
      c(
        n = length(x),
        mean = mean(x),
        med = median(x),
        sd = sd(x),
        max = max(x),
        min = min(x)
      )
    }
  ))
  EI.bm <- cbind(EI.bm[-ncol(EI.bm)], EI.bm[[ncol(EI.bm)]])
  colnames(EI.bm) <- c("vegClass", "n", "EI.mean", "EI.med", "EI.sd", "EI.max", "EI.min")

  # Add dummy stratum column
  EI.bm$stratum <- "nostrata"

  # Add EI & rearrange the columns
  EI.bm$EI <- curr.EI # add indicator
  EI.bm <- EI.bm[, c("EI", "vegClass", "stratum", "n", "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min")]

  # Make final, human-readable veg classes
  EI.bm$vegClass <- human.readable(EI.bm$vegClass)

  # Inspect the columns of EI.bm
  sorted.EI.bm <- EI.bm[order(EI.bm$vegClass), ]
  head(sorted.EI.bm)

  # save benchmark results file
  resultsFile <- paste(RESULTSDIR, resultsBm, sep = "")
  write.csv(EI.bm, file = resultsFile, row.names = FALSE)

  ####################################################
  # Prepare (focal) results file, and populate basic summary stats
  ####################################################

  source("includes/prepare.results.R")

  ####################################################
  # Calculate quality
  ####################################################

  source("includes/quality.R")
} else {
  cat("**************************\n")
  cat("Generating figures\n")
  cat("**************************\n")
} # END PLOT.FIGS.ONLY

####################################################
# Plot overlap
####################################################

if (plot.overlap == T) source("includes/overlap.R")

####################################################
# Graph the distributions for each vegetation type
# Include model statistic and overlap scores on figures if requested
####################################################

source("includes/call.graph.dists.R")

cat("\n##################################################\n")
cat("Operation completed\n")
cat("##################################################\n\n")

#######################################################
# End of script
#######################################################

# Closing bullshit brace to make stop() work
# Uncomment to enable use of stop() for debugging
# If activating, be sure to uncomment opening brace at start of
# script as well
# }

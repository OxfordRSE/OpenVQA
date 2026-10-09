# Opening bullshit brace to make stop() work.
# Uncomment to enable use of stop() for debugging.
# If activating, be sure to uncomment last line of this script as well.
# {

#######################################################
# Indicator value preparation and quality calculations
# for ecological indicator (EI) Canopy Height (CH): Mean
# Canopy Height in m, using the average of the tallest 10%
# of trees in plot
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
curr.EI <- "CH"

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
  cat("########################\n")
  cat("Preparing data\n")
  cat("########################\n\n")

  if (prepare.raw == TRUE || force.prepare.raw == TRUE) { # START prepare.raw

    cat("Loading raw data...")

    # Import plot data
    plotFile <- paste(INPUTDIR, PLOTMETADATA.FILE, sep = "")
    plot.dat <- read.csv(plotFile, header = T)

    # Import stem data
    inputFile <- paste(INPUTDIR, source.file, sep = "")
    dat <- read.csv(inputFile, header = T)
    dat <- dat

    # Import species list and update species data with native status
    species.attr.file <- paste(INPUTDIR, SPECIES.FILE, sep = "")
    df.species <- read.csv(species.attr.file, header = T)
    names(df.species)[names(df.species) == "isExotic"] <- "is_exotic" # rename just in case
    dat <- merge(dat, df.species[, c("species", "is_exotic")], by = "species", all.x = TRUE)

    if (exclude.exotics == TRUE) {
      if (IS.EXOTIC.MISSING.ASSUME.NATIVE == TRUE) {
        # Will also mark "indet_sp." as native, but not a problem
        dat$is_exotic[is.na(dat$is_exotic)] <- 0
      } else {
        # Quit and warn about missing values
        stop_quietly("ERROR: value of 'is_exotic' missing for one or more species. Please correct.\n")
      }
    }
    cat("done\n")

    ######################################
    # Preliminary validations & standardizations
    ######################################

    # Check that plotCodes are unique across entire dataset.
    # If not, throw error message and stop
    plots <- unique(dat$plotCode)
    n.plots <- length(plots)
    allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])
    if (n.plots != nrow(allPlots)) stop("Plot codes not unique!")

    ######################################
    # Transformations
    ############################

    # None
    # dat$ba <- dat$ba_m2 * ei.multiplier

    ############################
    # Get mean height of n tallest
    # trees in each plot
    ############################

    n <- 5
    cat("Flagging the ", n, " tallest trees in each plot...", sep = "")

    dat.top.n <- dat %>%
      arrange(desc(height_m)) %>%
      group_by(plotCode) %>%
      slice(1:n)

    dat.ch <- aggregate(
      height_m ~ plotCode,
      data = dat.top.n,
      FUN = "mean" # Quotes added to bypass error due to naming conflict
    )
    cat("done\n")

    ################################
    # Add back the essential plot
    # metadata columns
    ################################

    cat("Merging in plot metadata...")
    # Add plot metadata separately from full plot df
    # This approach ensures that all plots end up in final matrix,
    # even those with zero basal area
    dat <- merge(
      plot.dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")],
      dat.ch,
      by = "plotCode",
      all.x = TRUE
    )
    if (nrow(dat[is.na(dat$height_m), ]) > 0) {
      dat$height_m[is.na(dat$height_m)] <- 0
    }
    cat("done\n")

    ################################
    # Validations
    ################################

    cat("Performing validations...")
    # Check plotCodes are unique across both benchmark and focal data.
    # If not, stop and edit plot codes in original data matrix to ensure uniqueness
    plots <- unique(dat$plotCode)
    allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])
    if (length(plots) != nrow(allPlots)) stop("Plot codes not unique!")
    cat("done\n")

    ######################################
    # Transformations
    ############################

    cat("Performing transformations:\n")

    cat("- Adding one to all values to avoid distribution distortions due to zeros...")
    dat$height_m <- dat$height_m + 1
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
    names(dat)[names(dat) == "height_m"] <- "EI"

    # Tidy up
    dat <- dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI")]
    dat$EI.tr <- dat$EI # No transformations needed; EI.tr same as EI

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
  cat("inputFile=", inputFile)
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
  cat("########################\n")
  cat("Generating figures\n")
  cat("########################\n\n")
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

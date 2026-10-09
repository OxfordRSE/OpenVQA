# Opening bullshit brace to make stop() work.
# Uncomment to enable use of stop() for debugging.
# If activating, be sure to uncomment last line of this script as well.
# {

#######################################################
# Indicator value preparation and quality calculations
# for ecological indicator (EI) Abundance by Size Class
# (ASC): Number of individuals per DBH class
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

# Current EI (ecological indicator)
# Standard abbreviation for this EI
curr.EI <- "ASC"

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
na.to.zero <- ei.pars$NA.TO.ZERO

##############################
# Indicator-specific graphing parameters
##############################

# Group strata in single figure?
# Set to true to make extra set of figures grouping
# figures for different strata of same vegetation in
# single figure. MUST be FALSE if no strata for this
# indicator
allow.group.strata <- TRUE

# Set to true to print sample size for each panel in grouped figures
# Ignored if allow.group.strata==FALSE
group.strata.print.all.n <- FALSE

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
leg.x.extra <- 0
leg.y.extra <- NA

# Grouped figure legend additional offset NOT YET IMPLEMENTED!
# Positive: move to right, negative: move to left
# Comment out or set to "" or NA to use defaults
leg.x.extra.g <- -0.2
leg.y.extra.g <- NA

##############################
# Transformation options
##############################

# TRUE if data are proportions or percent, otherwise false
# proportions==TRUE causes transformations.R to be loaded & run
proportions <- FALSE

# Transform zeros to ones (NBin distns only)
# Prevents zero-related crashes
zero_to_one <- TRUE

# Scalar multiplier
# Multiply EI by this constant
# Use if counts are very small, or if standardizing counts to unit
# area cause some non-zero counts to be <1
# Set to NA to ignore
# WARNING: Must be NA or 1 for NBin distributions!
# Do not change! Keep this here to over-ride any mistakes in params file
ei.multiplier <- 1

# Round ei to nearest integer value?
# Recommended for negative binomial distribution
# WARNING: this will set 1 to 0 if ei.multiplier<0.5!!!
int.ei <- TRUE

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

    cat("Loading raw data:\n")

    cat("- Plots...")
    plotFile <- paste(INPUTDIR, PLOTMETADATA.FILE, sep = "")
    dat.plot <- read.csv(plotFile, header = T)
    dat.plot$plotCode <- as.character(dat.plot$plotCode)
    cat("done\n")

    cat("- Stem data...")
    inputFile <- paste(INPUTDIR, source.file, sep = "")
    dat <- read.csv(inputFile, header = T)
    dat$plotCode <- as.character(dat$plotCode)
    cat("done\n")

    # Import species list and update species data with native status
    species.attr.file <- paste(INPUTDIR, SPECIES.FILE, sep = "")
    df.species <- read.csv(species.attr.file, header = T)
    names(df.species)[names(df.species) == "isExotic"] <- "is_exotic" # rename just in case
    dat <- merge(dat, df.species[, c("species", "is_exotic")], by = "species", all.x = TRUE)

    cat("- Species master list...")
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

    cat("Checking plot codes unique...")

    # Check that plotCodes are unique across entire dataset.
    # If not, throw error message and stop
    plots <- unique(dat$plotCode)
    allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])

    if (length(plots) != nrow(allPlots)) {
      stop("Plot codes not unique!")
    } else {
      cat("passed\n")
    }

    ##########################################################
    # Prepare data frame of all plot x stratum combinations
    # Important to do this BEFORE blacklist/whitelist filtering
    ##########################################################

    # Get all unique strata & plots across entire data set, before filtering
    cat("Preparing list of strata...")
    strata <- unique(dat$stratum)
    cat("done\n")

    cat("Preparing list of plots...")
    if (INCLUDE.PLOTS.NODATA) {
      # Include all plots, even those without any cover data
      cat("including plots without stem data, if any...")
      plots <- unique(dat.plot$plotCode)
    } else {
      cat("including only plots with stem data...")
      # Include only plots with cover data
      plots <- unique(dat$plotCode)
    }
    cat("done\n")

    # Turn into data frame
    plot.strata <- expand.grid(plots, strata, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    colnames(plot.strata) <- c("plotCode", "stratum")
    plot.strata$plot.stratum <- paste0(plot.strata$plotCode, "-", plot.strata$stratum)

    # Add plot metadata to facilitate join to whitelist/blacklist (see below)
    cat("Combining plots and strata into data frame of all plotxstratum combinations...")
    plot.meta <- unique(dat.plot[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")])
    plot.strata <- merge(plot.strata, plot.meta, by = "plotCode", all.x = TRUE)
    cat("done\n")

    ##########################################################
    # Import whitelist, blacklist files
    ##########################################################

    cat("- Importing and preparing stratum include files...")

    # Blacklist: EI+stratum combinations to exclude
    # All combinations are included; categories to exclude flagged include=FALSE
    input.file <- paste(INPUTDIR, BLACKLIST.FILE, sep = "")
    blacklist <- read.csv(input.file, header = T)
    blacklist <- blacklist[, c("EI", "stratum", "f.group", "include")]

    # Whitelist: Exceptions to blacklist, for individual (benchmark) vegetation classes
    input.file <- paste(INPUTDIR, WHITELIST.FILE, sep = "")
    whitelist <- read.csv(input.file, header = T)
    whitelist <- whitelist[!is.na(whitelist$include), ]
    whitelist <- whitelist[, c("EI", "stratum", "bm.vegetation", "include")]

    cat("done\n")

    ##########################################################
    # Flag strata to include
    ##########################################################

    cat("- Filtering excluded strata...")

    # Make df of strata to include & update data
    strata.include <- blacklist[blacklist$EI == curr.EI, c("stratum", "include")]
    dat <- merge(dat, strata.include, by = "stratum", all.x = TRUE)

    # Make df of excluded strata exceptions	& update data
    strata.include.exceptions <- unique(whitelist[
      whitelist$EI == curr.EI & whitelist$include == TRUE,
      c("stratum", "bm.vegetation", "include")
    ])
    colnames(strata.include.exceptions) <-
      c("stratum", "vegClass", "include.exceptions")
    dat <- merge(dat, strata.include.exceptions,
      by = c("stratum", "vegClass"), all.x = TRUE
    )

    # Transfer exceptions to include column
    if (nrow(strata.include.exceptions) > 0) {
      dat$include[dat$include.exceptions == TRUE] <- TRUE
    }

    # Remove excluded
    dat$include <- as.logical(dat$include)
    dat <- dat[dat$include == TRUE, ]

    cat("done\n")


    #######################################################
    # Reduce df to one row per individual, keeping only the
    # largest DBH stem of each individual
    #######################################################

    cat("Aggregating stems to one row per individual...")

    dat$plotCode_ind_id <- paste0(dat$plotCode, "-", dat$ind_id)

    dat.ind <- aggregate(
      dbh_cm ~ plotCode + stratum + plotCode_ind_id,
      data = dat,
      FUN = max
    )

    # The following will remove duplicate records in the
    # unlikely event an individual has >1 largest stems
    # with the same DBH
    dat.ind <- unique(dat.ind)

    cat("done\n")

    #######################################################
    # Aggregate by stratum, counting individuals
    #######################################################

    cat("Aggregating individuals to counts of individuals per stratum...")

    dat.strat <- dat.ind
    dat.strat$plotCode_stratum <- paste0(dat.strat$plotCode, "-", dat.strat$stratum)
    # dat.strat.ind <- merge( dat.ind, dat.strat, by="plotCode_ind_id", all.x=TRUE)

    # dat.strat <- unique( dat.strat[ , c("plotCode", "stratum", "plotCode_stratum")] )
    dat.strat$no.ind <- 1

    dat.strat.abund <- aggregate(
      no.ind ~ plotCode + stratum + plotCode_stratum,
      data = dat.strat,
      FUN = sum
    )
    cat("done\n")

    #######################################################
    # Rename the final df back to dat and rename indicator
    # column to generic "EI"
    #######################################################

    dat <- dat.strat.abund[c("plotCode", "stratum", "no.ind")]
    names(dat)[names(dat) == "no.ind"] <- "EI"

    #######################################################
    # Add missing strata to each plot
    # These get value of ind=0
    #######################################################

    cat("- Adding missing plot strata...")

    # Purge excluded stratum-veg combos, same method as for main data, above
    plot.strata <- merge(plot.strata, strata.include, by = "stratum", all.x = TRUE)
    plot.strata <- merge(plot.strata, strata.include.exceptions,
      by = c("stratum", "vegClass"), all.x = TRUE
    )
    plot.strata$include[plot.strata$include.exceptions == TRUE] <- TRUE
    plot.strata <- plot.strata[plot.strata$include == TRUE, ]

    # Drop the include columns to avoid clutter
    drop.cols <- c("include", "include.exceptions")
    plot.strata <- plot.strata[, !names(plot.strata) %in% drop.cols]

    # Add plot-stratum join column to plot data & extract
    # all plot-stratum combinations
    dat$plot.stratum <- paste0(dat$plotCode, "-", dat$stratum)
    dat.plot.strata <- unique(dat[, c("plot.stratum"), drop = FALSE])
    dat.plot.strata$in.dat <- 1

    # Flag plot.strata already present & remove
    plot.strata <- merge(plot.strata, dat.plot.strata, by = "plot.stratum", all.x = TRUE)
    plot.strata <- plot.strata[is.na(plot.strata$in.dat), ]
    drop.cols <- c("in.dat")
    plot.strata <- plot.strata[, !(names(plot.strata) %in% drop.cols)]


    # Merge in metadata to dat & rearrange
    plot.meta <- unique(plot.meta)
    dat <- merge(dat, plot.meta, by = "plotCode", all.x = TRUE)
    dat <- dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI")]

    n.missing <- nrow(plot.strata)
    if (n.missing > 0) {
      # Rename, add zero cover column and reorder to match dat
      dat.missing <- plot.strata
      dat.missing$EI <- 0
      dat.missing <- dat.missing[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI")]

      # Add missing records to main data frame
      dat <- rbind(dat, dat.missing)
      cat(n.missing, " plot-stratum combinations added\n")
    } else {
      cat("all strata already present\n")
    }

    #######################################################
    # Add missing plots
    #######################################################

    cat("Adding missing plots...")

    # Plots current in dat
    curr.plots <- unique(dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")])
    curr.plots$in.curr.plots <- TRUE
    curr.plots <- curr.plots[, c("plotCode", "in.curr.plots")]

    # All plots (from plotMetadata)
    all.plots <- unique(dat.plot[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")])
    all.plots <- merge(all.plots, curr.plots, by = "plotCode", all.x = TRUE)
    plots.missing <- all.plots[is.na(all.plots$in.curr.plots), ]

    # Add missing plots, if any
    n.missing <- nrow(plots.missing)

    if (n.missing > 0) {
      # First, add strata
      all.strata <- as.data.frame(unique(dat[, c("stratum")]))
      colnames(all.strata) <- "stratum"
      plots.missing <- plots.missing[, !names(plots.missing) == "in.curr.plots"]
      plots.missing <- merge(plots.missing, all.strata, by = NULL) # Cross product

      # Rename, add zero cover column and reorder to match dat
      plots.missing$EI <- 0
      plots.missing <- plots.missing[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI")]

      # Add missing records to main data frame & reorder
      dat <- rbind(dat, plots.missing)
      dat <- dat[order(dat$vegClass, dat$landCover, dat$focalOrBenchmark, dat$plotCode, dat$stratum), ]
      rownames(dat) <- NULL # Reset row numbers
      cat(n.missing, " plots added\n")
    } else {
      cat("all plots already present\n")
    }

    #######################################################
    # Perform transformations, if applicable
    #######################################################

    # Set zeros to one, if requested (NBin distns only)
    if (zero_to_one == TRUE && distn == "NBin") dat$EI[dat$EI == 0] <- 1

    # Perform additional transformations, if applicable, and add EI.tr
    # NOTE: TIDY UP!!! All transformations and downstream
    # calculation should be performed on EI.tr only.
    # Do not do this casually: must check ALL functions
    # and ALL indicator scripts. Everything needs to be
    # updated at once or everything will break.
    if (!is.na(ei.multiplier)) dat$EI <- dat$EI * ei.multiplier
    if (int.ei == TRUE && distn == "NBin") dat$EI <- round(dat$EI)
    dat$EI.tr <- dat$EI

    ################################
    # Final validations
    ################################

    cat("Performing validations and transformations...")

    # Check plotCodes unique, both benchmark and focal
    plots <- unique(dat$plotCode)
    allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])

    if (length(plots) != nrow(allPlots)) {
      stop("Plot codes not unique!")
    } else {
      cat("done\n")
    }

    #######################################################
    # Add pseudo-stratum column
    #######################################################

    # Get column names
    cols <- colnames(dat)

    # Reorder columns, keeping stratum column if present,
    # or adding column with pseudo-stratum value "nostrata"
    if (!any("stratum" %in% cols)) {
      has.stratum <- FALSE
      dat$stratum <- "nostrata"
    }

    #######################################################
    # Tidy up
    #######################################################

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

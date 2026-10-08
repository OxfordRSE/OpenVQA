# Opening bullshit brace to make stop() work.
# Uncomment to enable use of stop() for debugging.
# If activating, be sure to uncomment last line of this script as well.
# {

#######################################################
# Quality Hectares Ecological Indicator (EI) Calculation
# Percent exotic species by stratum (PCESS)
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
# 9 April 2013
#
# Purpose:
# Calculates percent cover of exotic species within each stratum,
# relative to sample of benchmark sites from same (target) vegetation
# class. Relative scores are calculated separately for each stratum in
# each focal site, and the average used to discount the final
# quality coefficient.
#
# Requires tab-delimited file of raw EI values:
# 		plotCode: 						unique alphanumeric code for plot
# 		focalOrBenchmark: 	'b' for benchmark, 'f' for focal site
# 		vegClass: 							short code for vegetation class or category
# 		stratum: 							size class (stratum)
# 		cover: cover of all vegetation this stratum
#
# Note: percentages in field "cover" MUST be converted
#   to proportions!
#
# 		Where the aggregate percent of exotic species is not recorded directly,
# 		calculate from a species cover by stratum file, summing the percent
# 		cover of all exotic species in the stratum. If this results in cover
# 		values>1 (100%) in some strata, be sure to select scale.abund<-TRUE under
#  	"Transformation options", below
#######################################################

############################################
# Load functions if not already loaded.
############################################

if (!exists("functions.loaded", mode = "function")) source(paste0(SRCDIR, "includes/functions.R"))

#######################################################
# Parameters
#######################################################

#################################
# Legacy parameters moved to params file
# Also kept here for backward-compability
# with older scripts. In newer script, values
# set here will be over-ridden by values in
# function ei.params, as set in params file.
# Remove once all files have been updated.
#################################

# Set TRUE if data are percent, FALSE if they are proportions or N/A
# If TRUE will divide values by 100
convert.percent <- FALSE

# Prepare input data from scratch?
prepare.raw <- FALSE

# Remove all-zero cover plots?
remove.zero.cover.plots <- FALSE

#############################
# Ecological indicator (EI) to calculate
##############################

curr.EI <- "PCESS" # Use short code

##############################
# Input file(s) for this indicator
##############################

inputFileName <- "exoticCoverByStratum.csv"

##############################
# Load attributes for this indicator
# (ei.params set in params file)
##############################

# Import parameters set in params file
ei.pars <- ei.params(curr.EI)

# Extract the indicator values
ei.name <- ei.pars$ei.name # Full name of this EI, for graphics
EI.name <- ei.name # Temporary, for backwards-compatibility
prepare.raw <- ei.pars$prepare.raw # Prepare raw data from scratch?
convert.percent <- ei.pars$convert.percent # Convert percent cover to proportions
distn <- ei.pars$distn # Distribution of the EI
q.method <- ei.pars$q.method # Quality method: "fixed" | "empirical" (overlap)
bm.val <- ei.pars$bm.val # Applies only if q.method="fixed"
test.tail <- ei.pars$test.tail # Test tail for this EI
has.strata <- ei.pars$has.stratum # True if this EI has strata, otherwise F
na.to.zero <- ei.pars$NA.TO.ZERO # Set NA cover to zero

# # Reset has.strata if only using stratum "Herb"
# if ( exists( "pcess.herbs.only", mode="function") ) {
#   if ( pcess.herbs.only==TRUE ) has.strata <- FALSE
# }
has.stratum <- has.strata # Hack for backward compatibility
if (has.strata == FALSE) curr.stratum <- ""

# For next two, copy settings in ei.params.teck-pom.R if missing
scale.abund <- ei.pars$scale.abund
logit <- ei.pars$logit

##############################
# Purge excluded strata and stratum-veg
# combination from the prepared data file?
#
# Temporary parameter (i.e., a hack).
# Keep set to FALSE for now, until revise
# downstream scripts to prevent crashing.
# Should not be a problem as excluded strata
# and stratum-veg combination are excluded
# from final calculations of overall quality
# and quality hectares.
##############################

purge.excluded.strata <- FALSE

##############################
# Indicator-specific graphing parameters
##############################

allow.group.strata <- TRUE # Set to true to make extra set of figures grouping
# figures for different strata of same vegetation in
# single figure
# # Reset if only one stratum due to pcess.herbs.only==TRUE
# if ( has.strata==FALSE ) {
#   allow.group.strata <- FALSE
# }

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

##############################
# Names and locations of output files
##############################

rawFileName <- paste(curr.EI, "_raw.csv", sep = "") # Complete raw data with transformations
resultsFocal <- paste(curr.EI, "_focal.csv", sep = "")
resultsBm <- paste(curr.EI, "_benchmark.csv", sep = "")

##############################
# Transformation options
##############################

# Must be TRUE for this indicator
proportions <- TRUE

#  Set TRUE to scale abundance values between 0 and 1
# Useful if cover method sums to >100% (or >1
# proportional abundance)within a given stratum
scale.abund <- FALSE

# Set to TRUE to set any proportions > 1 to 1
# scale.abund=TRUE does this automatically
# you MUST either scale or truncate at one for beta distribution
truncate.at.one <- TRUE

# Set TRUE to logit transform (must also convert to proportions)
logit <- FALSE

#######################################################
# Main
#######################################################

cat("\n##################################################\n")
cat("Operation: Calculating Quality for indicator group PCESS\n")
cat("(Percent Cover Exotic Species by Stratum)\n")
cat("##################################################\n\n")

if (PLOT.FIGS.ONLY == FALSE) {
  cat("************************\n")
  cat("Preparing data\n")
  cat("************************\n")

  if (prepare.raw == TRUE || force.prepare.raw == TRUE) { # START prepare.raw

    cat("Preparing raw data from scratch:\n")

    #######################################################
    # Load raw data
    #######################################################

    cat("- Importing raw data...")

    # Import table containing cover values of all species each plot,
    # plus vegetation class by stratum (growth form)
    inputFile <- paste(INPUTDIR, inputFileName, sep = "")
    dat.raw <- read.csv(inputFile, header = T)
    dat <- dat.raw

    # Import plot metadata
    plotFile <- paste(INPUTDIR, PLOTMETADATA.FILE, sep = "")
    plotMetadata <- read.csv(plotFile, header = T)

    # Check for NA values of cover
    cat("- Checking for missing values of cover...")
    n_NA <- nrow(dat[is.na(dat$cover), ])

    if (n_NA == 0) {
      cat("no NAs found\n")
    } else {
      cat(n_NA, " NAs found!\n", sep = "")

      if (na.to.zero) {
        cat("- Setting NAs to 0...")
        dat$cover[is.na(dat$cover)] <- 0
      } else {
        cat("- Deleting ", n_NA, "rows with NA cover...", sep = "")
        dat <- dat[!is.na(dat$cover), ]
      }
      cat("done\n")
    }

    ##########################################################
    # Prepare data frame of all plot x stratum combinations
    # Important to do this BEFORE blacklist/whitelist filtering
    ##########################################################

    # Get all unique strata & plots across entire data set, before filtering
    strata <- unique(dat$stratum)

    if (INCLUDE.PLOTS.NODATA) {
      # Include all plots, even those without any cover data
      plots <- unique(plotMetadata$plotCode)
    } else {
      # Include only plots with cover data
      plots <- unique(dat$plotCode)
    }

    # Turn into data frame
    plot.strata <- expand.grid(plots, strata, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    colnames(plot.strata) <- c("plotCode", "stratum")
    plot.strata$plot.stratum <- paste0(plot.strata$plotCode, "-", plot.strata$stratum)

    # Add plot metadata to facilitate join to whitelist/blacklist (see below)
    plot.meta <- unique(dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")])
    plot.strata <- merge(plot.strata, plot.meta, by = "plotCode", all.x = TRUE)

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
    # Add missing strata to each plot
    # These get value of cover=0
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

    dat <- dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "cover")]

    if (nrow(plot.strata > 0)) {
      # Rename, add zero cover column and reorder to match dat
      dat.missing <- plot.strata
      dat.missing$cover <- 0
      dat.missing <- dat.missing[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "cover")]

      # Add missing records to main data frame
      dat <- rbind(dat, dat.missing)
      cat("done\n")
    } else {
      cat("all strata already present...done\n")
    }

    ########################################################
    # Validations and transformations
    ########################################################

    cat("- Completing transformations...")

    # get all unique plots across entire data set
    plots <- unique(dat$plotCode)

    # Check plotCodes are unique across both benchmark and focal data.
    # If not, stop and edit plot codes in original data matrix to ensure uniqueness
    allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])
    if (length(plots) != nrow(allPlots)) stop("Plot codes not unique!")

    dat <- dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "cover")]
    colnames(dat) <- c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI")
    dat$EI.tr <- NA

    # Load the transformations file
    source(paste(INCLUDESDIR, "transformations.R", sep = ""), local = TRUE)

    cat("done\n")

    cat("- Saving prepared raw data...")
    # Save transformed raw data
    rawFile <- paste(RESULTSDIR, rawFileName, sep = "")
    write.csv(dat, file = rawFile, row.names = FALSE)
    cat("done\n")
  } # END prepare.raw

  #######################################################
  # Calculate and save benchmark values
  #######################################################

  # Re-import prepared raw data
  inputFile <- paste(RESULTSDIR, rawFileName, sep = "")
  cat("inputFile=", inputFile)
  dat <- read.csv(inputFile, header = T)

  # Make data frame of moments for transformed cover
  # aggregated by vegclass + stratum
  EI.bm <- subset(dat, dat$focalOrBenchmark == "b")

  EI.bm <- with(EI.bm, aggregate(EI.tr, list(vegClass = vegClass, stratum = stratum),
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
  colnames(EI.bm) <- c("vegClass", "stratum", "n", "EI.mean", "EI.med", "EI.sd", "EI.max", "EI.min")

  # Add EI & rearrange the columns
  EI.bm$EI <- curr.EI # add indicator
  EI.bm <- EI.bm[, c("EI", "vegClass", "stratum", "n", "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min")]

  # Make final, human-readable veg classes
  EI.bm$vegClass <- human.readable(EI.bm$vegClass)

  # Inspect the columns of EI.bm
  sorted.EI.bm <- EI.bm[order(EI.bm$vegClass, EI.bm$stratum), ]
  sorted.EI.bm[1:12, ]

  # export benchmark site file
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
  cat("************************\n")
  cat("Generating figures\n")
  cat("************************\n")
} # END PLOT.FIGS.ONLY

####################################################
# Plot overlap
####################################################

if (plot.overlap == TRUE) source("includes/overlap.R")

####################################################
# Graph the fitted distributions for each vegetation type
# Include model statistic and overlap scores on figures if requested
####################################################

source("includes/call.graph.dists.R")

#######################################################
# End of script
#######################################################

cat("\n##################################################\n")
cat("Operation completed\n")
cat("##################################################\n\n")


# Closing bullshit brace to make stop() work
# Uncomment to enable use of stop() for debugging
# If activating, be sure to uncomment opening brace at start of
# script as well
# }

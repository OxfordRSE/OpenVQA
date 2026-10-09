# Opening bullshit brace to make stop() work.
# Uncomment to enable use of stop() for debugging.
# If activating, be sure to uncomment last line of this script as well.
# {

#######################################################
# Quality Hectares Ecological Indicator (EI) Calculation
# Percent exotic speciesm (PCES)
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
# 16 May 2025
#
# Purpose:
# Calculate *total* percent cover of exotic species within the entire plot
#   and determine quality relative to benchmark. Recommend fixed
#   benchmark==0.
#
# Requires input file of species cover by stratum, with exotic species indicated.
# 		plotCode: 						unique alphanumeric code for plot
# 		focalOrBenchmark: 	  'b' for benchmark, 'f' for focal site
# 		landCover: 						bland cover class name or code
# 		vegClass: 						benchmark vegetation name or code
# 	stratum: 							size class (stratum)
# 	species: 							size class (stratum)
# 	is_exotic: 						species native status (0=native, 1=introduced)
# 	cover:                proportional cover of species in stratum
#
# Notes:
# 1. Final value of cover per plot is calculated in 5 steps:
#     (1) Remove cover values for all native species
#     (2) Sum remaining cover values within each stratum in each plot,
#         ignoring species
#     (3) Truncate stratum cover at 1 (i.e., maintain domain over [0:1])
#     (4) Aggregate cover at plot level, using MAX of the stratum covers.
#     (5) Add back any missing plots, setting exoticCover=0
# 2. Percentages in original field "cover" MUST be proportions!
# 3. Input data has strata, but indicator does not
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

curr.EI <- "PCES" # Use short code

##############################
# Load attributes for this indicator
# (ei.params set in params file)
##############################

# Import parameters set in params file
ei.pars <- ei.params(curr.EI)

# Extract the indicator values
ei.name <- ei.pars$ei.name # Full name of this EI, for graphics
EI.name <- ei.name # Temporary, for backwards-compatibility
inputFileName <- ei.pars$source.file # Name of input file ('coverByGrowthForm.csv')
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
leg.x.extra <- -0.2
leg.y.extra <- NA

# Grouped figure legend additional offset NOT YET IMPLEMENTED!
# Positive: move to right, negative: move to left
# Comment out or set to "" or NA to use defaults
leg.x.extra.g <- -0.2
leg.y.extra.g <- NA

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
  cat("########################\n")
  cat("Preparing data\n")
  cat("########################\n\n")

  if (prepare.raw == TRUE || force.prepare.raw == TRUE) { # START prepare.raw

    #######################################################
    # Load raw data
    #######################################################

    cat("Importing raw data:\n")

    cat("- Plot data...")
    plotFile <- paste(INPUTDIR, PLOTMETADATA.FILE, sep = "")
    dat.plot <- read.csv(plotFile, header = T)
    dat.plot$plotCode <- as.character(dat.plot$plotCode)
    cat("done\n")

    cat("- Species cover by stratum data...")
    # Import table containing cover values of all species each plot,
    # plus vegetation class by stratum (growth form)
    inputFile <- paste(INPUTDIR, inputFileName, sep = "")
    dat.raw <- read.csv(inputFile, header = T)
    dat <- dat.raw
    cat("done\n")

    ################################
    # Remove rows with missing cover
    # & issue warning
    ################################

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
    # Aggregate exotic species cover to plot level, using the
    # maximum cover among all strata
    ##########################################################

    cat("Calculating total exotic cover per plot:\n")

    cat("- Aggregating cover by plot using max function...")
    df.plotCover <- aggregate(
      cover ~ plotCode,
      data = dat,
      FUN = sum,
      na.rm = TRUE,
      na.pass = NULL
    )
    cat("done\n")

    cat("- Merging into new data frame 'dat'...")
    df.plotCover <- merge(df.plotCover, dat[, c(
      "plotCode", "focalOrBenchmark", "landCover", "vegClass"
    )])
    df.plotCover <- df.reorder(df.plotCover, col.move = "cover", move.last = TRUE)
    dat.bak <- dat
    dat <- df.plotCover
    cat("done\n")

    cat("- Renaming 'cover' to generic 'EI'...")
    names(dat)[names(dat) == "cover"] <- "EI"
    cat("done\n")

    #######################################################
    # Add missing plots
    #######################################################

    if (INCLUDE.PLOTS.NODATA) {
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
        # Rename, add zero cover column and reorder to match dat
        plots.missing$EI <- 0
        plots.missing <- plots.missing[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "EI")]

        # Add missing records to main data frame & reorder
        dat <- rbind(dat, plots.missing)
        dat <- dat[order(dat$vegClass, dat$landCover, dat$focalOrBenchmark, dat$plotCode), ]
        rownames(dat) <- NULL # Reset row numbers
        cat(n.missing, " plots added\n")
      } else {
        cat("all plots already present\n")
      }
    }

    ################################
    # Add pseudo-stratum column
    ################################

    cat("Adding pseudo-stratum...")

    # Get column names
    cols <- colnames(dat)

    # Reorder columns, keeping stratum column if present,
    # and keeping the transformed EI column
    if (!any("stratum" %in% cols)) {
      has.stratum <- FALSE
      dat$stratum <- "nostrata"
    }

    colnames(dat) <- c(
      "plotCode", "focalOrBenchmark", "landCover",
      "vegClass", "EI", "stratum"
    )
    dat <- dat[, c(
      "plotCode", "focalOrBenchmark", "landCover",
      "vegClass", "stratum", "EI"
    )]

    cat("done\n")

    ########################################################
    # Validations and transformations
    ########################################################

    cat("Completing final transformations...")
    # None needed; just add column EI.tr, duplicating values in EI
    dat$EI.tr <- dat$EI
    cat("done\n")

    cat("Saving prepared raw data...")
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
  dat <- dat

  # Make data frame of moments for transformed cover
  # aggregated by vegclass + stratum
  EI.bm <- dat[dat$focalOrBenchmark == "b", ]

  EI.bm <- with(EI.bm, aggregate(EI.tr, list(vegClass = vegClass),
    FUN = function(x) {
      c(
        n = length(x), # This is incorrect, but we'll fix it later
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

  # Fix n
  EI.bm.plots <- unique(dat[dat$focalOrBenchmark == "b", c("plotCode", "vegClass")])
  EI.bm.n <- aggregate(
    plotCode ~ vegClass,
    data = EI.bm.plots,
    FUN = length
  )
  colnames(EI.bm.n) <- c("vegClass", "n.b")
  EI.bm <- merge(EI.bm, EI.bm.n, by = "vegClass")
  names(EI.bm)[names(EI.bm) == "n"] <- "n.bad" # Save temporarily in case want to compare
  names(EI.bm)[names(EI.bm) == "n.b"] <- "n" # Save temporarily in case want to compare
  EI.bm <- EI.bm[, !names(EI.bm) %in% c("n.bad")]

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

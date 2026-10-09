# Opening bullshit brace to make stop() work.
# Uncomment to enable use of stop() for debugging.
# If activating, be sure to uncomment last line of this script as well.
# {

#######################################################
# Quality Hectares Ecological Condition Metric (EI) Calculation
# Species Richness (SR)
#
# **** LATEST VERSION. USES FILE: speciesCover.csv ****
# **** Renamed to sr.R from sr2.R ****
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
#
# Purpose:
# Calculates species richness quality scores for
# one or more focal sites, relative to sample of benchmark sites
# from same vegetation class. Relative scores are calculated
# for entire plot and the average used to discount the final site EI score.
#
#
# Inputs. Two tab-delimited files. Files may contain other columns in addition to
# 		those listed, but only listed columns are required for this script. Set file names
# 		in parameters, below.
#
# 1. speciesCover.csv: species cover file:
# 		plotCode: 		unique alphanumeric code for plot
# 		species: 			species name
# 		is_exotic: 		0: native species; 1: exotic species
# 		cover: 				proportional cover of species in plotnumber species per plot
#
# 2. plotMetadata.csv: plot metadata file:
# 		plotCode: 					unique alphanumeric code for plot
# 		focalOrBenchmark: 	'f': focal plot; 'b': benchmark plot
# 		landCover: 			land cover code of land cover unit in which plot is found.
# 											Will be renamed to vegClass (legacy issue)
#######################################################

############################################
# Load functions if not already loaded.
############################################

if (!exists("functions.loaded", mode = "function")) source(paste0(SRCDIR, "includes/functions.R"))

#######################################################
# Parameters
#######################################################

##############################
# Ecological indicator (EI) to calculate
##############################

# EI code
curr.EI <- "SR"

##############################
# Input file(s) for this indicator
##############################

# Load attributes for this indicator (set in params file)
ei.pars <- ei.params(curr.EI)
ei.name <- ei.pars$ei.name # Full name of this EI, for graphics
EI.name <- ei.name # Temporary, for backwards-compatibility
source.file <- ei.pars$source.file # Name of input file ('speciesCover.csv' or 'speciesStems.csv')
prepare.raw <- ei.pars$prepare.raw # Prepare raw data from scratch?
ei.data.type <- ei.pars$ei.data.type # Data type for this EI (cover|ind)
distn <- ei.pars$distn # Distribution of the EI
q.method <- ei.pars$q.method # Quality method: "fixed" (fixed bm) | "empirical" (overlap)
bm.val <- ei.pars$bm.val # Fixed bm value. Applies only if q.method="fixed"
test.tail <- ei.pars$test.tail # Test tail for this EI
has.strata <- ei.pars$has.stratum # True if this EI has strata, otherwise F
has.stratum <- has.strata # Hack for backward compatibility
if (has.strata == F) curr.stratum <- ""
exclude.exotics <- ei.pars$exclude.exotics

##############################
# Indicator-specific graphing parameters
##############################

# *** PROBABLY NOT NEEDED ***
# group.strata <- FALSE	# Set to true to group figures for different strata of
# 											# same vegetation in single figure

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

# TRUE if data are proportions or percent, otherwise false
# If NA set to FALSE
proportions <- FALSE

#######################################################
# Main
#######################################################

cat("\n##################################################\n")
cat("Calculating Quality for indicator SR (Species Richness)\n")
cat("##################################################\n\n")

if (PLOT.FIGS.ONLY == FALSE) {
  cat("************************\n")
  cat("Preparing data\n")
  cat("************************\n")

  if (prepare.raw == TRUE || force.prepare.raw == TRUE) { # START prepare.raw

    #######################################################
    # Load raw data
    #######################################################

    cat("Loading input files:\n")

    # Plot metadata
    cat("- Loading plot metadata from file '", PLOTMETADATA.FILE, "'...", sep = "")
    plotFile <- paste(INPUTDIR, PLOTMETADATA.FILE, sep = "")
    plot.dat <- read.csv(plotFile, header = T)
    cat("done\n")

    # Import table containing cover values of all species each plot,
    # plus vegetation class by stratum (growth form)
    cat("- Loading species occurrence data from file '", source.file, "'...", sep = "")
    species.obs.file <- paste(INPUTDIR, source.file, sep = "")
    spp.dat <- read.csv(species.obs.file, header = T)
    cat("done\n")

    cat("- Loading species attributes from file '", SPECIES.FILE, "'...", sep = "")
    species.attr.file <- paste(INPUTDIR, SPECIES.FILE, sep = "")
    df.species <- read.csv(species.attr.file, header = T)
    names(df.species)[names(df.species) == "isExotic"] <- "is_exotic" # rename just in case
    cat("done\n")

    cat("- Merging species attributes into species occurrence data...", sep = "")
    spp.dat <- merge(spp.dat, df.species[, c("species", "is_exotic")], by = "species", all.x = TRUE)
    cat("done\n")

    cat("- Checking for missing species native status:\n", sep = "")
    if (IS.EXOTIC.MISSING.ASSUME.NATIVE == TRUE) {
      cat("setting unknown native status to 'is_exotic=0'...", sep = "")
      # Will also mark "indet_sp." as native, but not a problem
      spp.dat$is_exotic[is.na(spp.dat$is_exotic)] <- 0
      cat("done\n")
    } else {
      # Quit and warn about missing values
      stop_quietly("ERROR: value of 'is_exotic' missing for one or more species. Please correct.\n")
    }

    ################################
    # Remove zero-cover species
    ################################

    if (ei.data.type == "cover") {
      # Exclude species where cover == 0
      # Applies to cover data only
      spp.dat <- spp.dat[spp.dat$cover > 0, ]
    }

    ################################
    # Remove exotic species from raw data
    ################################

    cat("Removing exotic species...")

    # Remove exotics
    spp.dat <- spp.dat[!spp.dat$is_exotic == 1, ]
    cat("done\n")

    cat("Removing superfluous fields from spp.dat...")
    spp.dat <- spp.dat[, c("plotCode", "species")]
    cat("done\n")

    ################################
    # Add plot metadata
    ################################

    cat("Adding plot metadata...")
    # Get matching records & extract just the plot metadata.
    # This approach ensures that all plots end up in final matrix,
    # even those with zero native species
    plots <- merge(
      plot.dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")],
      spp.dat,
      by = "plotCode",
      all.x = TRUE
    )
    plots <- unique(
      plots[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")]
    )
    cat("done\n")

    ################################
    # Calculate native species richness
    ################################

    cat("Calculating native species richness per plot...")

    # Exclude vegetation with no data
    # Parameter veg.nodata set in params.R
    plots <- plots[!plots$landCover %in% veg.nodata &
      !plots$vegClass %in% veg.nodata, ]

    # Count species per plot
    # plot.spp <- unique(spp.dat[spp.dat$is_exotic==0, c('plotCode', 'species') ])
    plot.spp <- unique(spp.dat[, c("plotCode", "species")])
    richness <- as.data.frame(table(plot.spp$plotCode))
    colnames(richness) <- c("plotCode", "speciesRichness")

    # Merge using left join to preserve total number of plots
    # & set richness=0 for plots with speciesRichness==NA
    dat <- merge(plots, richness, by = "plotCode", all.x = TRUE)
    dat$speciesRichness[is.na(dat$speciesRichness)] <- 0
    dat$speciesRichness <- as.integer(dat$speciesRichness)
    cat("done\n")

    ################################
    # Validations and transformations
    ################################

    cat("Performing validations and transformations...")
    # Check plotCodes are unique across both benchmark and focal data.
    # If not, stop and edit plot codes in original data matrix to ensure uniqueness
    plots <- unique(dat$plotCode)
    allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])
    if (length(plots) != nrow(allPlots)) stop("Plot codes not unique!")
    cat("done\n")

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

    ################################
    # Make final adjustments
    ################################

    cat("Making final changes...")

    # Set zeros to one, if requested (NBin distns only)
    if (zero_to_one == TRUE) dat$EI[dat$EI == 0] <- 1
    dat$EI.tr <- dat$EI

    cat("done\n")

    ################################
    # Save prepared raw data
    ################################

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
  cat("************************\n")
  cat("Generating figures\n")
  cat("************************\n")
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

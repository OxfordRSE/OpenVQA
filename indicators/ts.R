# Opening bullshit brace to make stop() work.
# Uncomment to enable use of stop() for debugging.
# If activating, be sure to uncomment last line of this script as well.
# {

#######################################################
# Taxonomic Similarity
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
#
# Purpose:
# Prepares indicator values and generates quality scores and figures for
# indicator Taxonomic Similarity (TS). TS is a non-multivariate alternative
# to NMDS-based Taxonomic Distance (TD). The default method for TS is
# the Sorensen Index, which calculated proportional taxonomic overlap
# between 2 sites based on presence data only. This method is a safe
# alternative to TD when biased cover methods such as Braun Blanquet
# preclude the use of abundance-weighted cover data.
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
curr.EI <- "TS"

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

# Group strata in single figure?
# Set to true to make extra set of figures grouping
# figures for different strata of same vegetation in
# single figure. MUST be FALSE for indicators without
# strata
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
proportions <- TRUE

#######################################################
# Main
#######################################################

cat("\n##################################################\n")
cat("Calculating Quality for indicator TS (Taxonomic Similarity)\n")
cat("##################################################\n\n")

if (PLOT.FIGS.ONLY == FALSE) {
  cat("************************\n")
  cat("Preparing data\n")
  cat("************************\n")

  if (prepare.raw == TRUE || force.prepare.raw == TRUE) { # START prepare.raw

    cat("Loading input files:\n")

    #######################################################
    # Load raw data
    #######################################################

    cat("Loading raw data...")

    # Get plot metadata
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

    cat("- Checking for missing species native status...", sep = "")
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
    if (exists("veg.nodata")) {
      # Exclude vegetation with no data
      # Parameter veg.nodata set in params.R
      plots <- plots[!plots$landCover %in% veg.nodata &
        !plots$vegClass %in% veg.nodata, ]
    }
    cat("done\n")

    ################################
    # Calculate pairwise species overlap
    ################################

    cat("Calculating pairwise species overlap for benchmark plots...")
    vegClasses <- unique(plots$vegClass)

    # Toggle through each vegClass
    for (vegClass_ind in 1:length(vegClasses)) { # START vegClass_ind loop
      vegClass <- vegClasses[vegClass_ind]

      # Prepare empty data frame of benchmark-benchmark plot pairs
      bm_plots <- unique(plots[
        plots$vegClass == vegClass & plots$focalOrBenchmark == "b",
        c("plotCode"),
        drop = FALSE
      ])
      pp_bm_plots <- merge(bm_plots, bm_plots, by = NULL) # Unique pairings of different plots
      colnames(pp_bm_plots) <- c("plot2", "plot1")
      pp_bm_plots <- pp_bm_plots[pp_bm_plots$plot1 < pp_bm_plots$plot2, ] # Unique, different pairs only
      pp_bm_plots <- pp_bm_plots[, c("plot1", "plot2")]
      pp_bm_plots$plotComb <- paste0(pp_bm_plots$plot1, "_", pp_bm_plots$plot2)
      pp_bm_plots$focalOrBenchmark <- "b"
      pp_bm_plots$landCover <- vegClass
      pp_bm_plots$vegClass <- vegClass
      pp_bm_plots$stratum <- "nostrata"
      pp_bm_plots$EI <- as.numeric(NA) # Generic "EI" column to hold TS values

      for (pp_ind in 1:nrow(pp_bm_plots)) {
        plot1 <- pp_bm_plots$plot1[pp_ind]
        plot1_spp <- unique(spp.dat[spp.dat$plotCode == plot1, c("species")])
        plot2 <- pp_bm_plots$plot2[pp_ind]
        plot2_spp <- unique(spp.dat[spp.dat$plotCode == plot2, c("species")])
        ts <- sorensen(plot1_spp, plot2_spp)
        pp_bm_plots$EI[pp_bm_plots$plot1 == plot1 & pp_bm_plots$plot2 == plot2] <- ts
      }

      # Create or append results to final df
      if (vegClass_ind == 1) {
        dat_b <- pp_bm_plots
      } else {
        dat_b <- rbind(dat_b, pp_bm_plots)
      }
    } # END vegClass_ind loop
    cat("done\n")

    cat("Calculating pairwise species overlap between focal and benchmark plots...")
    landCoverClasses <- unique(plots$landCover[plots$focalOrBenchmark == "f"])

    # Toggle through each land cover class
    for (landCover_ind in 1:length(landCoverClasses)) { # START landCover_ind loop
      landCover <- landCoverClasses[landCover_ind]

      # Get focal plots in this land cover class
      f_plots <- unique(plots[
        plots$landCover == landCover & plots$focalOrBenchmark == "f",
        c("plotCode"),
        drop = FALSE
      ])

      # Get benchmark plots for the vegClass of the focal plots
      vegClass <- unique(plots$vegClass[plots$landCover == landCover])
      if (length(vegClass) > 1) {
        msg <- paste0("ERROR: multiple vegClasses found for landCover '", landCover, "'!")
        stop_quietly(msg)
      }
      bm_plots <- unique(plots[
        plots$focalOrBenchmark == "b" & plots$vegClass == vegClass,
        c("plotCode"),
        drop = FALSE
      ])

      # Prepare empty data frame of benchmark-benchmark plot pairs
      pp_f_plots <- merge(f_plots, bm_plots, by = NULL) # Unique pairings of different plots
      colnames(pp_f_plots) <- c("plot1", "plot2") # Remember that plot1 is focal
      pp_f_plots <- pp_f_plots[order(pp_f_plots$plot1, pp_f_plots$plot1), ]
      pp_f_plots <- unique(pp_f_plots) # Unique pairs only (probably not necessary in this case)
      pp_f_plots$plotComb <- paste0(pp_f_plots$plot1, "_", pp_f_plots$plot2)
      pp_f_plots$focalOrBenchmark <- "f"
      pp_f_plots$landCover <- landCover
      pp_f_plots$vegClass <- vegClass
      pp_f_plots$stratum <- "nostrata"
      pp_f_plots$EI <- as.numeric(NA)

      for (pp_ind in 1:nrow(pp_f_plots)) {
        plot1 <- pp_f_plots$plot1[pp_ind]
        plot1_spp <- unique(spp.dat[spp.dat$plotCode == plot1, c("species")])
        plot2 <- pp_f_plots$plot2[pp_ind]
        plot2_spp <- unique(spp.dat[spp.dat$plotCode == plot2, c("species")])
        ts <- sorensen(plot1_spp, plot2_spp)
        pp_f_plots$EI[pp_f_plots$plot1 == plot1 & pp_f_plots$plot2 == plot2] <- ts
      }

      # Create or append results to final df
      if (landCover_ind == 1) {
        dat_f <- pp_f_plots
      } else {
        dat_f <- rbind(dat_f, pp_f_plots)
      }
    } # END vegClass_indloop
    cat("done\n")

    cat("Combing focal and benchmark plots TS values into single data frame...")
    dat <- rbind(dat_f, dat_b)
    rm(dat_f, dat_b)
    cat("done\n")

    cat("Setting NAs to zero, if any...")
    dat$EI[is.na(dat$EI)] <- 0
    cat("done\n")

    cat("Formatting final raw indicator data frame...")
    dat <- df.reorder(dat, col.move = "plotComb", move.first = TRUE)
    names(dat)[names(dat) == "plotComb"] <- "plotCode" # Misleading but name is standard
    dat$EI.tr <- dat$EI # Need the column, but no transformations
    dat <- dat[order(
      dat$vegClass, dat$focalOrBenchmark,
      dat$landCover, dat$plotCode
    ), ]
    cat("done\n")

    ################################
    # Validations and transformations
    ################################

    # None needed for this indicator

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

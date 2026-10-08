# Opening bullshit brace to make stop() work.
# Uncomment to enable use of stop() for debugging.
# Also uncomment last line of this script as well.
# {

#######################################################
# Quality Hectares Ecological Indicator (EI) Calculation
# EI = Taxonomic Distance (TD)
# Method = NMDS
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
#
# Was td3.R
# This version includes automated detection and removal of TD outliers.
#
# Purpose:
# Calculates "Taxonomic Distance" ECM score
# for one or more focal sites, relative to sample of benchmark (bm) sites
#
# Important note:
# This version updated to handle data with >1 land cover class
# (landCover) per class of benchmark (bm) vegetation (vegClass). To work
# as intended, bm plots must be replicated for each land cover
# class and the value of vegClass for each bm plot set equal to the
# value of landCover for the land cover class. Replication and re-
# classification of the bm plots should be handled by the import script.
# For example, see "Hack #1" in script "import.teck-ev-demo.R"
#
# Details:
# Taxonomic distance is the standardized Euclidean distance from a
# given plot to the multivariate centroid in NMDS space. Centroid
# calculated for benchmark plots only. Distances for focal sites
# calculated individually, separately for from benchmark sites.
# ECM is based on the focal site TD score, relative to the distribution
# of TD scores for bm sites.
#
# Requires tab-delimited file of raw EI values:
# 		plotCode: 							unique alphanumeric code for plot
# 		focalOrBenchmark: 		'b' for benchmark, 'f' for focal site
# 		vegClass: 						short code for benchmark vegetation class
# 		landCover: 						short code for land cover class
# 		species: 							species name, excluding morphospecies
# 		cover: 								Maximum percent cover of the species across all
# 													stratum
#######################################################

############################################
# Load functions if not already loaded.
############################################

if (!exists("functions.loaded", mode = "function")) source(paste0(SRCDIR, "includes/functions.R"))

#######################################################
# Parameters
#######################################################

# Ecological indicator (EI) code
# CRITICAL! Determines which EI parameters are loaded
# and behavior of many included functions and files
curr.EI <- "TD"

# Make sure packages specific to this EI are loaded
# Should be in params, but double-check here just in case
require(vegan) # NMDS
require(picante) # Used to convert input file to vegan format

##############################
# Input file for this indicator
##############################

# Get attributes for this indicator (set in params file)
ei.pars <- ei.params(curr.EI)
source.file <- ei.pars$source.file
ei.name <- ei.pars$ei.name # Full name of this EI, for display/graphics
EI.name <- ei.name # Synonym for backwards-compatibility
source.file <- ei.pars$source.file # Name of input file ('speciesCover.csv' or 'speciesStems.csv')
prepare.raw <- ei.pars$prepare.raw # Prepare raw data from scratch?
ei.data.type <- ei.pars$ei.data.type # Data type for this EI (cover|ind)
distn <- ei.pars$distn # Distribution of the EI
q.method <- ei.pars$q.method # "fixed" (fixed bm) | "empirical" (overlap)
bm.val <- ei.pars$bm.val # Fixed bm value. Applies only if q.method="fixed"
test.tail <- ei.pars$test.tail # Test tail for this EI
convert.percent <- ei.pars$convert.percent # Convert % cover to proportion
remove.zero.cover.plots <- ei.pars$remove.zero.cover.plots
has.strata <- ei.pars$has.stratum # True if this EI has strata, otherwise F
has.stratum <- has.strata # Hack for backward compatibility
if (has.strata == F) curr.stratum <- ""
td.multiplier <- ei.pars$td.multiplier # Applies scalar multiplier to improve distn fit
td.multiplier.omit <- ei.pars$td.multiplier.omit
scale.abund <- ei.pars$scale.abund
logit <- ei.pars$logit

#################################
# Transformation parameters
#
# Set automatically based on value
# of ei.data.type. Generally should
# not be set manually.
# Passed to transformations.R.
#################################

# TRUE if data are proportions or percent, otherwise false
# In theory no longer needed, replaced by convert.percent
if (ei.data.type == "cover") {
  proportions <- TRUE
} else {
  proportions <- FALSE
}

# Set to TRUE to set any proportions > 1 to 1
# Passed transformations.R
# Truncation at one is a last resort; generally
# better to scale values over [0:1] by setting
# scale.abund<-TRUE (set in ei.params; see above)
# truncate.at.one has no effect if scale.abund==TRUE.
if (proportions == TRUE) {
  truncate.at.one <- TRUE
} else {
  truncate.at.one <- FALSE
}

##############################
# Indicator-specific graphing parameters
##############################

# Group strata in single figure?
# Set to TRUE to make extra set of figures grouping
# figures for different strata of same vegetation in
# single figure. MUST be FALSE if no strata for this
# indicator
allow.group.strata <- FALSE

# Minimum value for y axis
# Leave as empty string ("") to allow to vary automatically
y.axis.fixed.min <- 0

##############################
# Names and locations of output files
##############################
rawFileName <- paste(curr.EI, "_raw.csv", sep = "") # Complete raw data with transformations
resultsFocal <- paste(curr.EI, "_focal.csv", sep = "")
resultsBm <- paste(curr.EI, "_benchmark.csv", sep = "")

#######################################################
# Main
#######################################################

cat("\n##################################################\n")
cat("Calculating Quality for indicator TD (Taxonomic Distance)\n")
cat("##################################################\n\n")

# Set name and path+name of backup raw data file
# and path+name of raw data file (name set above at start)
# Defining here as need both inside and outside of prepare.raw block
rawFileName.bak <- str_replace(rawFileName, ".csv", ".bak.csv")
rawFile.bak <- paste0(RESULTSDIR, rawFileName.bak)
rawFile <- paste0(RESULTSDIR, rawFileName)

if (PLOT.FIGS.ONLY == FALSE) {
  cat("************************\n")
  cat("Preparing data\n")
  cat("************************\n")

  if (prepare.raw == TRUE || force.prepare.raw == TRUE) { # START prepare.raw

    #######################################################
    # Load data
    #######################################################

    cat("Loading input files:\n")

    # Import table containing cover values of all species in
    # each plot by stratum (growth form)
    # Table must also include landCover, vegClass (=bm vegetation)
    # and focalOrBenchmark (f|b) of each plot
    cat("- Loading species occurrence data from file '", source.file, "'...", sep = "")
    inputFile <- paste(INPUTDIR, source.file, sep = "")
    dat.raw <- read.csv(inputFile, header = T)
    # detachAllData()
    # attach(dat)
    dat <- dat.raw
    cat("done\n")

    # Load plot metadata
    # Used after NMDS for adding no-data plots and outliers to results files
    cat("- Loading plot metadata from file '", PLOTMETADATA.FILE, "'...", sep = "")
    plotFile <- paste(INPUTDIR, PLOTMETADATA.FILE, sep = "")
    plotMetadata <- read.csv(plotFile, header = T)
    cat("done\n")

    #######################################################
    # Prepare data
    #######################################################

    # For individuals/stem data only, convert to
    # counts of individuals per species per plot.
    # Require individual identifier column
    # Groups on individual id
    if (ei.data.type == "ind") {
      cat("Calculating individuals per species per plot...")
      dat$species.ns <- dat$species
      dat$species.ns <- gsub(" ", "_", dat$species.ns)
      dat$plot.species <- paste0(dat$plotCode, "_", dat$species.ns)
      plot.ind <- unique(dat[, c("plotCode", "plot.species", "species", "ind_id")])
      plot.spp.abund <- aggregate(
        ind_id ~ plot.species,
        data = plot.ind,
        FUN = length
      )
      colnames(plot.spp.abund) <- c("plot.species", "abund")
      plot.spp <- unique(dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "plot.species", "species")])
      plot.spp.abund <- merge(plot.spp.abund, plot.spp, by = "plot.species", all.x = TRUE)
      plot.spp.abund <- plot.spp.abund[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "species", "abund")]
      dat <- plot.spp.abund
      cat("done\n")
    }

    # **************************
    # WARNING: Need to add in plots with NO individuals
    # Perhaps add single individual of a non-existent species name?
    # **************************

    # Remove rows with missing data or zero cover
    # Appplies to cover data only
    if (ei.data.type == "cover") {
      cat("Removing rows with zero or missing cover...")
      n.before <- nrow(dat)

      # Save backups for troubleshooting
      dat.cover.na <- dat[is.na(dat$cover), ]
      dat.cover.lt.zero <- dat[dat$cover <= 0, ]

      dat <- dat[!is.na(dat$cover), ]
      dat <- dat[dat$cover > 0, ]
      n.after <- nrow(dat)

      n.diff <- n.before - n.after
      if (n.diff > 0) {
        cat("WARNING: ", n.diff, " of ", n.before, " rows deleted due to missing or zero cover!\n")
      }
      cat("done\n")
    }

    # Exclude vegetation with no data
    # (veg.nodata set in params.R)
    dat <- dat[
      !dat$landCover %in% veg.nodata &
        !dat$vegClass %in% veg.nodata,
    ]

    # Exclude land cover classes with no benchmark vegetation
    dat.f <- dat[dat$focalOrBenchmark == "f", ]
    dat.b <- dat[dat$focalOrBenchmark == "b", ]
    bm.veg.f <- unique(dat.f$vegClass)
    dat.b <- dat.b[dat.b$vegClass %in% bm.veg.f, ]
    dat <- rbind(dat.f, dat.b)

    if (remove.zero.cover.plots == TRUE && ei.data.type == "cover") {
      # Get sum of cover for each plot
      # Options 'na.rm = TRUE, na.action=na.pass' cause
      # NAs to be treated as zeros
      dat$plot <- as.character(dat$plotCode) # convert factor to character
      plot.tot.cover <- aggregate(
        cover ~ plot,
        data = dat,
        FUN = sum,
        na.rm = TRUE,
        na.action = na.pass
      )
      plots.no.cover <- plot.tot.cover[plot.tot.cover$cover == 0, c("plot")]
      n.plots.no.cover <- length(plots.no.cover)
      n.plots.before <- length(unique(dat$plot))
      dat <- dat[!dat$plot %in% plots.no.cover, ]
      n.plots.after <- length(unique(dat$plot))
      plots.deleted <- n.plots.before - n.plots.after
      perc.deleted <- specify_decimal(plots.deleted / n.plots.before * 100, 1)

      if (plots.deleted > 0) {
        msg <- paste0(
          "WARNING: ", plots.deleted, " plots (", perc.deleted,
          "%) removed due to all-zero cover!\n"
        )
        cat(msg)

        if (plots.deleted == n.plots.before) {
          msg <- "ERROR: No plots with no-zero cover; aborting...\n\n"
          stop(msg)
        }
      }
    }

    #######################################################
    # Remove outlier plots if applicable
    # this only applies if td.R has already been run once
    # before and file td_nmds_outliers.csv exists in the
    # results directory
    #######################################################

    f.outliers <- paste0(RESULTSDIR, F.TD.OUTLIERS)

    if (file.exists(f.outliers) && REMOVE.TD.OUTLIERS == TRUE) {
      cat("\nChecking for previously-saved NMDS outliers:\n")
      df.outliers <- read.csv(f.outliers, header = TRUE)
      outlier.plots <- unique(df.outliers$plotCode)
      n.outliers <- length(outlier.plots)
      cat(paste0("- Found ", n.outliers, " outlier plots in file '", F.TD.OUTLIERS, "'\n"))

      # Delete the offending plots
      cat(paste0("- Deleting ", n.outliers, " NMDS outliers..."))
      n.plots.before <- length(unique(dat$plotCode))
      dat <- dat[!dat$plotCode %in% outlier.plots, ]
      cat("done\n")

      n.plots.after <- length(unique(dat$plotCode))
      cat("- Plots before: ", n.plots.before, ", plots after: ", n.plots.after, "\n")
    }

    #######################################################
    # Validations and transformations
    #######################################################

    # Rename cover to "abund", if applicable
    names(dat)[names(dat) == "cover"] <- "abund"

    # Keep only fields needed
    dat <- dat[, c(
      "plotCode", "focalOrBenchmark", "landCover",
      "vegClass", "species", "abund"
    )]

    # Rename percent cover field to EI and add EI-transformed column
    colnames(dat) <- c(
      "plotCode", "focalOrBenchmark", "landCover",
      "vegClass", "species", "EI"
    )
    names(dat)[names(dat) == "abund"] <- "EI"
    dat$EI.tr <- NA

    # Temporary fix for now until I sort out which is being used!
    dat$EI.tr <- dat$EI

    # Load transformations script (transforms field "EI" in df "dat")
    source(paste(INCLUDESDIR, "transformations.R", sep = ""), local = TRUE)

    # Remove species with effectively zero cover after transformation
    dat <- dat[dat$EI.tr > 0.000000001, ]

    #######################################################
    # Calculate taxonomic distances by land cover class
    #######################################################

    cat("\n*************************\n")
    cat("Performing NMDS\n")
    cat("*************************\n\n")

    # Start data frame to hold raw NMDS results

    # Remove any NA land cover or bm veg classes
    dat <- dat[!is.na(dat$landCover) & !is.na(dat$vegClass), ]

    # Set up loop to extract all plots from a given vegetation type
    landCoverClasses <- unique(dat[dat$focalOrBenchmark == "f", c("landCover")]) # Focal land cover classes only
    tot.landCoverClasses <- length(landCoverClasses)
    landCoverVegClasses <- unique(dat[dat$focalOrBenchmark == "f", c("landCover", "vegClass")]) # bm veg for each land cover
    dat.f <- dat[dat$focalOrBenchmark == "f", ] # df of just focal plots
    dat.b <- dat[dat$focalOrBenchmark == "b", ] # df of just bm plots
    tot.converged <- 0
    tot.converged.high <- 0 # Num. of results with iterations>=1; solution found but may be unstable

    for (i in 1:tot.landCoverClasses) { # BEGIN NMDS landCover loop
      # Get each unique landCover x stratum combination in bm table
      curr.landCover <- landCoverClasses[i]
      curr.vegClass <- landCoverVegClasses[landCoverVegClasses$landCover == curr.landCover, c("vegClass")]
      cat("landCover='", as.character(curr.landCover), "'\n", sep = "")
      cat("bmVeg='", as.character(curr.vegClass), "'\n", sep = "")

      # Extract matrix of plots for this landCover, fully identified species only
      # Filter focal and bm separately, then combine
      spCov.f <- subset(dat.f,
        !is.nan(species) & !(species == "<NA>") &
          landCover == curr.landCover,
        select = c(plotCode, EI.tr, species)
      )
      spCov.b <- subset(dat.b,
        !is.nan(species) & !(species == "<NA>") &
          vegClass == curr.vegClass,
        select = c(plotCode, EI.tr, species)
      )
      spCov <- rbind(spCov.f, spCov.b)

      # Make another matrix of plots (by omitting species and abundance)
      plots.f <- subset(dat.f,
        landCover == curr.landCover,
        select = c(plotCode, focalOrBenchmark, landCover, vegClass)
      )
      plots.b <- subset(dat.b,
        vegClass == curr.vegClass,
        select = c(plotCode, focalOrBenchmark, landCover, vegClass)
      )
      plots <- rbind(plots.f, plots.b)
      plots <- plots[!duplicated(plots[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")]), ]

      n.f <- nrow(plots[plots$focalOrBenchmark == "f", ])
      n.b <- nrow(plots[plots$focalOrBenchmark == "b", ])
      n.tot <- n.f + n.b

      #############################
      # NMDS
      #
      # nmds parameters set by function
      # nmds.params() in main params file
      #############################

      # Get key NMDS parameters
      # These are set in the main params file
      # Will use default values, unless this vegetation
      # is listed in the function. Which is why
      # these parameters are being set here

      # Get NMDS parameters for current (focal) landCover
      nmds.pars <- nmds.params(curr.landCover)

      # Set general parameters
      nmds.verbose <- nmds.pars$nmds.verbose
      nmds.set.seed <- nmds.pars$nmds.set.seed

      # Set metaMDS parameters
      nmds.seed <- nmds.pars$nmds.seed
      nmds.trymax <- nmds.pars$nmds.trymax
      nmds.k <- nmds.pars$nmds.k
      nmds.maxit <- nmds.pars$nmds.maxit

      # Echo nmds parameters
      cat("  Sample sizes:\n")
      cat("    n.f=", n.f, ", n.b=", n.b, ", n.tot=", n.tot, "\n", sep = "")
      cat("  Parameters:\n")
      cat("    seed=", nmds.seed, "\n", sep = "")
      cat("    k=", nmds.k, "\n", sep = "")
      cat("    trymax=", nmds.trymax, "\n", sep = "")
      cat("    maxit=", nmds.maxit, "\n", sep = "")

      # write the file with headers
      write.table(spCov, file = "spCov.txt", quote = FALSE, sep = "\t", col.names = FALSE, row.names = FALSE)

      # Read it back in using function readsample (package picante) to
      # coerce into 'species x site matrix' format required by vegan
      spCov <- readsample("spCov.txt")
      unlink("spCov.txt")

      if (scale.abund == TRUE) {
        # Turn % cover into relative abundance by dividing each value by sample total abundance
        # apply(spCov, 1, sum) # check total abundance in each sample
        spCov <- decostand(spCov, method = "total")
        # apply(spCov, 1, sum) # check total abundance in each sample; all be 1 if relativized
        # spCov[1:5, 1:5]  # Inspect sample of the transformed data
      }

      # The NMDS
      if (set.seed == TRUE) set.seed(nmds.seed) # Set in params file

      spCov.mds <- metaMDS(spCov,
        distance = "bray",
        try = 20,
        trymax = nmds.trymax,
        k = nmds.k,
        maxit = nmds.maxit,
        autotransform = FALSE,
        trace = nmds.verbose
      )

      # Echo actual iterations and tries
      converged <- spCov.mds$converged
      stress <- spCov.mds$stress

      cat("  Results:\n")
      cat("    iterations=", spCov.mds$iters, "\n", sep = "")
      cat("    tries=", spCov.mds$tries, "\n", sep = "")
      cat("    stress=", stress, "\n", sep = "")
      cat("    converged=", converged, "\n", sep = "")

      if (converged == TRUE || converged > 0) tot.converged <- tot.converged + 1
      if (converged >= 10) tot.converged.high <- tot.converged.high + 1

      # Get species scores
      sampleScores <- data.frame(spCov.mds$points)

      # Restructure the rest of the df, converting row labels (plot
      # names) to column, and merge the two matrices
      for (j in 1:nrow(sampleScores)) {
        rn <- rownames(sampleScores)
        if (nmds.k == 2) sampleScores[j, 3] <- "" # Add dummy z col if 2D
        sampleScores[j, 4] <- nmds.k # Number of dimensions
        sampleScores[j, 5] <- rn[j] # Plot code
        sampleScores[j, 6] <- stress # Final stress
        sampleScores[j, 7] <- converged # Converged?
      }
      colnames(sampleScores) <- c("x", "y", "z", "dim", "plotCode", "stress", "converged")
      TdPlotTemp <- merge(plots, sampleScores, by = "plotCode")

      # Calculate centroids and taxonomic distance
      if (nmds.k == 2) {
        # Calculate 2D centroid of benchmark plots
        # using the mean coordinates algorithm
        bmTd <- subset(TdPlotTemp,
          TdPlotTemp$focalOrBenchmark == "b",
          select = c("x", "y")
        )
        centroid <- c(mean(bmTd$x), mean(bmTd$y))

        # Calculate TD (Taxonomic Distance) as 2D Eculidean
        # distance from each plot to centroid
        cen.x <- centroid[1]
        cen.y <- centroid[2]
        TdPlotTemp$td <- sqrt(
          (TdPlotTemp$x - cen.x)^2 + (TdPlotTemp$y - cen.y)^2
        )
      } else if (nmds.k == 3) {
        # Calculate 3D centroid of benchmark plots
        # using the mean coordinates algorithm
        bmTd <- subset(TdPlotTemp,
          TdPlotTemp$focalOrBenchmark == "b",
          select = c("x", "y", "z")
        )
        centroid <- c(mean(bmTd$x), mean(bmTd$y), mean(bmTd$z))

        # Calculate TD (Taxonomic Distance) as 3D Euclidean
        # distance from each plot to centroid
        cen.x <- centroid[1]
        cen.y <- centroid[2]
        cen.z <- centroid[3]
        TdPlotTemp$td <- sqrt(
          (TdPlotTemp$x - cen.x)^2 + (TdPlotTemp$y - cen.y)^2 +
            (TdPlotTemp$z - cen.z)^2
        )
      } else {
        stop(paste0("ERROR: invalid value (", nmds.k, ") of NMDS parameter k"))
      }

      if (TD.BM.PLOTS.DEDUPLICATE == FALSE) {
        #######################################################
        # Add a unique suffix to vegClass for current NMDS
        # run.
        #
        # Ensures that focal and benchmark plots are
        # grouped together when calculating quality. Bm plots
        # will be replicated for each landcover class linked
        # to that benchmark, but this OK because NMDS results
        # for the bm plots are different each time they are
        # ordinated with a different vegetation.
        #
        # Workaround until v7 of vegan is released and we are
        # able to add the focal ordination to a single set of
        # pre-existing benchmark scores from a separate NMDS run
        #
        # If TD.BM.PLOTS.DEDUPLICATE==TRUE, be sure to de-
        # duplicate bm plots. See second use of this parameter
        # below (after NMDS landCover loop)
        #######################################################

        # create a left-padded integer of exactly three digits
        suffix.num <- as.character(i)
        l.pad.n <- 3 - nchar(suffix.num)
        l.pad <- ""
        if (l.pad.n > 0) l.pad <- paste(rep("0", l.pad.n), collapse = "")

        # Create the suffix and append to vegClass for both focal and benchmark plots
        bm.suffix <- paste0("_", l.pad, suffix.num)
        TdPlotTemp$vegClass <- paste0(TdPlotTemp$vegClass, bm.suffix)
      }

      if (i == 1) {
        # Begin the final matrix
        TdPlot <- TdPlotTemp
      } else {
        # Append the next set of rows
        TdPlot <- rbind(TdPlot, TdPlotTemp)
      }
    } # END NMDS landCover loop

    # Echo summary of convergence results for all veg classes
    perc.conv <- specify_decimal(tot.converged / tot.landCoverClasses * 100, 1)
    cat("*********************************\n")
    cat("Convergence results:\n")
    cat("  ", tot.converged, " veg classes out of ", tot.landCoverClasses,
      " converged (", perc.conv, "%)\n",
      sep = ""
    )
    if (tot.converged.high >= 10) {
      cat("  ", tot.converged.high, " classes took >10 iterations to converge and may be unstable--recommend checking.\n", sep = "")
    }
    cat("*********************************\n\n")

    #######################################################
    # Add pseudo-stratum column
    #######################################################

    # Get column names
    cols <- colnames(TdPlot)

    # Reorder columns, keeping stratum column if present,
    # and keeping the transformed EI column
    if (!any("stratum" %in% cols)) {
      has.stratum <- FALSE
      TdPlot$stratum <- "nostrata"
    }

    if (TD.BM.PLOTS.DEDUPLICATE == TRUE) {
      # Remove duplicated bm plots
      # Workaround until vegan 7.0 released
      TdPlot <- TdPlot[!duplicated(TdPlot[, c("plotCode", "focalOrBenchmark", "landCover")]), ]
    }

    names(TdPlot)[names(TdPlot) == "td"] <- "EI"
    TdPlot$EI.tr <- TdPlot$EI
    TdPlot <- TdPlot[, c("vegClass", "landCover", "stratum", "focalOrBenchmark", "plotCode", "x", "y", "z", "dim", "stress", "converged", "EI", "EI.tr")]

    # Inspect the contents
    sorted <- TdPlot[order(
      TdPlot$vegClass, TdPlot$landCover,
      TdPlot$focalOrBenchmark, TdPlot$plotCode
    ), ]
    # head(sorted)

    #######################################################
    # Rescale TD values
    # Objective is to avoid bad pdf fits when one or both vectors (f & b) have
    # means at or below one
    #######################################################

    cat("Rescaling all TD to >1...")

    td.scaled.mult <- function(x, y) {
      # Stretch transformation
      # Increases scale of TD to ensure mean >>1
      # Resolves distortions caused by 0<TD<1
      # (i.e., removes or reduces fractional values of TD)

      # Multiply x by y, assuming x is float >= 0
      for (i in length(x)) {
        if (!(is.null(x[i]) || is.na(x[i]))) {
          x[i] <- x[i] * y
        }
      }

      return(x)
    }

    # Transform both EI and EI.tr because distinction between these two
    # is mixed up in later functions (need to fix)
    # No change if td.multiplier==1
    # TdPlot$EI.tr[ ! TdPlot$landCover %in% td.multiplier.omit ] <-
    # 	td.scaled.mult(TdPlot$EI.tr[ ! TdPlot$landCover %in% td.multiplier.omit ], td.multiplier)
    # TdPlot$EI[ ! TdPlot$landCover %in% td.multiplier.omit ] <-
    # 	td.scaled.mult(TdPlot$EI[ ! TdPlot$landCover %in% td.multiplier.omit ], td.multiplier)

    # Above not working; do as loop instead
    # Inefficient but it works
    if (!td.multiplier == 1) {
      for (i in 1:nrow(TdPlot)) {
        EI.tr.i <- TdPlot$EI.tr[i]
        EI.i <- TdPlot$EI[i]
        lc.i <- TdPlot$landCover[i]

        if (!lc.i %in% td.multiplier.omit) {
          EI.tr.i_scaled <- td.scaled.mult(EI.tr.i, td.multiplier)
          EI.i_scaled <- td.scaled.mult(EI.i, td.multiplier)
        }
        TdPlot$EI.tr[i] <- EI.tr.i_scaled
        TdPlot$EI[i] <- EI.i_scaled
      }
    }

    cat("done\n")

    #######################################################
    # Flag NMDS outliers and save to file
    # This file can be used to exclude outlier plots
    # by repeating the raw data import after setting
    # REMOVE.TD.OUTLIERS==TRUE
    #######################################################

    cat("Checking for new NMDS outliers...")

    # Need to extract bm vegetation again as these have changed
    landCoverVegClasses <- unique(TdPlot[TdPlot$focalOrBenchmark == "f", c("landCover", "vegClass")])
    TdPlot$outlier <- FALSE

    for (i in 1:tot.landCoverClasses) { # BEGIN outlier landCover loop
      # Get current landCover and bm vegetation codes
      curr.landCover <- landCoverClasses[i]
      curr.vegClass <- landCoverVegClasses[landCoverVegClasses$landCover == curr.landCover, c("vegClass")]

      # Extract matrix of plots for this landCover
      # Filter focal and bm separately, then combine
      nmds.scores.f <- TdPlot[
        TdPlot$landCover == curr.landCover & TdPlot$focalOrBenchmark == "f",
      ]
      nmds.scores.b <- TdPlot[
        TdPlot$vegClass == curr.vegClass & TdPlot$focalOrBenchmark == "b",
      ]
      nmds.scores <- rbind(nmds.scores.f, nmds.scores.b)
      dims <- as.integer(unique(nmds.scores$dim))
      fb.vals <- c("f", "b")

      for (fb in fb.vals) { # BEGIN for ( fb in fb.vals )
        curr.nmds.scores <- nmds.scores[nmds.scores$focalOrBenchmark == fb, ]

        # Test #1: TD > fixed maximum (=TD.OUTLIER.UPPER.THRESHOLD)
        # Extreme outliers must be removed as they can prevent Test #2
        # from working
        curr.nmds.scores$outlier[
          curr.nmds.scores$EI.tr > TD.OUTLIER.UPPER.THRESHOLD
        ] <- TRUE

        # Test #2: TD outside TD.OUTLIER.STDEVS standard deviations
        # of mean TD.
        mean <- mean(curr.nmds.scores$EI.tr)
        sds <- sd(curr.nmds.scores$EI.tr) * TD.OUTLIER.STDEVS
        upper <- mean + sds
        lower <- mean - sds

        # Flag the outliers
        curr.nmds.scores$outlier[
          curr.nmds.scores$EI.tr > upper | curr.nmds.scores$EI.tr < lower
        ] <- TRUE

        # # Old method: flags NMDS scores outside TD.OUTLIER.STDEVS std deviations
        # mean <- mean( curr.nmds.scores$x ); sds <- sd( curr.nmds.scores$x )*TD.OUTLIER.STDEVS
        # x.upper <- mean+ sds
        # x.lower <- mean-sds
        #
        # mean <- mean( curr.nmds.scores$y ); sds <- sd( curr.nmds.scores$y )*TD.OUTLIER.STDEVS
        # y.upper <- mean+sds
        # y.lower <- mean-sds
        #
        # if (dims==3 ) {
        #   mean <- mean( curr.nmds.scores$z ); sds <- sd( curr.nmds.scores$z )*TD.OUTLIER.STDEVS
        #   z.upper <- mean+sds; z.lower <- mean-sds
        #
        #   curr.nmds.scores$outlier[
        #     curr.nmds.scores$x > x.upper | curr.nmds.scores$x < x.lower |
        #     curr.nmds.scores$y > y.upper | curr.nmds.scores$y < y.lower |
        #     curr.nmds.scores$z > z.upper | curr.nmds.scores$z < z.lower
        #   ] <- TRUE
        # } else if (dims==2) {
        #   curr.nmds.scores$outlier[
        #     curr.nmds.scores$x > x.upper | curr.nmds.scores$x < x.lower |
        #     curr.nmds.scores$y > y.upper | curr.nmds.scores$y < y.lower
        #   ] <- TRUE
        # }

        # Update outlier column in TdPlot for this set of plots
        curr.plots <- curr.nmds.scores$plotCode
        TdPlot$outlier[TdPlot$plotCode %in% curr.plots] <- curr.nmds.scores$outlier
      } # END for ( fb in fb.vals )
    } # END outlier landCover loop

    if (!exists("F.TD.OUTLIERS")) stop("Parameter F.TD.OUTLIERS not defined!")
    outlier.file <- paste0(RESULTSDIR, F.TD.OUTLIERS)

    if (!file.exists(outlier.file)) {
      # Save backup of raw data file if this is the first run
      # Backup can be used to plot NMDS outliers
      write.csv(TdPlot, file = rawFile.bak, row.names = FALSE)
    }

    # count outliers
    n.outliers <- length(TdPlot$plotCode[TdPlot$outlier == TRUE])

    if (n.outliers == 0) {
      cat("no outliers found\n")
    } else {
      cat(n.outliers, " new outliers found!\n", sep = "")

      cat("Saving NMDS outliers:\n")
      td.outliers <- TdPlot[TdPlot$outlier == TRUE, ]
      td.outliers <- unique(td.outliers[, c("plotCode", "landCover", "vegClass", "focalOrBenchmark")])
      # TdPlot <- TdPlot[ TdPlot$outlier==FALSE, ]

      if (!file.exists(outlier.file)) {
        # Save the outliers to new file
        cat("- Saving new file '", F.TD.OUTLIERS, "'...", sep = "")
      } else {
        if (TD.OUTLIERS.APPEND == TRUE) {
          cat("- Appending to existing file '", F.TD.OUTLIERS, "'...", sep = "")
          td.outliers.orig <- read.csv(outlier.file, header = TRUE)
          td.outliers <- rbind(td.outliers.orig, td.outliers)
          td.outliers <- unique(td.outliers)
        } else {
          # Warn and replace
          cat("- Replacing existing file '", F.TD.OUTLIERS, "'...", sep = "")
        }
      }
      write.csv(td.outliers, file = outlier.file, row.names = FALSE)
      cat("done\n")
    }


    cat("Appending outliers and focal plots with no data")
    # Add column "no_data_plot"
    TdPlot$no_data_plot <- FALSE

    if (INCLUDE.PLOTS.NODATA) {
      cat(":\n")
      cat("- Checking for missing focal plots...")
      currPlots <- unique(TdPlot$plotCode)
      allPlots <- unique(plotMetadata$plotCode[plotMetadata$focalOrBenchmark == "f"])
      missingPlots <- setdiff(allPlots, currPlots)
      n_missing <- length(missingPlots)

      if (n_missing == 0) {
        cat("no missing plots...done\n")
      } else {
        cat(n_missing, " plots found\n", sep = "")
        cat("- Adding missing plots and metadata...")
        TdPlot_missing <- TdPlot[FALSE, ]
        TdPlot_missing[1:n_missing, ] <- NA
        TdPlot_missing$plotCode <- missingPlots
        TdPlot_missing <- TdPlot_missing %>%
          left_join(plotMetadata, by = "plotCode", suffix = c("", ".new")) %>%
          mutate(across(
            .cols = ends_with(".new"),
            .fns = ~ coalesce(.x, get(sub(".new", "", cur_column()))),
            .names = "{sub('.new', '', .col)}"
          )) %>%
          dplyr::select(-ends_with(".new"))
        TdPlot_missing$stratum <- "nostrata"
        cat("done\n")

        cat("- Flagging outlier plots...")
        if (file.exists(outlier.file)) {
          # Reload outlier plot file
          td.outliers <- read.csv(outlier.file, header = TRUE)
          outlierPlots <- td.outliers[, c("plotCode"), drop = FALSE]
          outlierPlots$is_outlier <- TRUE
          TdPlot_missing <- merge(TdPlot_missing, outlierPlots, by = "plotCode", all.x = TRUE)
          TdPlot_missing$outlier <- TdPlot_missing$is_outlier
          TdPlot_missing <- TdPlot_missing[, !names(TdPlot_missing) == "is_outlier"]
          cat("done\n")
        } else {
          cat("none present...done\n")
        }

        cat("- Flagging no-data plots...")
        # TdPlot_missing$no_data_plot[ is.na(TdPlot_missing$outlier) ] <- TRUE
        # TdPlot_missing$no_data_plot[ is.na(TdPlot_missing$no_data_plot) ] <- FALSE
        TdPlot_missing$no_data_plot <- TRUE
        cat("done\n")

        cat("- Assigning vegetation-specific no-data indicator values...")
        df.vegClasses <- unique(plotMetadata[, c("vegClass"), drop = FALSE])
        df.vegClasses$EI <- as.numeric(NA)
        vegClasses <- unique(df.vegClasses$vegClass)
        TdPlot_bm <- TdPlot[TdPlot$focalOrBenchmark == "b", ]
        TdPlot_f <- TdPlot[TdPlot$focalOrBenchmark == "f", ]

        # EI
        EI.means <- aggregate(
          EI ~ vegClass,
          data = TdPlot_bm,
          FUN = base::mean
        )
        colnames(EI.means) <- c("vegClass", "EI.mean")
        EI.sds.bm <- aggregate(
          EI ~ vegClass,
          data = TdPlot_bm,
          FUN = sd
        )
        colnames(EI.sds.bm) <- c("vegClass", "EI.sd")
        EI.sds.f <- aggregate(
          EI ~ vegClass,
          data = TdPlot_f,
          FUN = sd
        )
        colnames(EI.sds.f) <- c("vegClass", "EI.sd")
        EI.sds <- pmax(EI.sds.bm, EI.sds.f)
        colnames(EI.sds) <- c("vegClass", "EI.sd")
        EI.sds$EI.sd <- as.numeric(EI.sds$EI.sd)
        EI.vals <- merge(EI.means, EI.sds, by = "vegClass")
        EI.vals$EI <- EI.vals$EI.mean + (EI.vals$EI.sd * NODATA.SD)
        EI.vals <- EI.vals[, c("vegClass", "EI")]

        TdPlot_missing <- TdPlot_missing[, !names(TdPlot_missing) == "EI"]
        TdPlot_missing <- merge(TdPlot_missing, EI.vals, by = "vegClass", all.x = TRUE)
        TdPlot_missing <- df.reorder(TdPlot_missing, col.move = "EI", col.before = "converged")

        # EI.tr
        EI.means <- aggregate(
          EI.tr ~ vegClass,
          data = TdPlot_bm,
          FUN = base::mean
        )
        colnames(EI.means) <- c("vegClass", "EI.mean")
        EI.sds.bm <- aggregate(
          EI.tr ~ vegClass,
          data = TdPlot_bm,
          FUN = sd
        )
        colnames(EI.sds.bm) <- c("vegClass", "EI.sd")
        EI.sds.f <- aggregate(
          EI.tr ~ vegClass,
          data = TdPlot_f,
          FUN = sd
        )
        colnames(EI.sds.f) <- c("vegClass", "EI.sd")
        EI.sds <- pmax(EI.sds.bm, EI.sds.f)
        colnames(EI.sds) <- c("vegClass", "EI.sd")
        EI.sds$EI.sd <- as.numeric(EI.sds$EI.sd)
        EI.vals <- merge(EI.means, EI.sds, by = "vegClass")
        EI.vals$EI.tr <- EI.vals$EI.mean + (EI.vals$EI.sd * NODATA.SD)
        EI.vals <- EI.vals[, c("vegClass", "EI.tr")]

        TdPlot_missing <- TdPlot_missing[, !names(TdPlot_missing) == "EI.tr"]
        TdPlot_missing <- merge(TdPlot_missing, EI.vals, by = "vegClass", all.x = TRUE)
        TdPlot_missing <- df.reorder(TdPlot_missing, col.move = "EI.tr", col.before = "EI")
        cat("done\n")

        cat("- Appending no-data plots to main results data frame...")
        TdPlot <- rbind(TdPlot, TdPlot_missing)
        cat("done\n")
      }
    } else {
      cat("...skipping (INCLUDE.PLOTS.NODATA==FALSE)\n")
    }

    # Write file of taxonomic distances as raw data file
    write.csv(TdPlot, file = rawFile, row.names = FALSE)
  } # END prepare.raw

  cat("**************************************\n")
  cat("Preparing raw indicator values file\n")
  cat("**************************************\n")

  #######################################################
  # Calculate and save benchmark values
  #######################################################

  cat("Summarizing benchmark values...")

  # Re-import prepared raw data
  inputFile <- paste(RESULTSDIR, rawFileName, sep = "")
  # cat("InputFile=",inputFile)
  TdPlot <- read.csv(inputFile, header = T)
  TdPlot.bm <- subset(TdPlot, TdPlot$focalOrBenchmark == "b")

  # First, drop land cover class & remove duplicated bm plots
  TdPlot.bm <- subset(TdPlot.bm, select = -c(landCover))
  TdPlot.bm <- unique(TdPlot.bm)

  # Make data frame of moments for transformed species cover
  # aggregated by bm vegetation (vegclass) + stratum
  tdFinal.bm <- with(TdPlot.bm, aggregate(EI.tr, list(vegClass = vegClass),
    FUN = function(x) {
      c(
        n = length(x),
        mean = mean(x),
        sd = sd(x),
        med = median(x),
        max = max(x),
        min = min(x)
      )
    }
  ))
  tdFinal.bm <- cbind(tdFinal.bm[-ncol(tdFinal.bm)], tdFinal.bm[[ncol(tdFinal.bm)]])
  colnames(tdFinal.bm) <- c("vegClass", "n", "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min")

  # Add dummy stratum column
  tdFinal.bm$stratum <- "nostrata"

  # Add EI & rearrange the columns
  tdFinal.bm$EI <- curr.EI # add indicator
  tdFinal.bm <- tdFinal.bm[, c("EI", "vegClass", "stratum", "n", "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min")]

  # Make final, human-readable veg classes
  tdFinal.bm$vegClass <- human.readable(tdFinal.bm$vegClass)

  # Inspect the columns of tdFinal.bm
  sorted <- tdFinal.bm[order(tdFinal.bm$vegClass), ]
  head(sorted)

  # save benchmark results file
  resultsFile <- paste(RESULTSDIR, resultsBm, sep = "")
  write.csv(tdFinal.bm, file = resultsFile, row.names = FALSE)

  cat("done\n")

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

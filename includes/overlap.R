##########################################################
# Fits distribution to focal & benchmark distributions of Ecological Indicators (EI),
# plots overlap between distributions and saves results
#
# Requires the following data & parameters from the calling script:
# 		EIResults - data frame of raw EI values from each plot, with EIs already
# 			transformed, if applicable. Should contain both focal and benchmark
# 			plots; subsetting is done by this script.
# 		curr.EI - Standard abbreviation of the EI
# 		EI.name - Plain English name of EI
# 		distn - distribution model code; see list of values in calling script
# 		dist.type - distribution type ('continuous','discrete')
#
# 	Note: This version uses ggplot not hist to produce figures
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
# Created: August 25, 2016
#
##########################################################

############################################
# Graph parameters
# MOVE THESE TO MAIN PARAMS SCRIPTS AFTER TESTING
############################################

# Type of figure file
# Options: "pdf", "png" (just these for now; pdf for hi res only)
fig.file.type <- "pdf"

# Font family options:
# "AvantGarde", "Bookman", "Courier", "Helvetica", "Helvetica-Narrow",
# "NewCenturySchoolbook", "Palatino" or "Times"
# See: http://math.furman.edu/~dcs/courses/math47/R/library/grDevices/html/pdf.html#:~:text=R%20does%20not%20embed%20fonts,(equivalently%20%22symbol%22%20).
# Use "Helvetica" for Arial
fig.font.fam <- "Helvetica"

# Remaining options
fig.width.cm <- 9
res <- 500 # DPI (ignored for pdf)

############################################
# For development only
# --> Comment out for production
############################################

# # Reload parameters and functions (development only)
# source("params.R", local=TRUE)
# source(paste(INCLUDESDIR, "functions.R", sep=''), local=TRUE)

# # Fixed values for testing without loop
# j<- 1   	#curr.lc
# k<-1		#curr.stratum

############################################
# END For development only
###########################################

graphics.off()

############################################
# Parameters
############################################

no.bins <- 12 # Number of bins to use for histogram, adjust as needed
trt1.txt <- "Benchmark"
trt2.txt <- "Focal"

do.graph <- FALSE # this option is turned on if other tests succeed
has.data <- TRUE

# Name of file of bootstrap overlap runs
boot.ol.filename <- paste0(curr.EI, "_boot.ol", ".csv")

##########################################################
# Get attributes for current EI (curr.EI)
##########################################################

cat("\n")
cat("***************************************\n")
cat("Generating overlap figures\n")
cat("***************************************\n")

curr.EI <- as.character(curr.EI) # Just in case this is factor in calling script

# echo current EI and distribution
cat("")
cat(paste0(curr.EI, " - ", EI.name, " (distribution=", distn, ")", "\n"))

#################################################
# Load raw data for this EI & prepared focal results df
#################################################

# Load raw data and order by stratum
rawFileName <- paste(curr.EI, "_raw.csv", sep = "")
rawFile <- paste(RESULTSDIR, rawFileName, sep = "")
dat.EI <- read.csv(rawFile, header = T, stringsAsFactors = FALSE)
dat.EI <- with(dat.EI, dat.EI[order(
  dat.EI$landCover, dat.EI$vegClass,
  dat.EI$focalOrBenchmark, dat.EI$plotCode, dat.EI$stratum
), ])

# Load prepared focal results data frame
# Add columns for overlap results
f.results.file <- paste(RESULTSDIR, resultsFocal, sep = "")
f.results <- read.csv(f.results.file, header = T, stringsAsFactors = FALSE)

#################################################
# Set pdf x values for this EI
#################################################

max.ei <- max(dat.EI$EI)

if (distn == "Bet") {
  xs <- seq(0, 1, length.out = 100)
} else if (distn == "NBin") {
  xs <- xs_nbin <- seq(0, max.ei + 10)
} else if (distn == "gamma") {
  xs <- seq(0, max.ei, length.out = 1000)
}

##########################################################
# Set path to save output figures
#
# This is done after initally loading and inspecting the data in case parameter
# group.strata gets reset
##########################################################

# Base directory name for ungroup figure of combine focal and bm dists
figs.fitted.dir.name <- "overlap"

# Create directory if missing
# dir.create(file.path(figdir.abs, figs.fitted.dir.name), showWarnings = F)
# figs.fitted.dir <- paste(figs.dir, figs.fitted.dir.name, sep='')
dir.create(file.path(FIGDIR, figs.fitted.dir.name), showWarnings = F)
figs.fitted.dir <- paste(FIGDIR, figs.fitted.dir.name, sep = "")

#######################################
# Set up data frame to hold bootstrap overlap runs
#######################################
boot.ol.df <- data.frame(
  vegClass = character(),
  stratum = character(),
  boot.reps = integer(),
  ol.obs = double()
)
boot.cols <- data.frame(matrix(NA, nrow = 0, ncol = boot.reps))
boot.cols <- convert.magic(boot.cols, "numeric")
boot.ol.df <- cbind(boot.ol.df, boot.cols)

##########################################################
# Loop through vegetation and strata (if applicable), fitting the requested
# distribution and saving parameters. Plot and save the figures
##########################################################

# get vegetation classes
allveg <- unique(dat.EI$vegClass)
n.veg <- length(allveg)

for (j in 1:n.veg) { # START veg loop
  # for (j in 1:1) {				# Alternative for testing with loop
  curr.veg <- allveg[j]

  # Make unix-friendly version of vegetation for filename
  veg.fname <- gsub(" ", "_", curr.veg)
  veg.fname <- gsub("/", "_", veg.fname)
  veg.fname <- gsub(",", "", veg.fname)

  cat("\n")
  cat("====================================\n")
  cat(paste0("Current vegetation: ", veg.fname, "\n"))

  EI.veg <- subset(dat.EI, dat.EI$vegClass == curr.veg)

  # get strata
  strata <- unique(EI.veg$stratum)
  n.strata <- length(strata)

  # Echo the appropriate par command
  fig.cols <- 1 # Dummy values
  fig.rows <- 1

  for (k in 1:n.strata) { # START strata loop
    # for (k in 1:1) {					# Alternative for testing with loop

    # Set general graph attributes plus attributes
    # specific to current EI and vegetation
    text.main <- curr.veg
    text.x <- EI.name
    text.y <- "Count of Plots"

    # Get subset for this stratum and vegetation type, benchmark only
    curr.stratum <- strata[k]
    EI.strat <- subset(EI.veg, EI.veg$stratum == curr.stratum)
    cat("------------------------------------------------------------\n")
    cat(paste0("Current stratum: ", curr.stratum, "\n"))

    # Make unix-friendly version of stratum for filename
    stratum.fname <- gsub(" ", "_", curr.stratum)
    stratum.fname <- gsub("/", "_", stratum.fname)
    stratum.fname <- gsub(",", "", stratum.fname)
    stratum.fname <- tolower(stratum.fname)
    # stratum.fname <- paste('_', stratum.fname, sep='')
    if (!has.stratum) stratum.fname == ""

    # Check if data for this stratum
    has.data <- T
    if (nrow(EI.strat) == 0) {
      has.data <- F
      print(paste("WARNING: NO DATA (", curr.veg, "; ", curr.stratum, ")", sep = ""))
    }

    # Check if EI values all the same
    # If so, need special handling
    ei.numvals <- length(unique(EI.strat$EI))
    cat(paste0("  Unique values this stratum: ", ei.numvals, "\n"))

    # More graph attributes
    text.main <- curr.veg

    # Custom text for specific EIs as needed
    if (curr.EI == "PCGF") {
      text.x <- paste("Percent Cover ", curr.stratum, sep = "")
    } else if (curr.EI == "asc" || curr.EI == "ASC") {
      text.x <- paste("Abundance ", curr.stratum, sep = "")
    } else if (curr.EI == "GC" | curr.EI == "PCESS") {
      text.x <- paste("Percent Cover ", curr.stratum, sep = "")
    } else if (curr.EI == "SR") {
      text.x <- "Species Richness"
    } else if (curr.EI == "PES") {
      text.x <- "Proportion Exotic Species"
    } else if (curr.EI == "TD") {
      text.x <- "Taxonomic Distance from Benchmark Mean (Centroid)"
    }

    # get sample sizes for figure legend
    n.f <- nrow(EI.strat[EI.strat$focalOrBenchmark == "f" & !is.na(EI.strat$EI), ])
    n.bm <- nrow(EI.strat[EI.strat$focalOrBenchmark == "b" & !is.na(EI.strat$EI), ])

    # Compose text for focal/benchmark colour legends
    f.text <- "Focal"
    bm.text <- "Benchmark"

    if (show.n == T) {
      f.text <- paste0("Focal (n=", n.f, ")")
      bm.text <- paste0("Benchmark (n=", n.bm, ")")
    }

    # Get vector of EIservations for the current vegetation and EI
    EIs <- as.vector(EI.strat$EI)

    # Set to false for any distn that isn't ready yet
    under.construction <- FALSE

    #####################################
    # Fit distributions (focal and benchmark)
    #####################################

    cat(paste0("  Distribution=", distn, "\n"))

    if (has.data == T & distn == "Bet") {
      ######################
      # Beta distribution
      ######################

      # Transform if 0s or 1s present
      # Beta distribution does not include 0 & 1
      if (min(EI.strat$EI) == 0 || max(EI.strat$EI) == 1) {
        n <- nrow(EI.strat)
        EI.strat$EI <- (EI.strat$EI * (n - 1) + 0.5) / n
      }

      # Make separate vectors for each treatment
      b <- EI.strat[EI.strat$focalOrBenchmark == "b", c("EI")]
      f <- EI.strat[EI.strat$focalOrBenchmark == "f", c("EI")]

      # Upper and lower limits to prevent fitdistr optimization
      # from straying outside bounds the of the beta distribution
      lowlim <- 0.0000001
      uplim <- 1 - lowlim

      # Starting shape parameters, required for beta distr
      shape1 <- 0.1
      shape2 <- 0.1

      # Range of x
      # xs <- seq(0,1, length.out= no.bins)
      xs <- seq(0, 1, length.out = 100)

      # Fit distributions, calculate overlap & bootstrap conf. limits
      # Also returns bootsrapped pdfs and overlaps
      overlap <- boot.overlap(distn, b, f,
        xs = xs,
        boot.reps = boot.reps, seed = seed, set.seed = set.seed,
        shape1 = shape1, shape2 = shape2, lowlim = lowlim
      )
    } else if (has.data == T & distn == "NBin") {
      ######################
      # Negative binomial
      ######################

      # Make separate vectors for each treatment
      b <- EI.strat[EI.strat$focalOrBenchmark == "b", c("EI")]
      f <- EI.strat[EI.strat$focalOrBenchmark == "f", c("EI")]

      # Fit distributions, calculate overlap & bootstrap conf. limits
      # Also returns bootsrapped pdfs and overlaps
      xs <- seq(0, max(max(f), max(b)) + 10) # range of x
      overlap <- boot.overlap(distn, b, f,
        xs = xs, boot.reps = boot.reps,
        seed = seed, set.seed = set.seed
      )
      # # Use the updated function instead
      # # Handles zeros without crashing
      # overlap <- boot.overlap.diff(distn, test.tail= test.tail, f=f, b=b, xs=xs,
      # boot.reps=boot.reps, seed=seed, set.seed=set.seed,
      # q.tr.logit.inverse= q.tr.logit.inverse, logit.inverse.beta= logit.inverse.beta
      # )
    } else if (has.data == T & ei.numvals > 1 & distn == "norm") {
      ######################
      # Normal distribution
      ######################

      under.construction <- TRUE
    } else if (has.data == T & ei.numvals > 1 & distn == "gamma") {
      ######################
      # Gamma
      ######################

      # Check for values <0
      if (min(EI.strat$EI) < 0) {
        stop("ERROR: Values < 0 not allowed for gamma distribution!")
      }

      # Make separate vectors for each treatment
      b <- EI.strat[EI.strat$focalOrBenchmark == "b", c("EI")]
      f <- EI.strat[EI.strat$focalOrBenchmark == "f", c("EI")]

      # Upper and lower limits to prevent fitdistr optimization
      # from straying outside bounds the of the beta distribution
      lowlim <- 0.0000001
      uplim <- max(EI.strat$EI) + (0.1 * max(EI.strat$EI))

      # Starting shape parameters, required for beta distr
      shape <- 1
      rate <- 1

      # Range of x
      # xs <- seq(0, max( max(f),max(b) ), length.out=52)
      # Not sure where the above came from...
      xs <- seq(0, max.ei, length.out = 1000)

      # Fit distributions, calculate overlap & bootstrap conf. limits
      # Also returns bootsrapped pdfs and overlaps
      overlap <- boot.overlap(
        distn,
        v1 = b, v2 = f, xs = xs, boot.reps = boot.reps,
        seed = seed, set.seed = set.seed, shape = shape,
        rate = rate, lowlim = lowlim, uplim = uplim
      )
    } # END Fit distribution

    ######################################
    # Save distribution-fitting and overlap results
    ######################################

    if (!overlap[[1]] == "FAIL") {
      # if (distn=='NBin') {
      # ol <- overlap$ol
      # ol.sd <- overlap$q.sd
      # ol.lcl <- overlap$q.lcl
      # ol.ucl <- overlap$q.ucl
      # boot.ol <- overlap$boot.q
      # do.graph <- TRUE
      # } else {
      ol <- overlap$ol.obs
      ol.sd <- overlap$ol.sd
      ol.lcl <- overlap$ol.cls[1]
      ol.ucl <- overlap$ol.cls[2]
      boot.ol <- overlap$boot.ol
      do.graph <- TRUE
      # }

      # append boot overlap runs to boot df.
      curr.boot.ol <- data.frame(curr.veg, curr.stratum, boot.reps, ol, t(boot.ol))
      boot.ol.df <- rbind(boot.ol.df, curr.boot.ol)
    } else {
      # Distribution fitting failed; set all variables to NA
      ol <- NA
      ol.sd <- NA
      ol.lcl <- NA
      ol.ucl <- NA
      boot.ol <- rep(NA, length(xs))
      do.graph <- FALSE
    }

    ######################################
    # Graph distributions and overlap
    ######################################

    title.text <- text.main

    # Full figure name, including stratum if applicable
    if (curr.stratum == "nostrata") {
      graphFileName <- paste(curr.EI, "overlap", veg.fname, sep = "_")
    } else {
      graphFileName <- paste(curr.EI, "overlap", veg.fname, stratum.fname, sep = "_")
    }

    if (overlap[1] == "FAIL") {
      cat("  WARNING: Cannot generate overlap figure for '", graphFileName, "'\n", sep = "")
    } else {
      ol.disp <- specify_decimal(ol, 3)
      ol.lcl.disp <- specify_decimal(ol.lcl, 3)
      ol.ucl.disp <- specify_decimal(ol.ucl, 3)
      cat(paste0("  Overlap=", ol.disp, " (", ol.lcl.disp, "-", ol.ucl.disp, ")\n"))

      if (do.graph) {
        if (nchar(text.x) <= 30) {
          title.text <- paste(text.x, curr.veg, sep = " - ")
        } else {
          title.text <- paste(curr.EI, curr.veg, sep = " - ")
        }

        #  Set graph file name
        graphFile <- paste(figs.fitted.dir, "/", graphFileName, ".", fig.file.type, sep = "")
        cat(paste0("  Printing graph: ", graphFileName, "\n"))

        # Initialize plot object now if running from Rscript
        # if ( !interactive() ) png(graphFile)
        if (!interactive()) {
          # Start plot now if running from Rscript command line
          # png(graphFile)

          if (fig.file.type == "png") {
            png(graphFile)
          } else if (fig.file.type == "pdf") {
            pdf(graphFile, onefile = TRUE, bg = "white", family = fig.font.fam, width = fig.width.cm / 2.5)
          } else {
            err.msg <- paste0("ERROR: fig.file.type '", fig.file.type, "' not supported! (see file graph.dists.R)")
            stop(err.msg)
          }
        } # END interactive

        graph <- boot.ol.plot(overlap,
          title.text = title.text,
          f.text = f.text,
          bm.text = bm.text,
          subsample = boot.graph.subsample,
          plot.boot = plot.boot.pdfs
        )

        # Save plot now if running in R Studio or R console
        # if ( interactive() ) dev.copy(png, graphFile)
        if (interactive()) {
          # dev.copy(png, graphFile)
          if (fig.file.type == "png") {
            dev.copy(png, graphFile)
          } else if (fig.file.type == "pdf") {
            dev.copy(pdf, graphFile)
          }
        } # END interactive

        invisible(dev.off())
      } else if (under.construction == TRUE) {
        print("  This distribution under construction!")
      } else if (has.data == FALSE) {
        print("  No data for this EI+stratum combination")
      }
    }
  } # End strata loop
} # end veg loop

####################################################
# Save results file
####################################################

# Save boostrap overlap runs
boot.ol.file <- paste(RESULTSDIR, boot.ol.filename, sep = "")
write.csv(boot.ol.df, file = boot.ol.file, row.names = FALSE)

cat("\n")

##########################################################
# Fits distribution to focal & benchmark distributions of Ecological Indicators (EI)
# and overlays histograms and fits in single figure
#
# Requires the following data & parameters from the calling script:
# 		curr.EI:			Standard abbreviation of the EI
# 		EI.name:		Plain English name of EI
# 		distn:			Distribution model code; see list of values in calling script
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
##########################################################

#########################################################
# Note: Plot single figures and grouped figures separately,
# as follows:
#
# Single figures:
#   Run from command line only, using following parameters:
#   plot.dists_fitted_rescaled <- TRUE
#   plot.dists_fitted_grouped_rescaled <- FALSE
#   FIG.TYPE <- "pdf" or FIG.TYPE <- "png"
#   Both file types work, but pdf are better (higher resolution)
#   Single figures currently not working from RStudio.
# Group figures:
#   Run from RStudio, using following parameters:
#   plot.dists_fitted_rescaled <- FALSE
#   plot.dists_fitted_grouped_rescaled <- TRUE
#   FIG.TYPE <- "pdf"
#   But note: grouped figures of indicators reduced
#   to a single stratum (e.g., PCESS with pcess.herbs.only==TRUE)
#   do not work. Use the single figures, as described above.
#########################################################

do.debug <- FALSE

############################################
# Graph parameters
# MOVE THESE TO MAIN PARAMS SCRIPTS AFTER TESTING
############################################

# Type of figure file (pdf, png, etc.)
# FIG.TYPE set in main params file
if (exists("FIG.TYPE")) {
  fig.file.type <- FIG.TYPE
} else {
  # Default, for backward compatibility in case not defined in params file
  fig.file.type <- "png"
}

# Font family options:
# "AvantGarde", "Bookman", "Courier", "Helvetica", "Helvetica-Narrow",
# "NewCenturySchoolbook", "Palatino" or "Times"
# See: http://math.furman.edu/~dcs/courses/math47/R/library/grDevices/html/pdf.html#:~:text=R%20does%20not%20embed%20fonts,(equivalently%20%22symbol%22%20).
# Use "Helvetica" for Arial
fig.font.fam <- "Helvetica"

# Figure dimensions (PDFs only)
# Defaults (7" x 7"):
pdf.wid.def <- 7
pdf.ht.def <- 7

# Actual
pdf.wid.grouped <- pdf.wid.def + 2
pdf.ht.grouped <- pdf.ht.def + 2
pdf.wid.grouped.onerow <- pdf.wid.def + 2
pdf.ht.grouped.onerow <- pdf.ht.def / 1.2

pdf.wid.cm <- 9
pdf.wid.single <- pdf.wid.cm / 1.4
# pdf.wid.grouped <- pdf.wid.cm

# Figure dimensions & resolution (pngs only)
# Defaults:
png.dpi.def <- 72 # DPI (ignored for pdf)
png.wid.def <- 480
png.ht.def <- 480

# Actual (using defaults):
png.dpi <- png.dpi.def
png.wid <- png.wid.def
png.ht <- png.ht.def

# Actual (higher res):
# png.dpi <- 100
# png.wid <- png.wid.def * 2 * png.dpi / png.dpi.def
# png.ht <-  png.ht.def * 2 * png.dpi / png.dpi.def

# Note:
# The png default settings are dpi=72, height=480, width=480.
# To maintain the same scale, you need to multiply height and width
# by the resolution/72.
# See: https://stackoverflow.com/a/72701228/2757825

############################################
# Parameters for development only
# -- Comment out for production
############################################

############################################
# Parameters for testing only
# COMMENT OUT WHEN DONE TESTING!
############################################

# # Reload parameters and functions (development only)
# #source("params.R", local=TRUE)
# source(paste(INCLUDESDIR, "functions.R", sep=''), local=TRUE)

# # Fixed values for testing without loop
# j<- 1   	#curr.lc
# k<-1			#curr.stratum

# ############################################
# # Parameters supplied by calling script
# # COMMENT OUT WHEN DONE TESTING!
# ############################################
# group.strata <- F		# Group figures for strata in single mulitplot?
# plot.bm <- T					# Plot benchmark distribution?
# plot.focal <- T				# Plot focal distribution?
# rescale <- T					# Rescale Y axis to proportion of total counts
# plot.fit <- T					# Overlay distribution curve on histogram
# ############################################

############################################
# END Parameters for testing only
############################################

##########################################################
# Get attributes for current EI (curr.EI)
##########################################################

curr.EI <- as.character(curr.EI) # Just in case this is factor in calling script

##########################################################
# General graphing parameters
##########################################################

text.x <- EI.name # Histogram x axis text
text.y <- "Number of Plots" # Histogram y axis text
alpha.val <- 0.25 # Transparency of overlaps, from 0 to 1

no.bins <- 12 # Number of bins to use for histogram, adjust as needed

# Determine if figure plot requested
plot.figs <- T
if (plot.bm == F && plot.focal == F) plot.figs <- F

# Include model results and overlap on figure when plotting both
# focal and benchmark
plot.mod.results <- T

plot.fit.bak <- plot.fit
if (q.method == "fixed") plot.fit <- F

# Legend position adjustment
# X: positive—move to right, negative—move to left
# Y: negative—move up, positive—move down
# Non-grouped figures
leg.x.extra <- 0.45
leg.y.extra <- NA
leg.x.extra <- -0.1
leg.y.extra <- 0

# Adjustments for grouped figures
leg.x.extra.g <- -0.6
leg.y.extra.g <- 0
# this version move legend up and right
leg.x.extra.g <- -0.32
leg.y.extra.g <- -0.05

# Box background color
box.bg.color <- "white"
box.bg.color <- "transparent"

##########################################################
# Load  data for this EI
##########################################################

# Input file names
rawFileName <- paste(curr.EI, "_raw.csv", sep = "") # raw data
f.results.filename <- paste0(curr.EI, "_focal.csv") # focal pdfs
pdf.f.filename <- paste0(curr.EI, "_pdf.f.csv") # focal pdfs
pdf.b.filename <- paste0(curr.EI, "_pdf.b.csv") # bm pdfs
pdf.xs.filename <- paste0(curr.EI, "_pdf.xs.csv") # x values for both pdfs

# Load raw data, rename columns and order by stratum
rawFile <- paste(RESULTSDIR, rawFileName, sep = "")
dat.ei <- read.csv(rawFile, header = T, stringsAsFactors = FALSE)
names(dat.ei)[names(dat.ei) == "landCover"] <- "landcover"
names(dat.ei)[names(dat.ei) == "vegClass"] <- "bm.vegetation"
dat.ei <- with(dat.ei, dat.ei[order(
  dat.ei$landcover, dat.ei$bm.vegetation,
  dat.ei$focalOrBenchmark, dat.ei$plotCode, dat.ei$stratum
), ])

# Focal results
f.results.file <- paste(RESULTSDIR, f.results.filename, sep = "")
f.results <- read.csv(f.results.file, header = T)

# Focal pfs
pdf.f.file <- paste(RESULTSDIR, pdf.f.filename, sep = "")
pdf.f.df <- read.csv(pdf.f.file, header = T)

# Benchmark pdfs
pdf.b.file <- paste(RESULTSDIR, pdf.b.filename, sep = "")
pdf.b.df <- read.csv(pdf.b.file, header = T)

# pdf x values
pdf.xs.file <- paste(RESULTSDIR, pdf.xs.filename, sep = "")
pdf.xs.df <- read.csv(pdf.xs.file, header = T)

# Hack to fix NAs in beta pdfs. I *believe* all NAs should be infinity
if (distn == "Bet") {
  pdf.f.df[is.na(pdf.f.df)] <- Inf
  pdf.b.df[is.na(pdf.b.df)] <- Inf
}

##########################################################
# Set path to save output figures
#
# This is done after initally loading and inspecting the data in case parameter
# group.strata gets reset
##########################################################

# Base directory name for ungroup figure of combine focal and bm dists
figs.fitted.dir.name <- "dists_fitted"

# Place figures in separate directory if bm only or focal only
if (plot.bm == T && plot.focal == F) {
  figs.fitted.dir.name <- paste(figs.fitted.dir.name, "_bm", sep = "")
} else if (plot.bm == F && plot.focal == T) {
  figs.fitted.dir.name <- paste(figs.fitted.dir.name, "_focal", sep = "")
}

# Grouped figures go in separate directory
if (group.strata == T) {
  figs.fitted.dir.name <- paste(figs.fitted.dir.name, "_grouped", sep = "")
}

# Grouped rescaled figures go in separate directory
if (rescale == T) {
  figs.fitted.dir.name <- paste(figs.fitted.dir.name, "_rescaled", sep = "")
}

# Figures without fit go in separate directory
if (plot.fit == F && no.fit.keep.separate == T) {
  figs.fitted.dir.name <- paste(figs.fitted.dir.name, "_nofit", sep = "")
}

# Assume it's OK to make plots
make.plots <- TRUE

# Check graphing parameter to determine if
# skipping this set of figures
if (figs.fitted.dir.name == "dists_fitted") {
  if (plot.dists_fitted == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_bm") {
  if (plot.dists_fitted_bm == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_bm_grouped") {
  if (plot.dists_fitted_bm_grouped == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_bm_grouped_nofit") {
  if (plot.dists_fitted_bm_grouped_nofit == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_bm_grouped_rescaled") {
  if (plot.dists_fitted_bm_grouped_rescaled == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_bm_grouped_rescaled_nofit") {
  if (plot.dists_fitted_bm_grouped_rescaled_nofit == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_bm_nofit") {
  if (plot.dists_fitted_bm_nofit == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_bm_rescaled") {
  if (plot.dists_fitted_bm_rescaled == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_bm_rescaled_nofit") {
  if (plot.dists_fitted_bm_rescaled_nofit == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_focal") {
  if (plot.dists_fitted_focal == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_focal_grouped") {
  if (plot.dists_fitted_focal_grouped == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_focal_grouped_nofit") {
  if (plot.dists_fitted_focal_grouped_nofit == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_focal_grouped_rescaled") {
  if (plot.dists_fitted_focal_grouped_rescaled == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_focal_grouped_rescaled_nofit") {
  if (plot.dists_fitted_focal_grouped_rescaled_nofit == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_focal_nofit") {
  if (plot.dists_fitted_focal_nofit == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_focal_rescaled") {
  if (plot.dists_fitted_focal_rescaled == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_focal_rescaled_nofit") {
  if (plot.dists_fitted_focal_rescaled_nofit == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_grouped") {
  if (plot.dists_fitted_grouped == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_grouped_nofit") {
  if (plot.dists_fitted_grouped_nofit == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_grouped_rescaled") {
  if (plot.dists_fitted_grouped_rescaled == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_grouped_rescaled_nofit") {
  if (plot.dists_fitted_grouped_rescaled_nofit == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_nofit") {
  if (plot.dists_fitted_nofit == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_rescaled") {
  if (plot.dists_fitted_rescaled == FALSE) make.plots <- FALSE
} else if (figs.fitted.dir.name == "dists_fitted_rescaled_nofit") {
  if (plot.dists_fitted_rescaled_nofit == FALSE) make.plots <- FALSE
}

cat("Saving figures to '", FIGDIR_REL, figs.fitted.dir.name, "/'", sep = "")

if (make.plots == TRUE) {
  cat(":\n") # Close the directory name header and move to new line

  # Create directory if missing
  # dir.create(file.path(figdir.abs, figs.fitted.dir.name), showWarnings = F)
  # figs.fitted.dir <- paste(figs.dir, figs.fitted.dir.name, sep='')
  dir.create(file.path(FIGDIR, figs.fitted.dir.name), showWarnings = F)
  figs.fitted.dir <- paste(FIGDIR, figs.fitted.dir.name, sep = "")

  ##########################################################
  # Loop through vegetation and strata (if applicable), fitting the requested
  # distribution and saving parameters. Plot and save the figures
  ##########################################################

  # get land cover classes
  all.lc <- unique(dat.ei$landcover[dat.ei$focalOrBenchmark == "f"])
  n.lc <- length(all.lc)

  for (j in 1:n.lc) { # START land cover loop
    # for (j in 16:16) {				# Alternative for testing with loop
    curr.lc <- all.lc[j]

    # Make unix-friendly version of land cover code for filename
    lc.fname <- unix.friendly(curr.lc)

    # get all strata for this land cover
    strata <- unique(dat.ei$stratum[dat.ei$landcover == curr.lc])
    n.strata <- length(strata)

    # Reset group.strata if this indicator has been reduced to only one stratum.
    # Would be the case for PCESS if pcess.herbs.only==TRUE
    if (n.strata == 1) group.strata <- FALSE

    # Commenting the following out for now until I sort out the origin of stratum.order

    # Re-order strata if stratum.order has been defined for this EI
    #  	if ( exists("stratum.order") && !stratum.order=="" && !is.na(stratum.order) ) {
    # if ( has.stratum==TRUE && exists("stratum.order") ) {
    #   df.ord <- as.data.frame(stratum.order)
    # 	if ( length( df.ord$stratum ) == n.strata &&
    # 		length( unique( df.ord$stratum ) ) == length( df.ord$stratum ) ) {
    # 			strata <- df.ord[ order(df.ord$ord), c('stratum')]
    # 		}
    # }

    #################################
    # Echo the appropriate par command
    # based on the number of strata
    #################################

    fig.cols <- 1 # Dummy values
    fig.rows <- 1

    # Set figure margins
    # Note positions: c(bottom, left, top, right)
    # For grouped figures, bottom and left must be large enough to display
    # x- and y-axis labels, respectively
    # Also avoid voids stupid RStudio error:
    # "Error in plot.new(): figure margins too large"
    # par(mar=c(1,1,1,1))
    par(mar = c(4, 4, 2, 2))

    if (plot.figs == TRUE) {
      if (n.strata > 1 && group.strata == TRUE) {
        # Group stratum plots in single figure
        dims <- opt.grid(n.strata) # Optimum grid dimensions given # of strata
        fig.cols <- dims[1]
        fig.rows <- dims[2]
        par(
          bg = "white",
          mfrow = c(fig.rows, fig.cols),
          # Outer margins (bottom, left, top, right)
          # oma=c(0, 0, 2, 0)
          # oma=c(3, 3, 3, 3)
          oma = c(4, 4, 4, 3)
        )
      } else {
        par(bg = "white")
      }
    }

    # if (fig.rows>1) {
    #   # Adjust legend position if grouped figure with >1 row
    #   leg.y.extra.g <- -0.07
    # }


    ######################################
    # Set file name of multi plot & initialize graph
    # object if running from Rscript
    ######################################

    if (group.strata == TRUE) { # BEGIN group.strata==TRUE
      # Multi-panel plot: set figure name name outside stratum loop
      graphFileName <- paste(curr.EI, "dists_fitted", lc.fname, sep = "_")
      graphFileNameFull <- paste0(graphFileName, ".", fig.file.type)
      graphFile <- paste(figs.fitted.dir, "/", graphFileNameFull, sep = "")
      # cat(paste0('  ',  graphFile, "\r\n"))
      cat(paste0("  ", graphFileNameFull, "\n"))

      if (interactive() == FALSE) {
        # Start multipanel plot outside stratum loop if running from command line using Rscript
        if (fig.file.type == "png") {
          # png( graphFile )
          png(graphFile, res = png.dpi, height = png.ht, width = png.ht)
        } else if (fig.file.type == "pdf") {
          pdf(graphFile, onefile = TRUE, bg = "white", family = fig.font.fam, width = pdf.wid.grouped)
        } else {
          err.msg <- paste0("ERROR: fig.file.type '", fig.file.type, "' not supported! (see file graph.dists.R)")
          stop(err.msg)
        }
      }

      # Quickly toggle through all strata in advance to see if any will be skipped
      # and determine true dimensions of group figures
      n.k.skipped <- 0 # Number of strata skipped (due failed pdf fits, etc)

      for (k in 1:n.strata) {
        curr.stratum <- strata[k]
        curr.results <- f.results[f.results$landcover == curr.lc & f.results$stratum == curr.stratum, ]

        curr.dat.f <- dat.ei[dat.ei$landcover == curr.lc & dat.ei$stratum == curr.stratum & dat.ei$focalOrBenchmark == "f", ]
        curr.veg <- unique(curr.dat.f$bm.vegetation)
        curr.dat.b <- dat.ei[dat.ei$bm.vegetation == curr.veg & dat.ei$stratum == curr.stratum & dat.ei$focalOrBenchmark == "b", ]

        pdf.f <- pdf.f.df[pdf.f.df$landcover == curr.lc & pdf.f.df$stratum == curr.stratum, ]
        pdf.f <- pdf.f[, -which(names(pdf.f) %in% c("landcover", "stratum", "f.hat"))]
        pdf.f <- as.vector(unlist(pdf.f))

        pdf.b <- pdf.b.df[pdf.b.df$landcover == curr.lc & pdf.b.df$stratum == curr.stratum, ]
        pdf.b <- pdf.b[, -which(names(pdf.b) %in% c("landcover", "stratum", "b.hat"))]
        pdf.b <- as.vector(unlist(pdf.b))

        if (q.method == "empirical" && (length(pdf.f) == 0 || length(pdf.b) == 0)) {
          n.k.skipped <- n.k.skipped + 1
        }
        n.strata.actual <- n.strata - n.k.skipped
      }
    } # END group.strata==TRUE

    n.k.skipped <- 0 # Number of strata skipped (due failed pdf fits, etc)

    for (k in 1:n.strata) { # START strata loop
      # for (k in 1:1) {					# Alternative for testing with loop

      # Get current stratum
      curr.stratum <- strata[k]
      stratum.fname <- unix.friendly(curr.stratum) # Unix-friendly version for filename

      #########################################
      # Extract all data for this EI + land cover + stratum
      #########################################

      # Load df of quality results and summary statistics
      # for this land cover & stratum
      curr.results <- f.results[f.results$landcover == curr.lc & f.results$stratum == curr.stratum, ]

      # Load dfs of raw data
      curr.dat.f <- dat.ei[dat.ei$landcover == curr.lc & dat.ei$stratum == curr.stratum & dat.ei$focalOrBenchmark == "f", ]
      curr.veg <- unique(curr.dat.f$bm.vegetation)
      curr.dat.b <- dat.ei[dat.ei$bm.vegetation == curr.veg & dat.ei$stratum == curr.stratum & dat.ei$focalOrBenchmark == "b", ]

      # Vectors of the raw EI values
      f <- curr.dat.f$EI
      f <- f[!is.na(f)]
      b <- curr.dat.b$EI
      b <- b[!is.na(b)]

      # Vector of raw EI values combined
      ei.all <- c(f, b)

      # Vector of the fitted focal pdf
      pdf.f <- pdf.f.df[pdf.f.df$landcover == curr.lc & pdf.f.df$stratum == curr.stratum, ]
      pdf.f <- pdf.f[, -which(names(pdf.f) %in% c("landcover", "stratum", "f.hat"))]
      pdf.f <- as.vector(unlist(pdf.f))

      # Vector of the fitted bm pdf
      pdf.b <- pdf.b.df[pdf.b.df$landcover == curr.lc & pdf.b.df$stratum == curr.stratum, ]
      pdf.b <- pdf.b[, -which(names(pdf.b) %in% c("landcover", "stratum", "b.hat"))]
      pdf.b <- as.vector(unlist(pdf.b))

      # Vector of x values used to fit the pdfs
      xs <- as.vector(unlist(pdf.xs.df))

      # if either pdf empty, skip to next stratum
      # Ignore (do not skip) if q.method="fixed"
      if (q.method == "empirical" && (length(pdf.f) == 0 || length(pdf.b) == 0)) {
        n.k.skipped <- n.k.skipped + 1
        next
      }

      #########################################
      # Set graph parameters for this bm.vegetation+stratum
      #########################################

      # Sample sizes for figure legend
      n.f <- length(f)
      n.b <- length(b)

      # Overall min and max values, for axis limits
      min.ei <- min(ei.all)
      max.ei <- max(ei.all)

      # Text for focal/benchmark colour legends
      if (show.n == T) {
        # Include sample sizes
        f.text <- paste0("Focal (n=", n.f, ")")
        b.text <- paste0("Benchmark (n=", n.b, ")")
      } else {
        f.text <- "Focal"
        b.text <- "Benchmark"
      }

      # Set color of focal and bm color key
      b.fill <- c(rgb(0, 0, 1, alpha.val))
      f.fill <- c(rgb(1, 0, 0, alpha.val))
      bf.fill <- c(rgb(0, 0, 1, alpha.val), rgb(1, 0, 0, alpha.val))
      border.val <- "black"

      if (LEGEND.NO.FB == FALSE) {
        # Omit focal & bm legend entirely
        f.text <- ""
        b.text <- ""
        b.fill <- NULL
        f.fill <- NULL
        bf.fill <- NULL
        border.val <- "white"
      }

      # Set default main title
      text.main <- curr.lc

      # Adjust main title if needed
      if (group.strata == FALSE) {
        text.main <- curr.lc

        if (!curr.stratum == "nostrata" && !curr.stratum == "") {
          # Adjust title if this is an indicator group with strata

          # Extra steps required to final parameter value because
          # original parameter 'lc.in.title' may not exist for
          # some indicators
          if (!exists("lc.in.title")) {
            ti.stratum.only <- FALSE
          } else {
            if (lc.in.title == FALSE) {
              ti.stratum.only <- TRUE
            } else {
              ti.stratum.only <- FALSE
            }
          }

          if (ti.stratum.only == TRUE) {
            # Change title to name of stratum
            text.main <- curr.stratum
          } else {
            # Combine land cover and stratum in title
            text.main <- paste0(curr.lc, ":  ", curr.stratum)

            # Split title over 2 lines if combined title is too long
            ti.length <- nchar(text.main)
            if (ti.length > 45) text.main <- paste0(curr.lc, ":\n", curr.stratum)
          }
        }
      } else {
        text.main <- curr.stratum
      }

      # Custom text for specific EIs, as needed
      if (curr.EI %in% c("PCGF", "GC", "PCS")) {
        if (group.strata == T) {
          text.x <- paste("Percent Cover", sep = "")
        } else {
          text.x <- paste("Percent Cover ", curr.stratum, sep = "")
        }
        # } else if (curr.EI == 'GC' ) {
        # 	if (group.strata==T) {
        # 		text.x <- paste('Percent Cover ', sep='')
        # 	} else {
        # 		text.x <- paste('Percent Cover ', curr.stratum, sep='')
        # 	}
      } else if (curr.EI == "PCESS") {
        if (group.strata == T) {
          text.x <- paste("Percent Cover Exotic Species", sep = "")
        } else {
          text.x <- paste("Percent Cover Exotic Species (", curr.stratum, ")", sep = "")
        }
      } else if (curr.EI == "asc" || curr.EI == "ASC") {
        text.x <- paste0("Number of individuals")
      }

      # Some distribution-specific parameters
      if (distn == "Bet") {
        # x axis limits
        xlim.min <- 0
        xlim.max <- 1

        # y axis limits
        ysize <- sum(ei.all) * 1.1
        ylim.min <- 0
        ylim.max <- ysize

        # Histogram bins
        obs.x <- seq(0, 1, length.out = no.bins)
      } else if (distn == "NBin") {
        # x axis limits
        xlim.min <- 0
        xlim.max <- max.ei + (max.ei * 0.1)

        # y axis limits
        ysize <- sum(ei.all) * 1.1
        ylim.min <- 0
        ylim.max <- ysize

        # Histogram bins
        obs.x <- seq(0, max.ei + (max.ei * 0.1), length.out = no.bins)

        # Prepare manual x-axis units if max.ei < n.bins
        if (exists("x.ticks")) rm(x.ticks) # Clear previous values if any
        if (max.ei < no.bins) x.ticks <- seq(0, max.ei + 1)
      } else if (distn == "gamma") {
        # x axis limits
        xlim.min <- 0
        xlim.max <- max.ei + (max.ei * 0.1)

        # y axis lower limit
        # Upper limit set below after running hist() to determine bins
        ysize <- sum(ei.all) * 1.1 # was ysize<-sum(curr.dat $EI)*1.1
        ylim.min <- 0
        ylim.max <- NA

        # Histogram bins
        obs.x <- seq(0, xlim.max, length.out = no.bins)
      }

      # Get y axis upper limit by generating histograms
      # without plotting
      h.b <- hist(b, breaks = obs.x, plot = FALSE)
      h.f <- hist(f, breaks = obs.x, plot = FALSE)

      # y upper limit is bin with highest count
      highestCount <- max(h.b$counts, h.f$counts)
      if (plot.focal == F) {
        highestCount <- max(h.b$counts) # Use bm values only
      } else if (plot.bm == F) {
        highestCount <- max(h.f$counts) # Use focal values only
      }
      ylim.max <- highestCount + (highestCount * 0.4)

      # Rescale if requested
      freq.val <- T
      if (rescale == T) {
        h.b$density <- h.b$counts / sum(h.b$counts)
        h.f$density <- h.f$counts / sum(h.f$counts)
        freq.val <- F
        text.y <- "Proportion of plots"

        # y upper limit is bin with highest density
        highestDensity <- max(h.b$density, h.f$density)
        if (plot.focal == F) {
          highestDensity <- max(h.b$density) # Use bm values only
        } else if (plot.bm == F) {
          highestDensity <- max(h.f$density) # Use focal values only
        }
        ylim.max <- min(1, highestDensity + 0.1)

        # Rescale fitted distribution density
        if (length(pdf.b) > 0) pdf.b <- pdf.b / max(pdf.b[pdf.b < Inf & !is.na(pdf.b)]) * max(h.b$density) * 0.90
        if (length(pdf.f) > 0) pdf.f <- pdf.f / max(pdf.f[pdf.f < Inf & !is.na(pdf.f)]) * max(h.f$density) * 0.90
      } else {
        # Adjust fitted curve to scale of raw data
        if (length(pdf.b) > 0) pdf.b <- pdf.b / max(pdf.b[pdf.b < Inf & !is.na(pdf.b)]) * max(h.b$counts) * 0.90
        if (length(pdf.f) > 0) pdf.f <- pdf.f / max(pdf.f[pdf.f < Inf & !is.na(pdf.f)]) * max(h.f$counts) * 0.90
      }

      if (has.stratum == T && group.strata == T && n.strata > 1) {
        # Suppress x and y axis labels to reduce clutter
        del.y <- FALSE
        del.x <- FALSE
        k.actual <- k - n.k.skipped
        # n.strata.actual <- n.strata - n.k.skipped

        # # Suppress y axis label for grouped figures
        # first.col <- 1 + ( fig.cols * rep(0:(fig.rows-1) )  )
        # if ( ! ( k.actual %in% first.col) ) del.y <- TRUE
        #
        # # Suppress x axis label for grouped figures
        # last.row <- ( fig.cols * fig.rows ) - ( rep(0:(fig.cols-1) ) )
        # if ( ! ( k.actual %in% last.row ) ) del.x <- TRUE

        # A few hard-wired adjustments
        if (n.strata.actual == 4) {
          del.x <- FALSE
          if (k.actual %in% c(1, 2)) del.x <- TRUE
        } else if (n.strata.actual == 3) {
          del.x <- FALSE
          if (k.actual == 1) del.x <- TRUE
        } else if (n.strata.actual == 2) {
          del.x <- FALSE
        } # Add more here as needed

        if (del.y == TRUE) text.y <- ""
        if (del.x == TRUE) text.x <- ""
        # cat("EI: ", curr.EI, ", landCover: ", curr.lc, ", stratum: ", curr.stratum, ",
        # n.strata.actual=", n.strata.actual, ", k=", k, ", k.actual=", k.actual, ", text.x='",
        # text.x, "'\n", sep="")
      }

      ###########################################
      # Upper left legend text
      # Includes means test results & quality. Do only if
      # processing both focal, strata ungrouped
      ###########################################
      ull.text <- ""

      # if ( plot.focal==T && group.strata==F && plot.mod.results==T )
      if (plot.focal == T && plot.mod.results == T) { # START Upper-left legend text

        # Get results
        perc.diff <- NA
        perc.diff.text <- NA
        p.diff <- curr.results$p.diff
        if (is.null(p.diff)) p.diff <- NA # Set to NA instead to allow boolean tests
        f.mean <- curr.results$f.mean
        b.mean <- curr.results$b.mean
        if (!is.na(p.diff)) {
          perc.diff <- f.mean / b.mean
          perc.diff.text <- as.character(round(perc.diff, digits = 2))
        }
        ol <- curr.results$ol
        ol.lcl <- curr.results$ol.lcl
        ol.ucl <- curr.results$ol.ucl
        q <- curr.results$q
        q.lcl <- curr.results$q.lcl
        q.ucl <- curr.results$q.ucl

        if (f.mean > b.mean) {
          f.diff <- "greater"
        } else if (f.mean < b.mean) {
          f.diff <- "less"
        } else {
          f.diff <- "same"
        }

        ####################
        # Model results text
        ####################
        result.text <- ""
        if (!is.na(p.diff) && !is.na(perc.diff)) { # START result.text

          # Means test results text
          if (p.diff < 0.001) {
            if (f.diff == "greater") {
              f.to.b.text <- "Focal > Benchmark"
            } else if (f.diff == "less") {
              f.to.b.text <- "Focal < Benchmark"
            }
            result.text <- paste(f.to.b.text, " (p<0.001)", sep = "")
          } else {
            p.val.mod.text <- as.character(specify_decimal(p.diff, 3))
            if (p.diff >= 0.05) {
              result.text <- paste("Focal = Benchmark (p>0.05)", sep = "")
            } else {
              if (f.diff == "greater") {
                f.to.b.text <- "Focal > Benchmark"
              } else if (f.diff == "less") {
                f.to.b.text <- "Focal < Benchmark"
              }
              result.text <- paste(f.to.b.text, " (p=",
                p.val.mod.text, ")",
                sep = ""
              )
            }
          }
        } # END result.text

        ############################
        # Quality legend text
        ############################
        if (is.na(q)) {
          q.text <- ""
        } else {
          if (Q.LEGEND.PERCENT == TRUE) {
            # Format quality as percent
            q.fixed <- as.character(specify_decimal(q * 100, 1))
            q.text <- paste0("Q: ", q.fixed, "%")

            if (!(q.lcl == 1 && q.ucl == 1) && !is.na(q.lcl) && !is.na(q.ucl)) {
              # Add CLs
              q.lcl.fixed <- as.character(specify_decimal(q.lcl * 100, 1))
              q.ucl.fixed <- as.character(specify_decimal(q.ucl * 100, 1))
              q.text <- paste0(q.text, " [", q.lcl.fixed, ", ", q.ucl.fixed, "%]")
            }
          } else {
            # Keep quality a proportion
            q.fixed <- as.character(specify_decimal(q, 2))
            q.text <- paste0("Q: ", q.fixed)

            if (!(q.lcl == 1 && q.ucl == 1) && !is.na(q.lcl) && !is.na(q.ucl)) {
              # Set q.lcl decimal places
              if (q.lcl < 0.01) {
                q.lcl.fixed <- as.character(specify_decimal(q.lcl, 3))
              } else {
                q.lcl.fixed <- as.character(specify_decimal(q.lcl, 2))
              }

              # Set q.ucl decimal places
              if (q.ucl < 0.01) {
                q.ucl.fixed <- as.character(specify_decimal(q.ucl, 3))
              } else {
                q.ucl.fixed <- as.character(specify_decimal(q.ucl, 2))
              }

              # Compose complete quality text
              q.text <- paste0(q.text, " [", q.lcl.fixed, ", ", q.ucl.fixed, "]")
            }
          }
        }

        ############################
        # Overlap legend text
        ############################
        if (is.na(ol) || show.overlap == F) {
          ol.text <- ""
        } else {
          # Report overlap as percent
          ol.fixed <- as.character(specify_decimal(ol * 100, 0))

          if (test.tail == "upper") {
            ol.text <- paste0("Overlap (f<b, upper tail only):", ol.fixed, "%")
          } else if (test.tail == "lower") {
            ol.text <- paste0("Overlap (f<b, lower tail only): ", ol.fixed, "%")
          } else {
            # Both tails
            ol.text <- paste0("Overlap: ", ol.fixed, "%")
          }

          if (!(ol.lcl == 1 && ol.ucl == 1) && !is.na(ol.lcl) && !is.na(ol.ucl)) {
            # Add CLs
            ol.lcl.fixed <- as.character(specify_decimal(ol.lcl * 100, 1))
            ol.ucl.fixed <- as.character(specify_decimal(ol.ucl * 100, 1))
            ol.text <- paste0(ol.text, " [", ol.lcl.fixed, ", ", ol.ucl.fixed, "%]")
          }
        }

        # Combine final upper left legend text
        if (q.method == "fixed") {
          if (beta.algorithm == "overlap") {
            ull.text <- paste0(q.text)
          } else {
            ull.text <- paste0(result.text, "\n", q.text)
          }
        } else if (beta.algorithm == "overlap") {
          # Overlap quality only
          ull.text <- q.text
        } else {
          if (ol.text == "") {
            ull.text <- paste0(result.text, "\n", q.text)
          } else {
            ull.text <- paste0(result.text, "\n", ol.text, "\n", q.text)
          }
        }

        if (quality.legend.only == TRUE) ull.text <- q.text
      } # END  Upper-left legend text

      ######################################
      # Set file name of single plot & initialize graph
      # object if running from Rscript
      ######################################

      if (group.strata == FALSE) {
        # Set single plot file name inside stratum loop
        if (curr.stratum == "nostrata") {
          graphFileName <- paste(curr.EI, "dists_fitted", lc.fname, sep = "_")
        } else {
          graphFileName <- paste(curr.EI, "dists_fitted", lc.fname, stratum.fname, sep = "_")
        }

        graphFileNameFull <- paste0(graphFileName, ".", fig.file.type)
        graphFile <- paste(figs.fitted.dir, "/", graphFileNameFull, sep = "")
        cat(paste0("  ", graphFileNameFull, "\r\n"))

        if (interactive() == FALSE) {
          # Start printing plot before calling plot objects if running from Rscript command line
          # Do this inside stratum loop for single plot
          if (do.debug == TRUE) cat("  DEBUG: Printing single plot from Rscript command line...\n") # Debugging

          if (fig.file.type == "png") {
            # png( graphFile )
            png(graphFile, res = png.dpi, height = png.ht, width = png.ht)
          } else if (fig.file.type == "pdf") {
            pdf(graphFile, onefile = TRUE, bg = "white", family = fig.font.fam, width = pdf.wid.single)
          } else {
            err.msg <- paste0("ERROR: fig.file.type '", fig.file.type, "' not supported! (see file graph.dists.R)")
            stop(err.msg)
          }
        }
      }

      ######################################
      # Plot the graph

      ######################################

      # # Bm histogram
      if (plot.bm == T) {
        if (exists("x.ticks")) {
          # Plot x axis manually
          plot(h.b,
            main = text.main,
            font.main = 1, # Plain text title; default is bold for titles
            xlab = text.x,
            ylab = text.y,
            col = rgb(0, 0, 1, alpha.val),
            ylim = c(ylim.min, ylim.max),
            freq = freq.val,
            xaxt = "n"
          )
          axis(1,
            at = x.ticks,
            labels = x.ticks,
            las = 0
          )
        } else {
          # Use built-in x axis
          plot(h.b,
            main = text.main,
            font.main = 1, # Plain text title; default is bold for titles
            xlab = text.x,
            ylab = text.y,
            col = rgb(0, 0, 1, alpha.val),
            xlim = c(xlim.min, xlim.max),
            ylim = c(ylim.min, ylim.max),
            freq = freq.val
          )
        }
      }

      # Focal histogram
      if (plot.focal == T) {
        add.val <- T
        if (plot.bm == F) add.val <- F

        if (exists("x.ticks")) {
          # Plot x axis manually
          plot(h.f,
            main = text.main,
            font.main = 1, # Plain text title; default is bold for titles
            xlab = text.x,
            ylab = text.y,
            col = rgb(1, 0, 0, alpha.val),
            ylim = c(ylim.min, ylim.max),
            freq = freq.val,
            xaxt = "n",
            add = add.val
          )
          axis(1,
            at = x.ticks,
            labels = x.ticks,
            las = 0
          )
        } else {
          # Use built-in x axis

          plot(h.f,
            main = text.main,
            font.main = 1, # Plain text title; default is bold for titles
            xlab = text.x,
            ylab = text.y,
            col = rgb(1, 0, 0, alpha.val),
            xlim = c(xlim.min, xlim.max),
            ylim = c(ylim.min, ylim.max),
            freq = freq.val,
            add = add.val
          )
        }
      }

      # Add vertical line indicating fixed benchmark, if applicable
      if (q.method == "fixed" && !is.na(bm.val)) {
        abline(v = bm.val, col = "black", lwd = 3, lty = 2)

        # # Add label to line
        # bm.val.text <- paste0("Benchmark=", bm.val)
        # x.pos <- ( xlim.min + ( (xlim.max-xlim.min) * 0.15 ) )
        # y.pos <- ylim.max
        # text(
        # x= x.pos,
        # y= y.pos,
        # labels=bm.val.text
        # )
      }

      # The pdf fit lines
      if (plot.fit == T) { # START Add fit lines

        # Bm pdf
        if (plot.bm == T) {
          curve1 <- lines(xs, pdf.b,
            lwd = 2,
            col = rgb(0, 0, 1, alpha.val)
          )
        }

        # Focal pdf
        if (plot.focal == T) {
          curve2 <- lines(xs, pdf.f,
            lwd = 2,
            col = rgb(1, 0, 0, alpha.val)
          )
        }
      } # END The pdf fit lines

      # Print figure legends
      if (plot.legends == T) {
        # Legend placements
        inset.default <- c(0.1, 0) # Default
        inset.f.default <- c(0.17, 0.07) # Default, f only
        inset.qual.comb <- c(0.55, 0) # Quality legend, combined with f/b
        inset.beta <- c(0.58, 0.07) # Beta indicator legend
        inset.group <- c(0, 0) # Group (multi-panel) figures

        if (group.strata == TRUE) {
          inset.default <- c(0.4, -0.05) # Default
          inset.f.default <- c(0.4, -0.05) # Default, f only
          inset.qual.comb <- c(0.4, -0.05) # Quality legend, combined with f/b
          inset.beta <- c(0.45, 0.1) # Beta indicator legend
          inset.group <- c(0.4, -0.05) # Group (multi-panel) figures
        }

        # Custom adjustments, as set in indicator script
        if (group.strata == FALSE) {
          # Single graph
          if (exists("leg.x.extra") && is.num(leg.x.extra)) {
            inset.default[1] <- inset.default[1] + leg.x.extra
            inset.f.default[1] <- inset.f.default[1] + leg.x.extra
            inset.qual.comb[1] <- inset.qual.comb[1] + leg.x.extra
            inset.beta[1] <- inset.beta[1] + leg.x.extra
          }
          if (exists("leg.y.extra") && is.num(leg.y.extra)) {
            inset.default[2] <- inset.default[2] + leg.y.extra
            inset.f.default[2] <- inset.f.default[2] + leg.x.extra
            inset.qual.comb[2] <- inset.qual.comb[2] + leg.y.extra
            inset.beta[2] <- inset.beta[2] + leg.y.extra
          }
        } else {
          # Group graph
          if (group.strata == T && exists("leg.x.extra.g") && is.num(leg.x.extra.g)) {
            inset.default[1] <- inset.default[1] + leg.x.extra.g
            inset.f.default[1] <- inset.f.default[1] + leg.x.extra.g
            inset.qual.comb[1] <- inset.qual.comb[1] + leg.x.extra.g
            inset.beta[1] <- inset.beta[1] + leg.x.extra.g
          }
          if (group.strata == T && exists("leg.y.extra.g") && is.num(leg.y.extra.g)) {
            inset.default[2] <- inset.default[2] + leg.y.extra.g
            inset.f.default[2] <- inset.f.default[2] + leg.y.extra.g
            inset.qual.comb[2] <- inset.qual.comb[2] + leg.y.extra.g
            inset.beta[2] <- inset.beta[2] + leg.y.extra.g
          }
        }

        # Ignore all previous values if group figure
        # if (group.strata==T) inset.default <- inset.group

        # Print f/b colour legend (+optional n)
        if (
          (group.strata == F) ||
            (group.strata == T && k == fig.cols) ||
            (group.strata == T && group.strata.print.all.n == TRUE)
        ) {
          # Applies to grouped strata only if printing upper right figure

          # Set f, b or f/b combined
          if (plot.focal == T && plot.bm == T) {
            # Legend both f & b on same graph
            if (beta.algorithm == "overlap" || legend.combined == TRUE) {
              legend("topleft",
                legend = c(b.text, f.text),
                inset = inset.beta,
                fill = bf.fill,
                border = border.val,
                box.lty = 0
              )
            } else {
              legend("topright",
                legend = c(b.text, f.text),
                inset = inset.default,
                fill = bf.fill,
                border = border.val,
                box.lty = 0
              )
            }
          } else if (plot.focal == T && plot.bm == F) {
            # Legend focal only
            legend("topright",
              inset = inset.f.default,
              legend = c(f.text),
              fill = f, fill,
              border = border.val,
              box.lty = 0
            )
          } else { # plot.focal==F && plot.bm==T
            # Legend b only
            legend("topright",
              inset = inset.default,
              legend = c(b.text),
              fill = b.fill,
              border = border.val,
              box.lty = 0
            )
          } # END Set f, b or f/b combined
        } # END  Print f/b colour legend (+optional n)

        # Print quality legend
        # Only do if graph includes focal data with strata ungrouped
        if (plot.focal == T && plot.mod.results == T) {
          if (legend.combined == TRUE) {
            # Quality legends of grouped figures
            legend("topleft",
              inset = inset.qual.comb,
              box.lty = 0, # Comment out to add box for troubleshooting
              x.intersp = -0.5, # Move text horizontally within legend box
              bg = box.bg.color, # Box background
              xjust = 0, # Right-justify text in legend box
              legend = ull.text
              # legend="Hello World!" # For troubleshooting
            )
          } else {
            legend("topleft",
              legend = ull.text,
              # legend="Hello World!", # For troubleshooting
              inset = inset.default,
              box.lty = 0 # Comment out to add box for troubleshooting
            )
          }
        } # END Print quality legend
      } # END Print figure legends

      # Save single plot
      if (group.strata == FALSE) {
        if (interactive() == TRUE) {
          # Save plot now if running in R Studio or R console
          if (do.debug == TRUE) cat("  DEBUG: Printing single plot from R Studio...\n") # Debugging

          if (fig.file.type == "png") {
            dev.copy(png, graphFile)
          } else if (fig.file.type == "pdf") {
            dev.copy(pdf, graphFile)
          }
        } # END interactive

        # } else if ( interactive()==FALSE ) {
        #   # Start single plot inside stratum loop if running from Rscript command line
        #   #png(graphFile)
        #   if ( do.debug==TRUE ) cat("  DEBUG: Printing single plot from Rscript command line...\n")  # Debugging
        #
        #   if ( fig.file.type=="png" ) {
        #     png( graphFile )
        #   } else if ( fig.file.type=="pdf" ) {
        #     pdf( graphFile, onefile=TRUE, bg = "white", family = fig.font.fam, width = fig.width.single )
        #   } else {
        #     err.msg <- paste0("ERROR: fig.file.type '", fig.file.type, "' not supported! (see file graph.dists.R)")
        #     stop(err.msg)
        #   }
        # }

        invisible(dev.off())
      }
    } # End strata loop

    # Save multiplot
    if (group.strata == TRUE) {
      ei.name.alt <- gsub("Percent Cover", "Cover", ei.name, ignore.case = TRUE)
      ti.group <- paste0(ei.name.alt, " — ", curr.lc)

      # Save plot now if running in R Studio or R console
      if (interactive() == TRUE) {
        # Version for printing from R Studio or R app environments
        if (do.debug == TRUE) cat("  DEBUG: Printing multipanel plot from R Studio...\n") # Debugging

        # Add group title to multiplot
        # GROUP.MTEXT.LINE sets vertical placement of group title
        # mtext(curr.lc, side=3, line=GROUP.MTEXT.LINE, cex=1, font=2, outer=TRUE )
        mtext(ti.group, side = 3, line = GROUP.MTEXT.LINE, cex = 1, font = 2, outer = TRUE)
        # mtext("Hello world!", side=3, line=GROUP.MTEXT.LINE, cex=1.5, outer=TRUE )

        # Adjust dimensions if multiplot consists of only one row
        if (fig.rows == 1) {
          if (fig.file.type == "png") {
            dev.copy(png, graphFile, width = 800, height = 500)
          } else if (fig.file.type == "pdf") {
            # TITLE <- paste0("n.strata=", n.strata, ", Rows=", fig.rows)
            # mtext(TITLE, side=3, line=GROUP.MTEXT.LINE, cex=1.5, outer=TRUE )

            # dev.copy(pdf, graphFile)
            dev.copy(pdf, graphFile, width = pdf.wid.grouped.onerow, height = pdf.ht.grouped.onerow)
          }
        } else {
          # Group figure with >1 row
          if (fig.file.type == "png") {
            dev.copy(png, graphFile)
          } else if (fig.file.type == "pdf") {
            # TITLE <- paste0("n.strata=", n.strata, ", Rows=", fig.rows)
            # mtext(TITLE, side=3, line=GROUP.MTEXT.LINE, cex=1.5, outer=TRUE )

            # dev.copy(pdf, graphFile)
            dev.copy(pdf, graphFile, width = pdf.wid.grouped, height = pdf.ht.grouped)
          }
        }
      } else { # interactive()==FALSE
        # Version for printing from command line in Rscript
        # if ( do.debug==TRUE ) cat("  DEBUG: Printing multipanel plot from Rscript!!!\n", sep="")  # Debugging

        if (fig.file.type == "png") {
          # png( graphFile )
          png(graphFile, res = png.dpi, height = png.ht, width = png.ht)
        } else if (fig.file.type == "pdf") {
          pdf(graphFile, onefile = TRUE, bg = "white", family = fig.font.fam, width = pdf.wid.grouped)
        } else {
          err.msg <- paste0("ERROR: fig.file.type '", fig.file.type, "' not supported! (see file graph.dists.R)")
          stop(err.msg)
        }
      } # END if interactive()

      invisible(dev.off())
    }
  } # end land cover loop

  if (plot.figs == F) Print("WARNING: No figures printed (plot.figs==FALSE)")

  plot.fit <- plot.fit.bak
} else {
  # make.plots==FALSE
  cat("...skipping\n")
}

########### END SCRIPT #############

######################################################
# Plots results of VQA posthoc power analysis, for single land cover class
# at multiple effect sizes.
#
# Author: Brad Boyle (bboyle@email.arizona.edu)
# First release: 20 Jul 2017
#
# Requires:
# 		input file power.single.ei.results.csv
######################################################

rm(list = ls())

################ #######
# Load libraries and functions
#######################

# Load required packages
library(drc) # For fitting Michaelis-Menten curve
library(ggplot2)

# Set working directory
# Must set first to set directory parameters
wd <- getwd()
setwd(wd)
wd

# Load global params file if not already loaded
if (!exists("global.params.loaded", mode = "function")) source("params.R")

# get global parameters & functions
# source(paste0(INCLUDESDIR, "functions.R", local=TRUE))
source("includes/functions.R", local = TRUE)

# Create power figures directory if missing
dir.create(file.path(FIGDIR, "power"), showWarnings = F)

# Power analysis results to plot (input file)
results.file <- paste0(RESULTSDIR, "power/power.csv")
# results.file <- paste0(RESULTSDIR, "power/power.0.1-0.15.bio.ind.csv")
# results.file <- paste0(RESULTSDIR, "power/power.0.1-0.15.all.ind.csv")
# results.file <- paste0(RESULTSDIR, "power/power.0.05-0.1.bio.ind.csv")
results.file <- paste0(RESULTSDIR, "power/power.noGC.all.csv")
results.file <- paste0(RESULTSDIR, "power/power.with.GC.all.es.comb.csv")
results.file <- paste0(RESULTSDIR, "power/power.0.1-0.15.all.ind.csv")

# Figure file base name (output file)
# Set to NULL to use default name: "power_" + land.cover.class
# Do not include .png file extension
fig.file.basename <- NULL

#######################
# Parameters
#######################

# Target power for predicting minimum sample size
# Recommend conventional power of 0.80
pwr.target <- 0.8

# Integer amount of jitter to add to x and y in graph. Set to 0 or NULL for no jitter
jitter.val <- 0

# Model parameters
save.fig <- TRUE # Save figure
replace.fig <- TRUE # Replace existing figure for this ei+veg+stratum+e.size

# Set to TRUE to recalculate fit and n.min
fit.recalc <- FALSE

# Set to TRUE to save recalculated fit and n.min
# Only applies if fit.recalc==TRUE
fit.save <- FALSE

# Omit title at top of figure?
title.omit <- TRUE

# Center the main title?
# TRUE | FALSE
# Default = FALSE = NULL
title.main.center <- NULL
title.main.center <- TRUE

# Vector of color codes to apply to lines and legend
# Must be valid ggplot color codes
# Vector must have (at least) as many elements as
# there are effect sizes in the results file
# Set to NULL to use default colors
color.codes.manual <- NULL
color.codes.manual <- as.character(c("red", "royalblue1", "seagreen", "salmon"))
color.codes.manual <- as.character(c("seagreen", "red", "royalblue1", "salmon"))
color.codes.manual <- as.character(c("red", "seagreen", "royalblue1", "salmon"))

# Reverse the default sort order of legend
# TRUE | FALSE (default)
# NULL same as FALSE
reverse.legend <- TRUE

# Horizontal displacement of n.min label to right of n.min line
# Integer. Set to NULL to use default of 4
n.min.txt.pos.x.increment <- NULL
n.min.txt.pos.x.increment <- 1.5
# n.min.txt.pos.x.increment=3

# Vertical displacement of n.min label relative to
# default position, in increments of y. Can be negative.
# Set to NULL to use default
n.min.txt.pos.y.increment <- NULL
n.min.txt.pos.y.increment <- 0.1
# n.min.txt.pos.y.increment <- 0

# Change in horizontal placement of power line label, relative
# to default, in increments of x (=n.min). Can be negative.
# Set to NULL or 0 to use default.
p.text.pos.x.increment <- NULL
p.text.pos.x.increment <- -4
p.text.pos.x.increment <- 3

################################
# Confirm operation
################################

cat("\nPlot vqa.power results with following settings:\n")
cat("  Script to run (this one): vqa.power.plot.R\n", sep = "")
cat("  Recalculate fit: ", fit.recalc, "\n", sep = "")
cat("  Input file: ", results.file, "\n", sep = "")

msg.conf <- "Continue? (y/n):"
yes <- c("y", "Y", "Yes", "yes")

if (interactive()) {
  response <- readline(msg.conf)
} else {
  cat(msg.conf)
  response <- readLines("stdin", n = 1)
}

if (!response %in% yes) stop_quietly("Operation cancelled\n\n")

#################################
# Load the results & extract values for
# current veg & EI
#################################

results <- read.csv(results.file, header = T, stringsAsFactors = FALSE)

# Extract all land cover classes
veggies <- results[, c("landcover")]
veggies <- unique(veggies)
n.veg <- length(veggies)

# Starting message
br <- "\n"
h.line <- "------------------------------------------------"
main.title <- "Plotting VQA Power Simulation Results"
cat(paste0(h.line, br, main.title, br))

# for (x in 1:n.veg) {		# START veg loop
for (x in 1:1) { # FOR TESTING

  curr.veg <- veggies[x]
  full.veg <- trim(paste0(curr.veg)) # In case I decide to modify

  # Get the results df
  dat <- results[results$landcover == curr.veg, ]

  # Echo current land cover
  cat(paste0("  Plotting: ", full.veg))
  flush.console()

  #######################
  # Recalculate & save fit & n.min if requested
  #######################

  if (fit.recalc == TRUE) {
    e.sizes <- unique(dat[, c("e.size")])
    n.e.sizes <- length(e.sizes)

    for (es in 1:length(e.sizes)) { # START es loop
      curr.es <- e.sizes[es]
      dat.es <- dat[dat$e.size == curr.es, ]
      fit <- power.logistic(df.pwr = dat.es, pwr.target = pwr.target)
      n.max <- max(dat.es$n)

      if (!fit[1] == "fail") {
        n.min.target <- unique(fit$n.min)

        # Hack to control for bug that returns maximum value of x
        # over submitted x values (x.obs) if predicted value of x (x.pred)
        # is outside domain of x.obs
        if (is.null(n.min.target)) {
          n.min.target <- NA
        } else if (is.na(n.min.target)) {
          n.min.target <- NA
        } else if (n.min.target >= n.max) {
          n.min.target <- 99999
        }
        n.min.target <- rep(n.min.target, nrow(dat.es))

        dat$n.min[dat$e.size == curr.es] <- n.min.target

        # Set fit values >1 to 1 to allow graphing function
        # to plot complete asymptote
        fit$pwr.fit[fit$pwr.fit > 1] <- 1
        dat$fit[dat$e.size == curr.es] <- fit$pwr.fit

        if (fit.save == TRUE) {
          # Save the revised calculations
          results[results$landcover == curr.veg & results$e.size == curr.es, c("fit")] <- fit$pwr.fit
          results[results$landcover == curr.veg & results$e.size == curr.es, c("n.min")] <- n.min.target
          write.csv(results, file = results.file, row.names = FALSE)
        } # END if ( fit.save==TRUE )
      } # END if ( !fit[1]=='fail' )
    } # END es loop
  } # END if ( fit.recalc==TRUE )

  #######################
  # Plot the results & save figure
  #######################

  # Set plot parameters
  if (title.omit == TRUE) {
    main.text <- ""
  } else {
    main.text <- full.veg
  }

  x.text <- "Number of plots (n)"
  y.text <- "Power"

  pwr.target <- unique(dat$pwr.target)

  p <- plot.power.logistic.multi.ei(
    df.pwr = dat,
    title.main = main.text, title.main.center = title.main.center,
    x.title = x.text, y.title = y.text, jitter.val = jitter.val,
    n.min.txt.pos.x.increment = n.min.txt.pos.x.increment,
    n.min.txt.pos.y.increment = n.min.txt.pos.y.increment,
    p.text.pos.x.increment = p.text.pos.x.increment,
    color.codes.manual = color.codes.manual, reverse.legend = reverse.legend
  )

  # print the graph
  dev.new(width = 7, height = 6)
  print(p)

  # Make unix-friendly file name
  if (is.null(fig.file.basename)) {
    # Compose output file name if none supplied
    curr.veg.fname <- unix.friendly(curr.veg)
    fig.file.name <- paste("power", curr.veg.fname, sep = "_")
  }

  cat("\n curr.veg ='", curr.veg, "'\n", sep = "")
  cat("\n curr.veg.fname ='", curr.veg.fname, "'\n", sep = "")

  #####################
  # Save the figure
  #####################

  fig.file <- paste0(FIGDIR, "power/", fig.file.name, ".png")

  if (replace.fig == F) {
    # If file already exists, change name so as not to replace
    # Up to maximum of 11 versions of file
    for (i in 1:10) {
      if (!file.exists(fig.file)) {
        break
      } else {
        fig.file <- paste0(FIGDIR, "power/", fig.file.name, "_", i, ".png")
      }
    }
  }

  if (save.fig == T) {
    dev.copy(png, fig.file,
      width = 175, height = 150, units = "mm", res = 300
    )
  }
  dev.off()
} # END veg loop ###

cat("Run completed\n")

######################################################
# Plots results of VQA posthoc power analysis, for single vegetation and
# indicator (EI).
#
# Details:
# 		Plots simultaneously for all effect sizes. If multiple strata, plots each
# 		stratum separately. Fits curves using logistic function.
#
# Requires:
# 		Input file:		power.single.ei.results.csv (run vqa.power.ei.R first
# 							to generate this file)
#
# Author: Brad Boyle (bboyle@email.arizona.edu)
# First release: 20 Jul 2017
######################################################

rm(list = ls())

# Critical!
# Determines data directories and params file used
# Params file used=paste0("params.", proj, ".R")
# Assessment code and everything else are set in the params file
proj <- "vqa-pub"

#######################
# Load libraries, functions,
# external parameters & paths
######################

# Load required packages
library(drc) # For fitting Michaelis-Menten curve
library(ggplot2)

# Set working directory
wd <- getwd()
setwd(wd)
wd

# # Load global params file if not already loaded
params.file <- paste0("params.", proj, ".R")
source(params.file)

# Load general functions
source(paste0(INCLUDESDIR, "functions.R"), local = TRUE)

# Create power figures directory if missing
dir.create(file.path(FIGDIR, "power"), showWarnings = F)
pwr.figs.dir <- paste0(FIGDIR, "power/")

#######################
# Parameter options
#######################

# Vegetation you wish to plot
curr.veg <- "Alpine"
curr.veg <- "Alpine dwarf shrub"
curr.veg <- "Avalanche feature"
curr.veg <- "Brushland/Grassland"
curr.veg <- "Dry forest, early-mid"
curr.veg <- "Dry forest, mature"
curr.veg <- "Dry forest, old"
curr.veg <- "Intermediate forest, early-mid"
curr.veg <- "Intermediate forest, mature"
curr.veg <- "Intermediate forest, old"
curr.veg <- "Krummholz"
curr.veg <- "Rock/Talus"
curr.veg <- "Wet forest, early-mid"
curr.veg <- "Wet forest, mature"
curr.veg <- "Wet forest, old"
curr.veg <- "Wetland"
curr.veg <- "WBP"
curr.veg <- "Mesic forest, Mature"

# Ecological indicator
ei <- "asc"
ei <- "ba"
ei <- "mccsc"
ei <- "mpck"
ei <- "pbi"
ei <- "pbst"
ei <- "pcnwbp"
ei <- "pinf"
ei <- "sa"

#########################################
# Stratum
# Set to empty string if this EI has no strata
#########################################
# ei <- 'asc'	:
curr.stratum <- "Mature Trees"
curr.stratum <- "Medium Trees"
curr.stratum <- "Saplings"
curr.stratum <- "Small Trees"

# ei <- 'mccsc':
curr.stratum <- "Trees >=20 in DBH"
curr.stratum <- "Trees 10-15 in DBH"
curr.stratum <- "Trees 15-20 in DBH"
curr.stratum <- "Trees 5-10 in DBH"

# ei <- 'pcnwbp':
curr.stratum <- "Shrubs"
curr.stratum <- "Trees"

curr.stratum <- ""

# ei <- "SR"
# ei <- 'TD'
curr.stratum <- ""

# ei <- "GC"
curr.stratum <- "SubstrateOrganicMatter"
curr.stratum <- "SubstrateDecWood"

# ei <- "PCESS"
curr.stratum <- "Herb" # Don't bother; too little variance

# ei <- "PCGF"
curr.stratum <- "D_Soil"
curr.stratum <- "Dominant_Trees"
curr.stratum <- "Dw_Wood" # No variance; don't bother
curr.stratum <- "Epiphyte" # Almost no variance; don't bother
curr.stratum <- "Herb_Dwarf"
curr.stratum <- "Low_Shrub"
curr.stratum <- "Main_Canopy"
curr.stratum <- "Subcanopy_Trees"
curr.stratum <- "Tall_Shrub"

#######################
# Set local parameters
#######################

# Final options: vegetation
curr.veg <- "Avalanche feature"
curr.veg <- "Intermediate forest, Early-mid"
curr.veg <- "Wet forest, Old"
curr.veg <- "Brushland/Grassland"

# Final options: ei & stratum
ei <- "SR"
curr.stratum <- ""

# Custom title
# Set to NA to use default title
# Set to "" to remove title (or use title.main.omit)
title.main.custom <- ""
title.main.custom <- "Species Richness"

# Set to TRUE to omit main title at top of graph
title.main.omit <- FALSE

# Custom suffix for fig file
# Inserted at end of base name but before file extension
# Recommend including initial period or other delimiter
# Omit final delimiter as extension includes initial period
# Set to empty string to omit
fig.file.suffix <- ".boot.reps.100"
fig.file.suffix <- ""

# Set to TRUE to recalculate fit and n.min
fit.recalc <- FALSE

# Set to TRUE to save recalculated fit and n.min
# Only applies if fit.recalc==TRUE
fit.save <- TRUE

# Target power for predicting minimum sample size
# Recommend conventional power of 0.80
pwr.target <- 0.8

# Get remaining parameters for this ei
ei.pars <- ei.params(ei) # Get attributes of this EI (global function)
distn <- ei.pars$distn # Distribution of the EI
test.tail <- ei.pars$test.tail # Test tail for this EI
has.strata <- ei.pars$has.stratum # True if this EI has strata, otherwise F
if (has.strata == F) curr.stratum <- ""

# Make unix-friendly versions of veg, ei and stratum, for file name
veg.uf <- unix.friendly(curr.veg)
ei.uf <- unix.friendly(ei)
ei.veg.stratum.uf <- paste(ei.uf, veg.uf, sep = "-")
stratum.uf <- ""
if (trim(curr.stratum) != "") {
  stratum.uf <- paste0(".", unix.friendly(curr.stratum))
  ei.veg.stratum.uf <- paste(ei.veg.stratum.uf, stratum.uf, sep = "-")
}

# Model parameters
save.fig <- T # Save figure
replace.fig <- T # Replace existing figure for this ei+veg+stratum+e.size

# Input file of power analysis results
results.file <- paste0(RESULTSDIR, "power/power.ei.", veg.uf, ".", ei.uf, stratum.uf, ".csv")

# Output file of the figure
# Same as input file, but save to output directory
# and change extension to "png"
fig.file <- sub("results/", "figs/", results.file)
fig.file <- replace.last("csv", "png", fig.file) # change csv to png

if (!fig.file.suffix == "") {
  new.suff <- paste0(fig.file.suffix, ".png")
  fig.file <- sub(".png", new.suff, fig.file)
}

#################################
# Graph options
#################################

use.grayscale <- FALSE # FALSE=plot in color or grayscale

omit.fill <- TRUE # FALSE =filled symbols; TRUE=outline only

p.text.pos <- 30 # Position of target (conventional) power legend

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
color.codes.manual <- as.character(c("red", "royalblue1", "seagreen", "salmon"))
color.codes.manual <- as.character(c("seagreen", "red", "royalblue1", "salmon"))
color.codes.manual <- NULL

# Reverse the default sort order of legend
# TRUE | FALSE (default)
# NULL same as FALSE
reverse.legend <- TRUE

# Horizontal displacement of n.min label to right of n.min line
# Integer. Set to NULL to use default of 4
n.min.txt.pos.x.increment <- NULL
n.min.txt.pos.x.increment <- -4.5 # Best if max n.min close to R edge of figure
n.min.txt.pos.x.increment <- 4.5

# Vertical displacement of n.min label relative to
# default position, in increments of y. Can be negative.
# Set to NULL to use default
n.min.txt.pos.y.increment <- NULL
# n.min.txt.pos.y.increment <- 0.3
n.min.txt.pos.y.increment <- -0.23 # Best if max n.min close to R edge of figure
n.min.txt.pos.y.increment <- -0.2

# Change in horizontal placement of power line label, relative
# to default, in increments of x (=n.min). Can be negative.
# Set to NULL or 0 to use default.
# Exact numbers as well as effect of "NULL" depend on range of x
p.text.pos.x.increment <- NULL # Start at default position R of middle
p.text.pos.x.increment <- -35 # Start power far left

################################
# Set dependent parameters
################################

if (title.main.omit == TRUE) {
  main.text <- ""
} else if (!(is.na(title.main.custom) || title.main.custom == "")) {
  main.text <- title.main.custom
} else {
  main.text <- full.stratum
}

################################
# Confirm operation
################################

cat("\nRun vqa.power.plot.edi with following settings:\n")
cat("  Vegetation: ", curr.veg, "\n", sep = "")
cat("  Indicators ", ei, "\n", sep = "")
cat("  Stratum: ", curr.stratum, "\n", sep = "")
cat("  Recalculate fit: ", fit.recalc, "\n", sep = "")
cat("  Figure title: ", main.text, "\n", sep = "")
cat("  Data file (input): ", results.file, "\n", sep = "")
cat("  Figure file (output): ", fig.file, "\n", sep = "")

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

br <- "\n"
h.line <- "------------------------------------------------"

results <- read.csv(results.file, header = T, stringsAsFactors = FALSE)

# Extract all land cover classes, EIs and strata
sample.strata <- results[results$landcover == curr.veg & results$EI == ei, c("landcover", "EI", "stratum")]
sample.strata <- sample.strata[!duplicated(sample.strata), ]
n.strata <- nrow(sample.strata)

# Starting message
main.title <- "Plotting VQA Power Simulation Results"
cat(paste0(h.line, br, main.title, br, br))

### for (x in 1:n.strata) {		# START stratum loop
x <- 1
curr.veg <- sample.strata[x, c("landcover")]
curr.ei <- sample.strata[x, c("EI")]
ei.text <- ei.params(curr.ei)$ei.name

curr.stratum <- sample.strata[x, c("stratum")]
if (curr.stratum == "nostrata" || is.na(curr.stratum)) curr.stratum <- ""
full.veg <- trim(paste0(curr.veg, " ", curr.stratum))
full.stratum <- paste0(ei.text, " - ", full.veg)

if (curr.stratum == "") {
  dat <- results[results$landcover == curr.veg & results$EI == curr.ei, ]
} else {
  dat <- results[results$landcover == curr.veg &
    results$EI == curr.ei & results$stratum == curr.stratum, ]
}

# Echo current sample stratum
cat(paste0(full.stratum), "\n")

#######################
# Set plot parameters
#######################

x.text <- "Number of plots (n)"
y.text <- "Power"

pwr.target <- unique(dat$pwr.target)

#######################
# Recalculate & save fit & n.min if requested
#######################

if (fit.recalc == TRUE) {
  cat("Re-calculating fit line and n.min...")
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
        results[results$e.size == curr.es, c("n.min")] <- n.min.target
        write.csv(results, file = results.file, row.names = FALSE)
      } # END if ( fit.save==TRUE )
    } # END if ( !fit[1]=='fail' )
  } # END es loop
  cat("done\n")
} # END if ( fit.recalc==TRUE )

#######################
# Plot the results & save figure
#######################

p <- plot.power.logistic.multi.ei(
  df.pwr = dat,
  title.main = main.text, title.main.center = title.main.center,
  x.title = x.text, y.title = y.text,
  grayscale = use.grayscale,
  no.fill = omit.fill,
  n.min.txt.pos.x.increment = n.min.txt.pos.x.increment,
  n.min.txt.pos.y.increment = n.min.txt.pos.y.increment,
  p.text.pos.x.increment = p.text.pos.x.increment,
  color.codes.manual = color.codes.manual, reverse.legend = reverse.legend
)

# print the graph
dev.new(width = 7, height = 6)
print(p)

# # Make unix-friendly file name
# curr.veg.fname <- unix.friendly(curr.veg)
# stratum.fname <- unix.friendly(curr.stratum)

# if (has.strata) {
# fig.file.name<-paste("ei.power", curr.veg.fname, stratum.fname, ei, sep="_")
# } else {
# fig.file.name<-paste("ei.power", curr.veg.fname, ei, sep="_")
# }

#####################
# Save the figure
#####################
# fig.file <- paste0("figs/power/", fig.file.name, ".png")

if (replace.fig == F) {
  # If file already exists, change name so as not to replace
  # Up to maximum of 11 versions of file, including original
  for (i in 1:11) {
    if (!file.exists(fig.file)) {
      cat("File doesn't exist!!!")
      if (i > 10) {
        stop("Too many versions of this figure, delete one or more!")
      } else {
        break
      }
    } else {
      cat("File exists!")
      # Rename the new file with numbered suffix
      fig.file <- replace.last(".png", paste0("_", i, ".png"), fig.file)
    }
  }
}

if (save.fig == T) {
  dev.copy(png, fig.file, width = 175, height = 150, units = "mm", res = 300)
  dev.off()
  cat(paste0("Saving figure as file: '", fig.file, "'\n"))
}

### } 	# END stratum loop ###

cat("\nOperation completed\n")
cat(paste0(h.line, br))

######################################################
# Plots results of VQA posthoc power analysis for overall quality. Plots
# power versus sample size (n) for multiple vegetation types at a single
# effect size.
#
# Author: Brad Boyle (bboyle@email.arizona.edu)
#
# Requires:
# 		input file power.single.ei.results.csv
######################################################

rm(list = ls()) # clear workspace

# Critical!
# Determines data directories and params file used
# Params file used=paste0("params.", proj, ".R")
# Assessment code and everything else are set in the params file
proj <- "teck-dpm"
proj <- "teck-pom"

#######################
# Load packages and set
# dependent parameters
#######################

# Load required packages
library(drc) # For fitting Michaelis-Menten curve
library(ggplot2)

# Set working directory
# Must set first to set directory parameters
wd <- getwd()
setwd(wd)
wd

# # Load global params file if not already loaded
# if(!exists("global.params.loaded", mode="function")) source("params.R")
params.file <- paste0("params.", proj, ".R")
# cat("Loading parameters file '", params.file, "'...", sep="")
# if ( file.copy(from=params.file, to="params.R", overwrite = TRUE)==FALSE ) {
# stop("ERROR: file copy failed!\n")
# }
# cat("done\n\n")

#####################################################
# Parameters
#####################################################

# Get global parameters & functions
source(params.file)
source(paste0(INCLUDESDIR, "functions.R"), local = TRUE)

########################
# File and directory names
########################

# Set output directory for power figure & create if missing
POWERFIGDIR <- paste0(FIGDIR, "power/")
dir.create(file.path(POWERFIGDIR), showWarnings = F)

# Input file name and directory (power analysis results)
# Include extension
# Adjust as needed
results.file.name <- "power_int-forest-early-mid.boot.reps.100.csv"
results.file.name <- "power_int-forest-early-mid.boot.reps.10.csv"
results.file.name <- "power.baseline.final.csv"
results.file.name <- "power_baseline.boot.10.csv"
results.file.name <- "power_baseline.same.as.rec.csv"
results.file.name <- "power_int-forest-early-mid.boot.reps.comparison.csv"
results.file.name <- "power.baseline.boot-1000.csv"
results.file.name <- "power.reclamation.boot-1000.csv"
results.file.name <- "power.final.csv" # default
results.file.name <- "power.omit.GC.csv"
results.file.name <- "power.GC-noGC.comparison.csv"
results.file.name <- "power.with.GC.csv"

results.file.dir <- paste0(RESULTSDIR, "power/")
results.file <- paste0(results.file.dir, results.file.name)

# Output file basename (graph)
# Omit extension
fig.file.name <- "power.int-forest-early-mid.boot.reps.100"
fig.file.name <- "power.int-forest-early-mid.boot.reps.10"
fig.file.name <- "power.baseline.all.boot.100"
fig.file.name <- "power.baseline.boot.10"
fig.file.name <- "power_int-forest-early-mid.boot.reps.comparison"
fig.file.name <- "power.multi.baseline.same.as.rec"
fig.file.name <- "power.baseline.boot.1000"
fig.file.name <- "power.reclamation.boot.1000"
fig.file.name <- "power.multi.baseline.same.as.rec.final"
fig.file.name <- "power.multi.rec.final"
fig.file.name <- "power.all.boot.1000"
fig.file.name <- "power.omit.GC"
fig.file.name <- "power.GC-noGC.comparison"
fig.file.name <- "power.with.GC"

########################
# Input data options
########################

# Filter by vegetation (TRUE) or include all in input file (FALSE)
filter.by.veg <- FALSE

# Vegetation types to include
# Ignored if filter.by.veg==FALSE
veg.list <- c(
  "Alpine",
  "Alpine Dwarf Shrub",
  "Brushland/Grassland",
  "Intermediate forest, Old"
)
veg.list <- c(
  "Alpine",
  "Alpine Dwarf Shrub",
  "Intermediate forest, Old"
)
veg.list <- c(
  "Brushland/Grassland",
  "Dry forest, Early-mid",
  "Intermediate forest, Early-mid",
  "Wet forest, Early-mid"
)

# Hack to substitute benchmark vegetation name for
# reclamation veg name by removing suffix " (reclaimed)"
rec.veg.set.bm <- FALSE

########################
# Power analysis options
########################

# Plot only single effect size (TRUE) or plot all?
filter.by.e.size <- TRUE

# The effect size you wish to plot
# Ignored if filter.by.e.size==FALSE
e.size <- 0.1

# Target power for predicting minimum sample size
# Recommend conventional power of 0.80
pwr.target <- 0.8

# Recalculate fit and n.min? (TRUE | FALSE )
fit.recalc <- FALSE

########################
# Output options
########################

save.fig <- T # Save figure to file
replace.fig <- T # Replace existing figure for this ei+veg+stratum+e.size

# Save recalculated fit and n.min to input file? (TRUE | FALSE )
# If TRUE will replace original values
# Only used if fit.recalc==TRUE
fit.save <- FALSE

########################
# Figure options
########################

# Figure resolution
res <- 300
res <- 1000

# Figure file type
# Options: "png", "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg"
# "pdf" is for vector graphics (e.g. line plots, point plots, polygon plots etc.)
# "tiff" best ror raster graphics (e.g. photos, density scatter plots, anything pixel based).
fig.file.type <- "pdf"
fig.file.type <- "png"

# Integer amount of jitter to add to x and y in graph. Set to 0 or NULL for no jitter
jitter.val <- 0

# Omit n.min lines? (TRUE | FALSE )
n.min.omit <- FALSE

# Place n.min text to right of line? (TRUE | FALSE )
# If FALSE, places text to left
n.min.txt.right <- TRUE

# Omit legend? (TRUE | FALSE )
# Set to TRUE to reduce clutter if many vegetation types,
# or if will add legend manually or in caption.
no.legend <- TRUE

# Legend title
# Set NULL to use default
leg.ti <- "Land cover"

# Legend text fontsize
# Set NULL to use default (see function)
leg.text.fontsize <- 10

# Set legend position manually?
# If TRUE uses supplied values of leg.pos.x & leg.pos.y
leg.pos.manual <- TRUE

# Manual position of legend, inside graph
# Both are relative to lower left corner of graph, on 0-1 scale
leg.pos.x <- 0.77
leg.pos.y <- 0.23

# Plot lowest ahd highest n.min lines only? (TRUE | FALSE )
# If FALSE, prints vertical line for all vegetation classes
n.min.minmaxonly <- FALSE

# Fixed values of n.min.min and n.min.max
# If number supplied, over-rides actual values set by n.min.txt.pos.right & n.min.txt.pos.left
# Set to NULL to use actual values
n.min.min.fixed <- NULL
n.min.max.fixed <- NULL

# Displacement of n.min text position from n.min line
# Set only one and leave the NULL
n.min.txt.pos.right <- 1.0
n.min.txt.pos.left <- NULL

# Size of m
n.min.text.size <- 4

# Fixed x-axis limit
# Set to null to use default
x.max.fixed <- 20
# x.max.fixed =NULL

# # Position (x value: n) of power label
# # Set to NULL to use default
p.text.pos <- NULL
p.text.pos <- 6

# Figure dimensions
if (no.legend == FALSE) {
  # Adjust width to accomodate legend
  if (leg.pos.manual == FALSE) {
    # Legend outside graph; increase width
    fig.width <- 10
    fig.height <- 6
  } else {
    # Legend inside graph; decrease width
    fig.width <- 6
    fig.height <- 6
  }
} else {
  fig.width <- 6
  fig.height <- 6
  fig.file.name <- paste0(fig.file.name, ".nolegend")
}

#####################################################
# Functions
#####################################################

plot.power.logistic.multi.veg <- function(df.pwr,
                                          no.legend = FALSE,
                                          leg.pos.manual = FALSE, leg.pos.x = NULL, leg.pos.y = NULL,
                                          n.min.minmaxonly = NULL, n.min.min.fixed = NULL, n.min.max.fixed = NULL,
                                          n.min.txt.pos.left = NULL, n.min.txt.pos.right = NULL,
                                          n.min.omit = FALSE, x.max.fixed = NULL, n.min.txt.right = NULL,
                                          title.main = NULL, x.title = NULL, y.title = NULL,
                                          p.text.pos = NULL, n.text.pos = NULL, veg.text.pos.x = NULL, veg.text.pos.y = NULL,
                                          leg.text.fontsize = NULL, leg.ti = NULL,
                                          jitter.val = NULL) {
  ###################################################
  # Plots predicted (pwr) vs sample size (n) with fit curve for multi
  # strata (vegetation types) at once. Adds lines for target power
  # (pwr.target) and minimum sample size @ target power (n.min).
  # Can be configured to show only min and max lines for n.min.
  # Uses conventional power of 0.80 if target power not supplied.
  #
  # Parameters:
  # 		df.pwr				df of power analysis simulation results, should already
  # 								be subsetted to single vegetation + EI only
  # 	From df.pwr:
  # 		landcover		Vegetation/land cover class ("veg")
  # 		EI						Indicator
  # 		stratum			Stratum (if applicable)
  # 		n.b					Total benchmark samples in original data
  # 		n.f					Total focal samples in original data
  # 		n						Simulated sample sizes (n.b.sim=n.f.sim)
  # 		e.size				Effect size used for simulation
  # 		e.size.adj
  # 		e.size.min		Lowest of the two margins of error: min(mean-lcl, ucl-mean)
  # 		pwr					Post hoc (observed) power of current simulation
  # 		n.min				Sample size at target power (pwr.target), from logistic fit
  # 		fit				Predicted power values from logistic fit
  # 		expts				Number of runs (experiments) in this simulation
  # 		boot.reps		Number of bootstrap replications used to calculate 95% CLs
  # 		pwr.target		Target power (default: 0.80)
  # 	Passed directly
  # 		no.legend		Omit legend (avoids clutter if many veg types)
  # 		n.min.minmaxonly	Plot smallest and largest n.min lines only
  # 		n.min.min.fixed		Fixed value of n.min.min. If supplied overrides actual value from data
  # 		n.min.max.fixed		Fixed value of n.min.max. If supplied overrides actual value from data
  # 		n.min.omit		Don't plot n.min line(s)
  # 		n.min.txt.right	Place n.min text to right of .n.min line? (default: R, FALSE: L)
  # 		title.main			Title for graph (default: none)
  # 		x.title				X axis title (default: "Sample size")
  # 		y.title				Y axis title (default: "Power")
  # 		p.text.pos		Optional specification of x coordinate of left edge of
  # 								label for target power line
  # 		n.text.pos		Optional specification of x coordinate of left edge of
  # 								label for n.target line
  # 		veg.text.pos.x		Optional specification of x coordinate of left edge of
  # 								label for effect size
  # 		veg.text.pos.y		Optional specification of y coordinate of label for
  # 								effect size
  # 		jitter.val			Option. Amount of jitter to add to x and y using function
  # 								jitter(). Integer. See base function definition.
  # 		x.max.fixed		Fixed max value of x axis. If NULL then max value of x used
  #
  # Returns: ggplot object, ready for printing and saving
  ###################################################

  if (!require(ggplot2)) stop("ERROR: package 'ggplot2' not loaded")

  # Extract key variables and vectors
  df.pwr$landcover <- as.factor(df.pwr$landcover) # Convert main grouping class to factor

  pwr.target <- unique(df.pwr$pwr.target)
  if (length(pwr.target) > 1) stop("ERROR: >1 value of target power (plot.power.logistic.multi.ei)")

  all.n <- df.pwr$n # Simulated sample sizes
  n.max <- max(all.n)
  n.min <- min(all.n)
  veggies <- unique(df.pwr$landcover) # Effect sizes used
  num.es <- length(veggies) # Number of effect sizes
  n.mins <- unique(df.pwr$n.min) # Minimum sample sizes at each effect size
  num.n.mins <- length(n.mins) # Should be same as num.es

  # Get min and max values of n.min
  n.min.min <- min(df.pwr$n.min)
  if (is.null(n.min.min.fixed)) {
    n.min.min <- min(df.pwr$n.min)
  } else {
    n.min.min <- n.min.min.fixed
  }
  n.min.min.text <- paste0("n.min=", n.min.min)
  if (is.null(n.min.max.fixed)) {
    n.min.max <- max(df.pwr$n.min)
  } else {
    n.min.max <- n.min.max.fixed
  }
  n.min.max.text <- paste0("n.min=", n.min.max)
  if (is.null(n.min.txt.right)) n.min.txt.right <- FALSE

  # Set default/calculated values
  if (is.null(title.main)) title.main <- "Power vs. Sample size"
  if (is.null(x.title)) x.title <- "Sample size (n)"
  if (is.null(y.title)) y.title <- "Power"
  if (is.null(pwr.target)) pwr.target <- 0.80
  if (is.null(no.legend)) nolegend <- FALSE
  if (is.null(n.min.minmaxonly)) n.min.minmaxonly <- FALSE
  if (is.null(n.min.txt.pos.left)) n.min.txt.pos.left <- 0
  if (is.null(n.min.txt.pos.right)) n.min.txt.pos.right <- 0
  if (leg.pos.manual == TRUE) {
    if (is.null(leg.pos.x) || is.null(leg.pos.y)) leg.pos.manual <- FALSE
  }
  if (is.null(leg.text.fontsize)) leg.text.fontsize <- 14

  # NOTE: leg.ti not implemented. Can't figure out how to apply it
  # wthout generating second legend
  if (is.null(leg.ti)) leg.ti <- "Legend"

  # Set horizontal placement of text label for conventional power line
  if (is.null(p.text.pos)) {
    p.text.pos.fixed <- FALSE
    if (min(n.mins, na.rm = TRUE) > min(all.n, na.rm = TRUE) + 0.75 * ((max(all.n, na.rm = TRUE)) - (min(all.n, na.rm = TRUE)))) {
      # Place legend at left of graph
      p.text.pos <- min(all.n) + 6
    } else {
      # Place legend at right of graph
      p.text.pos <- max(all.n) - 6
    }
  } else {
    p.text.pos.fixed <- TRUE
  }

  # Axis limits
  if (is.null(x.max.fixed)) {
    x.axis.max <- n.max
  } else {
    x.axis.max <- x.max.fixed

    if (p.text.pos.fixed == FALSE) {
      # Also adjust power text position relative to fixed position of x.max
      p.text.pos <- x.axis.max - (0.05 * x.axis.max)
    }
  }
  x.axis.min <- n.min
  y.axis.max <- 1
  y.axis.min <- 0

  # Generate n.min text and set positions
  df.pwr$n.min.txt <- paste0("n=", as.character(df.pwr$n.min))
  str_len <- function(x) nchar(x)
  df.pwr$n.min.txt.len <- 1
  df.pwr[, c("n.min.txt.len")] <- lapply(df.pwr[c("n.min.txt.len")], str_len)
  if (n.min.txt.right == 0 || is.null(n.min.txt.right)) {
    n.min.txt.adj <- n.min.txt.pos.left
  } else {
    n.min.txt.adj <- n.min.txt.pos.right
  }
  # adj <- function(x) x + n.min.txt.adj
  df.pwr$n.min.txt.pos <- df.pwr$n.min + n.min.txt.adj

  # Set vector of manually-defined colour codes
  # Make sure this has enough elements to support the largest possible number
  # of effect sizes
  color.codes.all <- as.character(c("blue", "red", "green", "orange"))
  # color.codes.all <-as.character(c("blue4", "royalblue1", "lightskyblue", "salmon" ))
  # color.codes.all <-as.character(c("coral", "lightskyblue2", "seagreen", "red" ))
  color.codes <- color.codes.all[1:num.es] # Select the first num.es colors

  # Manually defined shape codes
  # These are all filled with outline
  shape.codes.all <- c(21, 22, 23, 24)
  shape.codes <- shape.codes.all[1:num.es] # Select the first num.es colors

  # Add requested jitter if applicable
  if (!is.null(jitter.val) || jitter.val == 0) {
    df.pwr$n <- jitter(df.pwr$n, jitter.val)
    df.pwr$pwr <- jitter(df.pwr$pwr, jitter.val)
  }

  # Generate target power text
  p.text <- paste0("Power=", pwr.target)

  # Echo parameter values...for testing only
  cat("Plotting function parameters:", "\n", sep = "")
  cat("n.min.min:", n.min.min, "\n", sep = "")
  cat("n.min.min.text:", n.min.min.text, "\n", sep = "")
  cat("n.min.max:", n.min.max, "\n", sep = "")
  cat("n.min.max.text:", n.min.max.text, "\n", sep = "")
  cat("pwr.target:", pwr.target, "\n", sep = "")
  cat("p.text:", p.text, "\n", sep = "")
  cat("p.text.pos:", p.text.pos, "\n", sep = "")
  cat("p.text.pos.fixed:", p.text.pos.fixed, "\n", sep = "")
  flush.console()

  # ---------------------------------------------
  # Plot the power curve
  # --------------------------------------------

  p <- ggplot(df.pwr, aes(x = n, y = pwr, colour = landcover, shape = landcover)) +
    scale_shape_manual(values = 1:nlevels(df.pwr$landcover)) +
    theme_bw() +
    xlab(x.title) +
    ylab(y.title) +
    ggtitle(title.main) +
    geom_point(alpha = 0.95)
  # Add the logistic fit lines
  p <- p + geom_line(aes(y = fit), show.legend = FALSE)

  if (no.legend == TRUE) {
    # Turn off legend
    p <- p + theme(legend.position = "none")
  } else {
    # Generate the legend
    # "override.aes" modifies the size of the legend symbols
    # p <- p + guides( colour = guide_legend( override.aes=list(size=3) ) ) +
    # theme(legend.key=element_rect(fill=NA))
    p <- p + guides(colour = guide_legend(override.aes = list(size = 3)))

    if (leg.pos.manual == TRUE) {
      # Set manual legend position
      p <- p + theme(legend.position = c(leg.pos.x, leg.pos.y))
    }
  }

  # Set axis limits, removing gutter
  p <- p + scale_x_continuous(limits = c(x.axis.min, x.axis.max))
  p <- p + scale_y_continuous(limits = c(y.axis.min, y.axis.max))

  # Add horizontal line at target power or precision & label it
  p <- p + geom_hline(aes(yintercept = pwr.target), colour = "black", linetype = "dotted")
  p <- p + geom_text(aes(p.text.pos - 1, pwr.target, label = p.text, vjust = -1), size = 5, colour = "black", show.legend = FALSE)

  # Add vertical lines for n.min @ target power, and label them
  if (n.min.minmaxonly == TRUE) {
    p <- p + geom_vline(aes(xintercept = n.min.min), colour = "black", linetype = "dotted")
    p <- p + geom_vline(aes(xintercept = n.min.max), colour = "black", linetype = "dotted")
  } else {
    p <- p + geom_vline(data = df.pwr, aes(xintercept = n.min, colour = landcover), linetype = "dashed", show.legend = FALSE)
  }

  # Label the n.min lines
  if (n.min.minmaxonly == TRUE) {
    p <- p + geom_text(aes(n.min.min - 1.7, 0, label = n.min.min.text, vjust = -1, fontface = "plain", family = "sans"), size = n.min.text.size, colour = "black", show.legend = FALSE)
    p <- p + geom_text(aes(n.min.max - 1.7, 0.1, label = n.min.max.text, vjust = -1, fontface = "plain", family = "sans"), size = n.min.text.size, colour = "black", show.legend = FALSE)
  } else {
    p <- p + geom_text(data = df.pwr, aes(n.min.txt.pos, 0, label = n.min.txt, vjust = -1, fontface = "plain", family = "sans"), size = n.min.text.size, colour = "black", show.legend = FALSE)
  }

  # eliminates background, gridlines, and chart border
  p <- p + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  # Set text sizes
  p <- p + theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
  p <- p + theme(plot.title = element_text(size = 14))
  if (no.legend == FALSE) {
    p <- p + theme(legend.text = element_text(size = leg.text.fontsize))
  }

  # return the graph
  return(p)
}

#####################################################
# Main
#####################################################

#################################
# Load the results & extract values for
# current veg & EI
#################################

if (file.exists(results.file)) {
  results <- read.csv(results.file, header = T, stringsAsFactors = FALSE)
} else {
  stop(paste0("File '", results.file, "' not found!"))
}

#######################
# Recalculate & save fit & n.min if requested
#######################

if (fit.recalc == TRUE) {
  cat("Re-calculating fit line and n.min...")

  # Extract all land cover classes & effect sizes
  veggies <- results[, c("landcover")]
  veggies <- unique(veggies)
  e.sizes <- unique(results[, c("e.size")])

  for (veg in 1:length(veggies)) { # START veg loop
    curr.veg <- veggies[veg]

    for (es in 1:length(e.sizes)) { # START es loop
      curr.es <- e.sizes[es]

      # Get current veg + es subset
      results.veg.es <- results[results$landcover == curr.veg & results$e.size == curr.es, ]

      fit <- power.logistic(df.pwr = results.veg.es, pwr.target = pwr.target)
      n.max <- max(results.veg.es$n)

      if (!fit[1] == "fail") {
        n.min.target <- unique(fit$n.min)

        # Hack for bug that returns maximum value of x
        # over submitted x values (x.obs) if predicted value of x (x.pred)
        # is outside domain of x.obs
        if (is.null(n.min.target)) {
          n.min.target <- NA
        } else if (is.na(n.min.target)) {
          n.min.target <- NA
        } else if (n.min.target >= n.max) {
          n.min.target <- 99999
        }
        n.min.target <- rep(n.min.target, nrow(results.veg.es))

        results$n.min[results$landcover == curr.veg & results$e.size == curr.es] <- n.min.target

        # Set fit values >1 to 1 to allow graphing function
        # to plot complete asymptote
        fit$pwr.fit[fit$pwr.fit > 1] <- 1
        results$fit[results$landcover == curr.veg & results$e.size == curr.es] <- fit$pwr.fit

        if (fit.save == TRUE) {
          # Save the revised calculations
          results[results$landcover == curr.veg & results$e.size == curr.es, c("fit")] <- fit$pwr.fit
          results[results$landcover == curr.veg & results$e.size == curr.es, c("n.min")] <- n.min.target
          write.csv(results, file = results.file, row.names = FALSE)
        } # END if ( fit.save==TRUE )
      } # END if ( !fit[1]=='fail' )
    } # END es loop
  } # END veg loop
  cat("done\n")
} # END if ( fit.recalc==TRUE )

if (filter.by.veg == TRUE) {
  results <- results[results$landcover %in% veg.list, ]
}

if (rec.veg.set.bm == TRUE) {
  # Hack to substitute benchmark veg for reclamation veg name
  results$landcover <- gsub(" \\(reclaimed\\)", "", results$landcover)
}

# Re-extract all land cover classes
# veggies <- results[ results$landcover==curr.veg, c("landcover")]
veggies <- results[, c("landcover")]
veggies <- unique(veggies)
n.veg <- length(veggies)

# Starting message
br <- "\n"
h.line <- "------------------------------------------------"
main.title <- "Plotting VQA Power Simulation Results"
cat(paste0(h.line, br, main.title, br))

# Get the results df
if (filter.by.e.size == TRUE) {
  dat <- results[results$e.size == e.size, ]
} else {
  dat <- results
}

#######################
# Plot the results & save figure
#######################

# Set plot parameters
main.text <- ""
x.text <- "Number of plots (n)"
y.text <- "Power"
n.min.min <- min(dat$n.min)
n.min.max <- max(dat$n.min)

p <- plot.power.logistic.multi.veg(
  df.pwr = dat,
  no.legend = no.legend,
  leg.pos.manual = leg.pos.manual, leg.pos.x = leg.pos.x, leg.pos.y = leg.pos.y,
  p.text.pos = p.text.pos,
  n.min.minmaxonly = n.min.minmaxonly, n.min.min.fixed = n.min.min.fixed,
  n.min.txt.pos.left = n.min.txt.pos.left, n.min.txt.pos.right = n.min.txt.pos.right,
  n.min.txt.right = n.min.txt.right,
  n.min.max.fixed = n.min.max.fixed, n.min.omit = n.min.omit, x.max.fixed = x.max.fixed,
  title.main = main.text, x.title = x.text, y.title = y.text,
  leg.text.fontsize = leg.text.fontsize, leg.ti = leg.ti,
  jitter.val = jitter.val
)

#####################
# Print and save the figure
#####################

dev.new(width = fig.width, height = fig.height)
print(p)

fig.file <- paste0(POWERFIGDIR, fig.file.name, ".", fig.file.type)

if (replace.fig == F) {
  # If file already exists, change name so as not to replace
  # Up to maximum of 11 versions of file
  for (i in 1:10) {
    if (!file.exists(fig.file)) {
      break
    } else {
      fig.file <- paste0(POWERFIGDIR, fig.file.name, "_", i, ".", fig.file.type)
    }
  }
}

# print the graph
# dev.new(width=7, height=6)
if (save.fig == T) {
  # 	dev.copy(png, fig.file, width=fig.width, height=fig.height)
  ggsave(fig.file, width = fig.width, height = fig.height, dev = fig.file.type, dpi = res, bg = "white")
}

cat("Run completed\n")

##########################################################
# Plot multiple NMDS results, colored by class
#
# Requires:
# 	1. NMDS site score file "site_scores.csv"
# 	2. NMDS results summary file "nmds_results.csv"
#
# By: Brad Boyle
# Email: brad@hg-llc.com
# Created: 19 May 2023
#
# Note temporarily commented code at end!
##########################################################

##########################
# Reset all and set base directory
##########################
graphics.off()
rm(list = ls())
wd <- getwd()
setwd(wd)

############################################
# Parameters
############################################

# Project & assessment
# ----> CRITICAL <----
PROJ <- "teck-dpm"
ASSESS <- "001_offset1_baseline_2024504"

# Input file of NMDS scores
# Typically, will be the TD raw indicator values file, but can
# use any NMDS results file, as long as the required columns
# are present.
# Generally, "TD_raw.bak.csv" contains all plots, whereas
# "TD_raw.csv" has already been pruned of outliers.
F.SITE.SCORES.BASE <- "TD_raw.bak" # Input file
# F.SITE.SCORES.BASE <- "TD_raw" # Input file
F.SITE.SCORES <- paste0(F.SITE.SCORES.BASE, ".csv") # Input file

# Label points?
# show.sites is ignored if show.outlier==TRUE
show.outliers <- TRUE # Detect outliers and display plot code beside outlier points?
show.sites <- FALSE # Display all plot codes? Ignored if show.outliers==TRUE

# Vegetation column to group by
veg.grouping.class <- "landCover" # Land cover
# veg.grouping.class<-"vegClass"		# Benchmark vegetation

# Compare benchmark plots only
bm.only <- FALSE

# Number of SDs required to detect outlier
sd.mult <- 3

# Number of dimensions to plot
# Set to NULL to use empirical values from NMDS scores file
dims.custom <- NULL
dims.custom <- 3

# General graph attributes
use.colors.fb <- TRUE # Manually set legend colors using focal/benchmark colors
alpha.val <- 0.50
colors.fb <- c(rgb(0, 0, 1, alpha.val), rgb(1, 0, 0, alpha.val))
title.bold <- TRUE
title.fontsize <- 18 # NULL=use default
x.text <- "NMDS Axis 1"
y.text <- "NMDS Axis 2"
z.text <- "NMDS Axis 3"
axis.fontsize <- 14 # NULL=use default
axis.tick.fontsize <- 10 # NULL=use default
leg.title <- "Focal or \nBenchmark" # Legend title; NULL=use default
res <- 100 # Resolution; NULL to use default

# Rearrange point labels for improved readability
# Generally only works well for show.outliers=TRUE
labels.repel <- TRUE

# Remove legend for multi-panel plots?
remove.legend <- FALSE

# Directories
BASEDIR <- "/Users/bboyle/Documents/hga/vqa2/" # Absolute path
# BASEDIR <- paste0(wd, "/../", )										# Relative path
DATA_BASEDIR <- paste0(BASEDIR, "data/", PROJ, "/", ASSESS, "/")
CODE_BASEDIR <- paste0(BASEDIR, "scripts/")
CODE_BASEDIR <- paste0(BASEDIR, "src/")
INPUTDIR <- paste0(DATA_BASEDIR, "results/")
FIG_BASEDIR <- paste0(DATA_BASEDIR, "figs/")
FIG_NMDS_DIR <- paste0(FIG_BASEDIR, "nmds/")
FIGDIR <- FIG_NMDS_DIR

# Functions file
F.FUNCTIONS <- paste0(CODE_BASEDIR, "includes/functions.R")

if (show.outliers == TRUE) {
  if (bm.only == TRUE) {
    BM.ONLY.DIR <- paste0(FIG_NMDS_DIR, "bm_only/")
    FIGSUBDIR <- "bm_only/zz_outliers/"
    FIGDIR <- paste0(FIG_NMDS_DIR, FIGSUBDIR)
  } else {
    FIGSUBDIR <- "zz_outliers/"
    FIGDIR <- paste0(FIG_NMDS_DIR, FIGSUBDIR)
  }
} else if (show.sites == TRUE) {
  if (bm.only == TRUE) {
    BM.ONLY.DIR <- paste0(FIG_NMDS_DIR, "bm_only/")
    FIGSUBDIR <- "bm_only/zz_plots/"
    FIGDIR <- paste0(FIG_NMDS_DIR, FIGSUBDIR)
  } else {
    FIGSUBDIR <- "zz_plots/"
    FIGDIR <- paste0(FIG_NMDS_DIR, FIGSUBDIR)
  }
}

# Output data files
F.NMDS.OUTLIERS.BASE <- paste0(F.SITE.SCORES.BASE, "_NMDS.outliers") # Copy of input file, outliers only
F.NMDS.OUTLIERS <- paste0(F.NMDS.OUTLIERS.BASE, ".csv") # Copy of input file, outliers only
f.outliers.compiled <- paste0(F.SITE.SCORES.BASE, "_NMDS.outliers.compiled.csv") # Compiled multiple runs
f.outlier.plots.compiled <- paste0(F.SITE.SCORES.BASE, "_NMDS.outliers.plots.compiled.csv") # Plots only

# Figure base name (prefix) and suffix
# NULL to omit
# Do NOT include delimiters such as "_",
# these will be added automatically
if (show.outliers == TRUE) {
  FIGBASENAME <- NULL
  FIGSUFFIX <- "outliers"
} else if (show.sites == TRUE) {
  FIGBASENAME <- NULL
  FIGSUFFIX <- "plots"
} else {
  # FIGBASENAME <- NULL
  # FIGSUFFIX <- NULL
  FIGBASENAME <- "NMDS"
  FIGSUFFIX <- NULL
}

##########################
# Load libraries & functions
##########################

suppressMessages(library(tidyverse)) # General tools
suppressMessages(library(ggrepel)) # Relocate labels for readability
source(F.FUNCTIONS, local = TRUE)

######################
# Custom functions
######################
nmds.plot <- function(df.nmds,
                      a1name = NULL, a2name = NULL, grp.name = NULL,
                      show.sites = FALSE, site.type.name = NULL, show.outliers = FALSE,
                      title.main = NULL, title.bold = FALSE, title.fontsize = NULL,
                      axis.fontsize = NULL, axis.tick.fontsize = NULL,
                      use.colors.fb = NULL, colors.fb = NULL, leg.title = NULL,
                      labels.repel = FALSE, ggrepel.max.overlaps = NULL, remove.legend = FALSE) {
  #########################################
  # plot NMDS axes provided, with point colored by class
  #
  # Required parameters:
  # 		df.nmds: data frame of nmds scores
  # 			Required columns:
  # 				a1: horizontal axis values
  # 				a2: vertical axis values
  # 				grp: grouping class values
  # 			Optional columns:
  # 				site: site names (required if show.sites==TRUE)
  # Optional parameters:
  # 		show.sites: include site names by points (TRUE|FALSE)
  # 		a1name: a1 axis name (e.g., "x", "Axis 1", etc.)
  # 		a2name: a2 axis name (e.g., "y", "Axis 2", etc.)
  # 		grp.name: Grouping class name (e.g., "Vegetation type")
  # 		site.type.name: Class name of sites (e.g., "Plot", etc.)
  # 		title: Main title, if not null placed above graph
  # Returns: graph object
  #########################################

  if (is.null(ggrepel.max.overlaps)) options(ggrepel.max.overlaps = Inf)

  # Set defaults
  if (is.null(a1name)) a1name <- "Axis 1"
  if (is.null(a2name)) a2name <- "Axis 2"
  if (is.null(grp.name)) grp.name <- "Group"
  if (is.null(site.type.name)) site.type.name <- "Site"
  if (is.null(title.main)) title.main <- ""
  if (is.null(colors.fb)) colors.fb <- c(rgb(0, 0, 1), rgb(1, 0, 0))

  # Set axis limits
  x.max <- max(df.nmds$a1)
  x.min <- min(df.nmds$a1)
  y.max <- max(df.nmds$a2)
  y.min <- min(df.nmds$a2)
  extra <- 0.01
  xlim.max <- x.max + ((x.max - x.min) * extra)
  xlim.min <- x.min - ((x.max - x.min) * extra)
  ylim.max <- y.max + ((y.max - y.min) * extra * 100)
  ylim.min <- y.min - ((y.max - y.min) * extra)

  # Make the plot
  p <- qplot(a1, a2, data = df.nmds, color = grp, main = title.main)
  p <- p + theme_classic()
  p <- p + theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
  # p <- p + scale_color_discrete(name=grp.name)

  # Apply standard fb colors if request
  if (use.colors.fb == TRUE) {
    p <- p + scale_color_manual(values = colors.fb)
  }

  # Label points if requested
  # Outlier take precedence over labeling all sites
  if (show.outliers == TRUE) {
    if (labels.repel == TRUE) {
      p <- p + geom_text_repel(aes(label = ifelse(outlier == TRUE, site, "")),
        box.padding = unit(0.45, "lines"),
        show.legend = FALSE
      )
    } else {
      p <- p + geom_text(
        label = ifelse(
          df.nmds$outlier == TRUE,
          df.nmds$site, ""
        ),
        hjust = -0.15, show.legend = FALSE
      )
    }
  } else if (show.sites == TRUE) {
    if (labels.repel == TRUE) {
      # Generally NOT a good idea for all points
      # If many points, only a few will be labeled
      p <- p + geom_text_repel(aes(label = site),
        box.padding = unit(0.45, "lines"), show.legend = FALSE
      )
    } else {
      p <- p + geom_text(
        label = df.nmds$site,
        hjust = -0.15, show.legend = FALSE
      )
    }
  }

  # Set title properties
  if (!title.main == "") {
    if (title.bold == TRUE) p <- p + theme(plot.title = element_text(face = "bold"))
    if (!is.null(title.fontsize)) {
      p <- p + theme(plot.title = element_text(size = title.fontsize))
    }
  }

  # Set axis names & font
  p <- p + labs(x = a1name, y = a2name)
  if (!is.null(axis.fontsize)) {
    p <- p + theme(axis.title = element_text(size = axis.fontsize))
  }
  if (!is.null(axis.tick.fontsize)) {
    p <- p + theme(axis.text = element_text(size = axis.tick.fontsize))
  }

  # Remove legend if request
  if (remove.legend == TRUE) {
    p <- p + theme(legend.position = "none")
    # p <- p + guides(color = FALSE, size = FALSE)
  } else {
    # Adjust the legend
    if (!is.null(leg.title)) p <- p + labs(color = leg.title) # Custom title
    p <- p + theme(legend.position = "right")
    p <- p + theme(legend.background = element_rect(colour = "black"))
  }

  return(p)
}

save.plot <- function(plot, f.name, dir.name, res = NULL) {
  if (is.null(res)) res <- 200

  f.fig <- paste(FIGDIR, "/", figname, ".png", sep = "")
  print(p)
  # dev.copy(png, f.fig)
  if (remove.legend == TRUE) {
    plot.w <- 6.5
    plot.h <- 6
  } else {
    plot.w <- 7.7
    plot.h <- 6
  }
  ggsave(f.fig, width = plot.w, height = plot.h, dpi = res)
  # dev.off()
}

get.fig.name <- function(main, prefix = NULL, suffix1 = NULL, suffix2 = NULL) {
  fig.name <- main
  if (!is.null(prefix)) fig.name <- paste(prefix, fig.name, sep = "_")
  if (!is.null(suffix1)) fig.name <- paste(fig.name, suffix1, sep = "_")
  if (!is.null(suffix2)) fig.name <- paste(fig.name, suffix2, sep = "_")
  return(fig.name)
}

############################################
############################################
# Main
############################################
############################################

#####################################
# Display main settings and confirm operation
#####################################

if (interactive() == FALSE) {
  yes <- c("y", "Y", "Yes", "yes")

  cat("Plot NMDS figures with following settings:\n")
  cat("  Project: ", PROJ, "\n", sep = "")
  cat("  Assessment: ", ASSESS, "\n", sep = "")
  cat("  Grouping class: ", veg.grouping.class, "\n", sep = "")
  cat("  Input directory: ", INPUTDIR, "\n", sep = "")
  cat("  Output directory: ", FIGDIR, "\n", sep = "")
  cat("  Input file: ", F.SITE.SCORES, "\n", sep = "")
  cat("  Label outliers: ", show.outliers, "\n", sep = "")
  cat("  Outlier std. deviations: ", sd.mult, "\n", sep = "")
  cat("  Label all: ", show.sites, "\n", sep = "")
  cat("  Repel labels? ", labels.repel, "\n", sep = "")
  cat("  Remove legend? ", remove.legend, "\n", sep = "")
  cat("Continue? (y/n):")
  response <- readLines("stdin", n = 1)
  if (!response %in% yes) stop_quietly("Operation cancelled\n\n")
  cat("\n")
  cat("--------------------------------------------\n")
  cat("Plotting NMDS figures\n")
  cat("--------------------------------------------\n")
}

# Create figure directories if not exist
dir.create(file.path(FIG_BASEDIR), showWarnings = FALSE)
dir.create(file.path(FIG_NMDS_DIR), showWarnings = FALSE)
if (bm.only == TRUE) {
  suppressWarnings(dir.create(file.path(BM.ONLY.DIR)))
}
if (show.outliers == TRUE || show.sites == TRUE) {
  suppressWarnings(dir.create(file.path(FIGDIR)))
}

##########################################################
# Load NMDS site score file
##########################################################

# NMDS site scores
file <- paste(INPUTDIR, F.SITE.SCORES, sep = "")
site.scores.all <- read.csv(file, header = TRUE)
site.scores.all.orig <- site.scores.all

if (veg.grouping.class == "landCover") {
  names(site.scores.all)[names(site.scores.all) == "landCover"] <- "veg"
} else if (veg.grouping.class == "vegClass") {
  names(site.scores.all)[names(site.scores.all) == "vegClass"] <- "veg"
} else {
  msg.err <- paste0("ERROR: unknown value of veg.grouping.class '", veg.grouping.class, "'\n")
  stop(msg.err)
}

names(site.scores.all)[names(site.scores.all) == "plotCode"] <- "plot"
site.scores.all$focalOrBenchmark <- tolower(site.scores.all$focalOrBenchmark)
veg.all <- unique(site.scores.all$veg)

# The following is critical to prevent f'ing R from plotting
# factor numbers instead of actual values
site.scores.all$plot <- as.character(site.scores.all$plot)
site.scores.all$veg <- as.character(site.scores.all$veg)

if (!interactive()) cat("\nSaving figures to ", FIGDIR, ":\n", sep = "")

for (i in 1:length(veg.all)) { # START landcover loop
  # for ( i in 1:1) {	# for testing with loop
  # i=22					# for testing without loop
  veg <- veg.all[i]
  site.scores <- site.scores.all[site.scores.all$veg == veg, ]
  site.scores$outlier <- NA
  if (is.null(dims.custom)) {
    # Use empirical number of dimension from NMDS results
    dims <- unique(site.scores$dim)[1] # should only be one, but just in case
  } else {
    dims <- dims.custom
  }


  if (show.outliers == TRUE) {
    ############################
    # Detect & flag potential outliers
    ############################

    site.scores$outlier <- FALSE

    mean <- mean(site.scores$x)
    sds <- sd(site.scores$x) * sd.mult
    x.upper <- mean + sds
    x.lower <- mean - sds

    mean <- mean(site.scores$y)
    sds <- sd(site.scores$y) * sd.mult
    y.upper <- mean + sds
    y.lower <- mean - sds

    if (dims > 2) {
      mean <- mean(site.scores$z)
      sds <- sd(site.scores$z) * sd.mult
      z.upper <- mean + sds
      z.lower <- mean - sds

      site.scores$outlier[
        site.scores$x > x.upper | site.scores$x < x.lower |
          site.scores$y > y.upper | site.scores$y < y.lower |
          site.scores$z > z.upper | site.scores$z < z.lower
      ] <- TRUE
    } else {
      site.scores$outlier[
        site.scores$x > x.upper | site.scores$x < x.lower |
          site.scores$y > y.upper | site.scores$y < y.lower
      ] <- TRUE
    }
  }

  ########################################
  # Set land cover-specific graph parameters
  ########################################

  title.text <- paste0("NMDS: ", veg) # NULL to omit

  ##########################################################
  # Plot the results
  ##########################################################

  # Axis 1 vs 2
  df.plot <- site.scores[, c("x", "y", "focalOrBenchmark", "plot", "outlier")] # Extract subset
  if (bm.only == TRUE) df.plot <- df.plot[df.plot$focalOrBenchmark == "b", ]
  colnames(df.plot) <- c("a1", "a2", "grp", "site", "outlier") # Apply required col names

  p <- nmds.plot(df.plot,
    a1name = "Axis 1", a2name = "Axis 2", grp.name = "Focal or Benchmark",
    show.sites = show.sites, site.type.name = "plot", show.outliers = show.outliers,
    title.main = title.text, title.bold = title.bold, title.fontsize = title.fontsize,
    axis.fontsize = axis.fontsize, axis.tick.fontsize = axis.fontsize,
    colors.fb = colors.fb, use.colors.fb = use.colors.fb, leg.title = leg.title,
    labels.repel = labels.repel, remove.legend = remove.legend
  )
  lc.text <- unix.friendly(veg)
  coords <- "xy"
  figname <- get.fig.name(
    main = lc.text, prefix = FIGBASENAME,
    suffix1 = coords, suffix2 = FIGSUFFIX
  )
  if (!interactive()) cat("  ", figname, ".png", "\n", sep = "")
  save.plot(p, f.name = figname, dir.name = FIGDIR, res = res)

  if (dims > 2) {
    # Axis 1 vs 3
    df.plot <- site.scores[, c("x", "z", "focalOrBenchmark", "plot", "outlier")] # Extract subset
    if (bm.only == TRUE) df.plot <- df.plot[df.plot$focalOrBenchmark == "b", ]
    colnames(df.plot) <- c("a1", "a2", "grp", "site", "outlier") # Apply required col names

    p <- nmds.plot(df.plot,
      a1name = "Axis 1", a2name = "Axis 3", grp.name = "Focal or Benchmark",
      show.sites = show.sites, site.type.name = "plot", show.outliers = show.outliers,
      title.main = title.text, title.bold = title.bold, title.fontsize = title.fontsize,
      axis.fontsize = axis.fontsize, axis.tick.fontsize = axis.fontsize,
      colors.fb = colors.fb, use.colors.fb = use.colors.fb, leg.title = leg.title,
      labels.repel = labels.repel, remove.legend = remove.legend
    )
    coords <- "xz"
    figname <- get.fig.name(
      main = lc.text, prefix = FIGBASENAME,
      suffix1 = coords, suffix2 = FIGSUFFIX
    )
    if (!interactive()) cat("  ", figname, ".png", "\n", sep = "")
    save.plot(p, f.name = figname, dir.name = FIGDIR, res = res)

    # Axis 2 vs 3
    df.plot <- site.scores[, c("y", "z", "focalOrBenchmark", "plot", "outlier")] # Extract subset
    if (bm.only == TRUE) df.plot <- df.plot[df.plot$focalOrBenchmark == "b", ]
    colnames(df.plot) <- c("a1", "a2", "grp", "site", "outlier") # Apply required col names

    p <- nmds.plot(df.plot,
      a1name = "Axis 2", a2name = "Axis 3", grp.name = "Focal or Benchmark",
      show.sites = show.sites, site.type.name = "plot", show.outliers = show.outliers,
      title.main = title.text, title.bold = title.bold, title.fontsize = title.fontsize,
      axis.fontsize = axis.fontsize, axis.tick.fontsize = axis.fontsize,
      colors.fb = colors.fb, use.colors.fb = use.colors.fb, leg.title = leg.title,
      labels.repel = labels.repel, remove.legend = remove.legend
    )
    coords <- "yz"
    figname <- get.fig.name(
      main = lc.text, prefix = FIGBASENAME,
      suffix1 = coords, suffix2 = FIGSUFFIX
    )
    if (!interactive()) cat("  ", figname, ".png", "\n", sep = "")
    save.plot(p, f.name = figname, dir.name = FIGDIR, res = res)
  }

  # Compile df of outliers
  if (i == 1) df.outliers <- site.scores[0, ]
  df.outliers <- rbind(df.outliers, site.scores[site.scores$outlier == TRUE, ])


  # # Hard-wired hack
  # # Comment out when done but keep for future adaptation
  # if ( site.scores$veg == "Mesic forest, Old") {
  # cat("    Saving visual outliers for ", unique(site.scores$veg), "'\n", sep="")
  # df.outliers.visual <- site.scores[ site.scores$x>2, ]
  # filename <- paste0(INPUTDIR, "nmds.outliers.visual.csv")
  # write.csv(df.outliers.visual, file=filename, row.names=FALSE)
  # }
} # END landCover loop

if (show.outliers == TRUE) {
  # Save outlier file from this run
  if (!interactive()) cat("Saving outlier files to ", INPUTDIR, ":\n", sep = "")
  pf.outliers <- paste0(INPUTDIR, F.NMDS.OUTLIERS)
  write.csv(df.outliers, file = pf.outliers, row.names = FALSE)
  if (!interactive()) cat("  ", F.NMDS.OUTLIERS, "\n", sep = "")

  # ###################
  # # Combine outliers with
  # # previous, if applicable
  # ###################

  # # Path + file names of compiled outlier results and plots files
  # pf.outliers.compiled <- paste0(INPUTDIR, f.outliers.compiled)
  # pf.outlier.plots.compiled <- paste0(INPUTDIR, f.outlier.plots.compiled)

  # # DF of outlier plots and land cover only
  # # df.outlier.plots <- df.outliers[ ,
  # # c("veg", "landCover", "focalOrBenchmark", "plot")
  # # ]
  # df.outlier.plots <- df.outliers[ ,
  # c("veg", "vegClass", "focalOrBenchmark", "plot")
  # ]

  # # Append to existing compiled files or start new
  # if ( file.exists(pf.outliers.compiled) ) {
  # # Append to existing files
  # df.outliers.compiled <- read.csv(pf.outliers.compiled,
  # header=TRUE)
  # df.outliers.compiled <- rbind( df.outliers.compiled, df.outliers )
  # df.outlier.plots.compiled <- read.csv(pf.outlier.plots.compiled,
  # header=TRUE)
  # df.outlier.plots.compiled <- rbind( df.outlier.plots.compiled, df.outlier.plots )

  # # Make dfs unique
  # df.outlier.plots <- distinct( df.outlier.plots )
  # df.outlier.plots.compiled <- distinct( df.outlier.plots.compiled )
  # } else {
  # # Start new files
  # df.outliers.compiled <- df.outliers
  # df.outlier.plots.compiled <- df.outlier.plots
  # }
  # # Save the files
  # if (!interactive()) cat( "  ", f.outliers.compiled, "\n", sep="" )
  # write.csv(df.outliers.compiled, file=pf.outliers.compiled,
  # row.names=FALSE)
  # if (!interactive()) cat( "  ", f.outlier.plots.compiled, "\n\n", sep="" )
  # write.csv(df.outlier.plots.compiled, file=pf.outlier.plots.compiled,
  # row.names=FALSE)
}

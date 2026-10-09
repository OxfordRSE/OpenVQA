##############################################
# Remove NMDS outlier plots from VQA input files
#
# Requires files: [path/file: action]
# 		inputs/coverByGrowthForm.csv: remove plots
# 		inputs/exoticCoverByStratum.csv: remove plots
# 		inputs/groundCover.csv: remove plots
# 		inputs/plotMetadata.csv: remove plots
# 		inputs/speciesCover.csv: remove plots
# 		inputs/focal_summary.csv: adjust sample sizes
# 		results/nmds_outliers_compiled.csv: outlier plots to remove
#
# Notes:
# 		1. 	MUST be run as part of VQA pipeline using script
# 			vqa.batch.[proj].R to load parameters
#
# By: Brad Boyle
# Email: brad@hg-llc.com
# Created: 19 May 2023
##########################################################

# clear the workspace & set wd
rm(list = ls())
wd <- getwd()
setwd(wd)

####################################
# Parameters
####################################

# Load project/assessment parameters and libraries
# params.file <- "params.R"					# Params file set dynamically
params.file <- "params.vqa-pub.R" # Set manually
source(params.file)

# Set file names
f.outliers.compiled <- "nmds_outlier_plots_compiled.csv"
input.files <- c(
  "coverByGrowthForm.csv",
  "exoticCoverByStratum.csv",
  "groundCover.csv",
  "plotMetadata.csv",
  "speciesCover.csv"
)
f.sample.size <- "focal_summary.csv"

####################################
# Functions
####################################

source("includes/functions.R", local = TRUE)

####################################
# Main
####################################

cat("-----------------------------\n")
cat("Removing NMDS outliers for proj='", proj, "', assess='", assess, "'\n", sep = "")
cat("-----------------------------\n\n")

# Make vector of outlier plots
pf.outliers.compiled <- paste0(RESULTSDIR, f.outliers.compiled)

if (file.exists(pf.outliers.compiled)) { # START check-outlier-file-exists [non-indented]
  df.outliers.compiled <- read.csv(pf.outliers.compiled, header = TRUE)
  n.outliers <- nrow(df.outliers.compiled)
  if (n.outliers > 0) { # START check-outliers-exist [non-indented]
    cat(n.outliers, " outlier plots found\n", sep = "")
    outlier.plots <- unique(df.outliers.compiled$plot)

    cat("Removing outliers from input files:\n")
    for (f.input in input.files) {
      cat("  ", f.input, "...", sep = "")
      pf.input <- paste0(INPUTDIR, f.input)

      # backup the file if this is the first run
      pf.input.bak <- paste0(pf.input, ".bak")
      if (!file.exists(pf.input.bak)) file.copy(pf.input, pf.input.bak)

      # Read in file, delete outliers, and save
      df.input <- read.csv(pf.input, header = TRUE)
      df.input <- df.input[!df.input$plotCode %in% outlier.plots, ]
      write.csv(df.input, file = pf.input, row.names = FALSE)

      # Save counts of total plots by land cover if plotMetdata file
      if (f.input == "plotMetadata.csv") {
        df.plots <- df.input[, c(
          "plotCode", "focalOrBenchmark", "landCoverCode", "targetVegCode"
        )]
        colnames(df.plots) <- c("plotCode", "focalOrBenchmark", "landCover", "vegClass")

        df.plots$cnt <- 1

        # focal plots
        df.n.f <- aggregate(
          cnt ~ landCover,
          data = df.plots[df.plots$focalOrBenchmark == "f", ],
          FUN = sum,
          na.rm = TRUE,
          na.pass = NULL
        )
        # colnames(df.n.f) <- c("landCover", "n.f")
        colnames(df.n.f) <- c("landCoverCode", "n.f")

        # benchmark plots
        df.n.b <- aggregate(
          cnt ~ vegClass,
          data = df.plots[df.plots$focalOrBenchmark == "b", ],
          FUN = sum,
          na.rm = TRUE,
          na.pass = NULL
        )
        # colnames(df.n.b) <- c("vegClass", "n.b")
        colnames(df.n.b) <- c("targetVegCode", "n.b")
      }
      cat("done\n")
    }

    # Update plot counts in summary file
    cat("Updating plot counts in summary file '", f.sample.size, "'...", sep = "")
    pf.sample.size <- paste0(INPUTDIR, f.sample.size)
    if (!file.exists(pf.sample.size)) stop("File '", f.sample.size, "' missing!")

    # backup the file if this is the first run
    pf.sample.size.bak <- paste0(pf.sample.size, ".bak")
    if (!file.exists(pf.sample.size.bak)) file.copy(pf.sample.size, pf.sample.size.bak)

    # Read in file & add updated counts
    df.sample.size <- read.csv(pf.sample.size, header = TRUE)
    df.sample.size <- merge(df.sample.size, df.n.f, by = "landCoverCode", all.x = TRUE)
    df.sample.size <- merge(df.sample.size, df.n.b, by = "targetVegCode", all.x = TRUE)

    # Rename the columns and save
    drop.cols <- c("focal_plots", "bm_plots")
    df.sample.size <- df.sample.size[, !names(df.sample.size) %in% drop.cols]
    names(df.sample.size)[names(df.sample.size) == "n.f"] <- "focal_plots"
    names(df.sample.size)[names(df.sample.size) == "n.b"] <- "bm_plots"
    write.csv(df.sample.size, file = pf.sample.size, row.names = FALSE)
    cat("done\n")

    # Conclusion of non-indented outlier checks
  } else {
    cat("No outliers to remove!\n")
  } # END check-outliers-exist [non-indented]
} else {
  cat("No outliers to remove!\n")
} # END check-outlier-file-exists [non-indented]

cat("\nOperation concluded\n")
cat("-----------------------------\n\n")

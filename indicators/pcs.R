#######################################################
# Quality Hectares Ecological Indicator (EI) Calculation
# Percent Cover by Height Stratum (PCS)
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
# 9 April 2013
#
# *** NOTE ***
# This indicator is essentially identical to PCGF, but will generally
# be used for height class strata rather than growth form strata.
# Two separate indicators allows these two similar indicators
# to be included in the same analysis. The input df is identical in
# structure to PCGF but with different strata. Recommend calling it
# coverByStratum.csv
#
# Purpose:
# Calculates percent cover of all vegetation within each stratum,
# relative to sample of benchmark sites from same (target) vegetation
# class. Relative scores are calculated separately for each stratum in
# each focal site, and the average used to discount the final
# quality coefficient.
#
# Strata can be size class, growth form, whatever.
#
# This EI presents special challenges as bm scores of zero (no cover
# within stratum) are common. For example, no trees in grassland.
# Quality is delegated to the configured shared benchmark routines; preparation
# does not apply an arcsine-square-root transformation or a 0.15 cutoff.
#
# Input coverByStratum.csv contains raw cover and plot metadata:
# 		plotCode: 						unique alphanumeric code for plot
# 		focalOrBenchmark: 	'b' for benchmark, 'f' for focal site
# 		vegClass: 							short code for vegetation class or category
# 		stratum: 							size class (stratum)
# 		cover: cover of all vegetation this stratum
# Note: percentages in field "cover" MUST be converted
#   to proportions!
#######################################################


# Prepare raw PCS values without file I/O; retain the established input and transformation rules.
pcs_data <- function(inputs, config) {
  required <- c("coverByStratum.csv", "ei.stratum.include.csv", "ei.stratum.veg.include.csv")
  if (any(vapply(inputs[required], is.null, logical(1)))) {
    stop("PCS requires: ", paste(required, collapse = ", "), call. = FALSE)
  }
  settings <- resolve_params(config$indicator_defaults, config$indicator_settings$PCS)
  convert.percent <- settings$convert_percent
  remove.zero.cover.plots <- settings$remove_zero_cover_plots
  n.min <- 6

  cat("Preparing raw data from scratch:\n")

  #######################################################
  # Load raw data
  #######################################################

  cat("- Importing raw data...")
  # Import table containing cover values of all species each plot,
  # plus vegetation class by stratum (growth form)
  dat.raw <- inputs[["coverByStratum.csv"]]
  dat <- dat.raw

  # Remove rows with missing data
  n.before <- nrow(dat)
  dat <- dat[!is.na(dat$cover), ]
  n.after <- nrow(dat)

  n.diff <- n.before - n.after
  if (n.diff > 0) {
    cat("WARNING: ", n.diff, " of ", n.before, " rows deleted due to missing data!\n")
  }
  cat("done\n")

  #########################
  # Check for all-zero cover plots
  # and remove if applicable
  #########################

  if (remove.zero.cover.plots == TRUE) {
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
    perc.deleted <- format(round(plots.deleted / n.plots.before * 100, 1), nsmall = 1)

    # Calculate remaining f sample sizes per vegetation class
    # and warn if any classes < n.min plots
    f.plots <- unique(dat[dat$focalOrBenchmark == "f", c("plot", "landCover")])
    f.plots$cnt <- 1
    veg.n.f <- aggregate(
      cnt ~ landCover,
      data = f.plots,
      FUN = sum
    )
    colnames(veg.n.f) <- c("landCover", "plots")
    veg.n.f.low <- veg.n.f$landCover[veg.n.f$cnt < 6]
    veg.n.f.low.cnt <- length(veg.n.f.low)

    # Calculate remaining bm sample sizes per vegetation class
    # and warn if any classes < n.min plots
    b.plots <- unique(dat[dat$focalOrBenchmark == "b", c("plot", "vegClass")])
    b.plots$cnt <- 1
    veg.n.b <- aggregate(
      cnt ~ vegClass,
      data = b.plots,
      FUN = sum
    )
    colnames(veg.n.b) <- c("vegClass", "plots")
    veg.n.b.low <- veg.n.b$vegClass[veg.n.b$cnt < 6]
    veg.n.b.low.cnt <- length(veg.n.b.low)

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

    if (veg.n.f.low.cnt > 0) {
      msg <- paste0(
        "WARNING: ", veg.n.f.low.cnt, " land cover classes have less than n.min=",
        n.min, " focal plots!\n"
      )
      cat(msg)
    }
    if (veg.n.b.low.cnt > 0) {
      msg <- paste0(
        "WARNING: ", veg.n.b.low.cnt, " bm vegetation classes have less than n.min=",
        n.min, " bm plots!\n"
      )
      cat(msg)
    }
  } # END remove.zero.cover.plots

  # Capture the observed plot universe before stratum exclusions. The shared
  # inclusion rules complete missing included plot-stratum combinations with zeros.
  plots <- unique(dat$plotCode)
  dat <- complete_cover_strata(dat, plots, inputs, "PCS")

  ###########################################################
  # Validations and transformations
  ###########################################################

  cat("- Completing transformations...")

  # get all unique plots across entire data set
  plots <- unique(dat$plotCode)

  # Check plotCodes are unique across both benchmark and focal data.
  # If not, stop and edit plot codes in original data matrix to ensure uniqueness
  allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])
  if (length(plots) != nrow(allPlots)) stop("Plot codes not unique!")

  dat <- dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "cover")]
  colnames(dat) <- c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI")
  dat$EI.tr <- NA

  # Percentages must become proportions. Truncate values above one for beta
  # fitting; PCS always disables abundance scaling and logit transformation.
  if (convert.percent) dat$EI <- dat$EI / 100
  dat$EI[dat$EI > 1] <- 1
  dat$EI.tr <- dat$EI
  cat("done\n")
  dat
}

# Write raw, benchmark, focal and bootstrap results, then generate configured figures.
run_pcs <- function(inputs, config, output_paths, seed = config$seed) {
  config$seed <- seed
  nodata <- unlist(config$vegetation_without_data, use.names = FALSE)
  if (!length(nodata)) nodata <- ""
  curr.EI <- "PCS" # Short code

  ##############################
  # Input file for this indicator
  ##############################
  inputFileName <- "coverByStratum.csv"

  ##############################
  # Load attributes for this indicator
  # (ei.params set in params file)
  ##############################

  # Import parameters set in params file
  ei.pars <- indicator_parameters(curr.EI, config)

  # Extract the indicator values
  ei.name <- ei.pars$name # Full name of this EI, for graphics

  prepare.raw <- ei.pars$prepare_raw # Prepare raw data from scratch?
  convert.percent <- ei.pars$convert_percent # Convert percent cover to proportions
  distn <- ei.pars$distribution # Distribution of the EI
  q.method <- ei.pars$quality_method # Quality method: "fixed" | "empirical" (overlap)
  bm.val <- ei.pars$benchmark_value # Applies only if q.method="fixed"
  test.tail <- ei.pars$test_tail # Test tail for this EI
  has.strata <- ei.pars$has_stratum # True if this EI has strata, otherwise F
  has.stratum <- has.strata # Hack for backward compatibility
  remove.zero.cover.plots <- ei.pars$remove_zero_cover_plots
  scale.abund <- ei.pars$scale_abundance
  logit <- ei.pars$logit

  if (has.strata == F) curr.stratum <- ""

  # Sample-size threshold used in the low-count warning messages.
  n.min <- 6

  ##############################
  # Indicator-specific graphing parameters
  ##############################

  allow.group.strata <- TRUE # Set to true to make extra set of figures grouping
  # figures for different strata of same vegetation in
  # single figure

  # Set to true to print sample size for each panel in grouped figures
  # Ignored if allow.group.strata==FALSE
  group.strata.print.all.n <- FALSE

  # Minimum value for y axis
  # Leave as empty string ("") to allow to vary automatically
  y.axis.fixed.min <- 0

  # Include landcover in graph title?
  # If true, title = landcover + stratum
  # If false, title = stratum only
  # Applies to single stratum graphs only
  lc.in.title <- TRUE

  ##############################
  # Names and locations output files
  ##############################
  rawFileName <- paste(curr.EI, "_raw.csv", sep = "") # Complete raw data with transformations
  resultsFocal <- paste(curr.EI, "_focal.csv", sep = "")
  resultsBm <- paste(curr.EI, "_benchmark.csv", sep = "")

  ##############################
  # Transformation options
  ##############################

  # Must be TRUE for this indicator
  proportions <- TRUE

  # Summed cover can exceed 100% within a stratum. PCS disables abundance
  # scaling and truncates those values rather than rescaling the sample.
  scale.abund <- FALSE

  # Truncation above one keeps proportions in the beta-distribution domain.
  truncate.at.one <- TRUE

  # PCS disables logit transformation; EI.tr retains the truncated proportions.
  logit <- FALSE

  #######################################################
  # Main
  #######################################################

  cat("\n##################################################\n")
  cat("Operation: Calculating Quality for indicator group PCS\n")
  cat("(Percent Cover by Height Class Stratum)\n")
  cat("##################################################\n\n")

  if (config$plot_figures_only == FALSE) {
    cat("************************\n")
    cat("Preparing data\n")
    cat("************************\n")

    if (prepare.raw == TRUE || config$force_prepare_raw == TRUE) { # START prepare.raw
      dat <- pcs_data(inputs, config)
      write.csv(dat, paste0(paste0(output_paths$results, "/"), rawFileName), row.names = FALSE)
    } # END prepare.raw

    #######################################################
    # Calculate and save benchmark values
    #######################################################

    # Re-import prepared raw data
    inputFile <- paste(paste0(output_paths$results, "/"), rawFileName, sep = "")
    cat("inputFile=", inputFile)
    dat <- read.csv(inputFile, header = T)
    dat <- dat

    # Make data frame of moments for transformed cover
    # aggregated by vegclass + stratum
    EI.bm <- subset(dat, dat$focalOrBenchmark == "b")

    EI.bm <- with(EI.bm, aggregate(EI.tr, list(vegClass = vegClass, stratum = stratum),
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
    colnames(EI.bm) <- c(
      "vegClass", "stratum", "n", "EI.mean",
      "EI.med", "EI.sd", "EI.max", "EI.min"
    )

    # Add EI & rearrange the columns
    EI.bm$EI <- curr.EI # add indicator
    EI.bm <- EI.bm[, c(
      "EI", "vegClass", "stratum", "n",
      "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min"
    )]

    # Make final, human-readable veg classes
    EI.bm$vegClass <- human.readable(EI.bm$vegClass)

    # Inspect the columns of EI.bm
    sorted.EI.bm <- EI.bm[order(EI.bm$vegClass, EI.bm$stratum), ]
    sorted.EI.bm[1:12, ]

    # export benchmark site file
    resultsFile <- paste(paste0(output_paths$results, "/"), resultsBm, sep = "")
    write.csv(EI.bm, file = resultsFile, row.names = FALSE)

    ####################################################
    # Prepare (focal) results file, and populate basic summary stats
    ####################################################

    result <- prepare_indicator_results(read_indicator_output(
      curr.EI, config,
      output_paths, allow.group.strata
    ), config, output_paths)

    ####################################################
    # Calculate quality
    ####################################################

    result <- calculate_indicator_quality(result, config, output_paths)
  } else {
    cat("************************\n")
    cat("Generating figures\n")
    cat("************************\n")
  } # END PLOT.FIGS.ONLY

  ####################################################
  # Plot overlap
  ####################################################

  if (config$plot_figures_only) {
    result <- read_indicator_output(
      curr.EI, config,
      output_paths, allow.group.strata
    )
  }
  if (config$plot_overlap == T) plot_indicator_overlap(result, config, output_paths)

  ####################################################
  # Graph the fitted distributions for each vegetation type
  # Include model statistic and overlap scores on figures if requested
  ####################################################

  plot_indicator_distributions(result, config, output_paths)

  cat("\n##################################################\n")
  cat("Operation completed\n")
  cat("##################################################\n\n")

  if (config$plot_figures_only) {
    return(invisible(NULL))
  }
  invisible(list(
    raw = read.csv(file.path(output_paths$results, "PCS_raw.csv")),
    focal = read.csv(file.path(output_paths$results, "PCS_focal.csv")),
    benchmark = read.csv(file.path(output_paths$results, "PCS_benchmark.csv"))
  ))
}

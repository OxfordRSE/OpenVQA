#######################################################
# Indicator value preparation and quality calculations
# for ecological indicator Basal Area Exotic Species (BAES):
# sum of ba_m2 * the configured multiplier for exotic stems in each plot.
#
# Multiplication changes the scale before gamma-distribution fitting; no
# sampling-area normalization is performed. Output units depend on that multiplier.
#
# By: Brad Boyle
# Email: boyle@hg-llc.com
# Created: 23 February 2025
#######################################################


# Prepare raw BAES values without file I/O; retain the established input and transformation rules.
exotic_basal_area_data <- function(inputs, config) {
  required <- c("plotMetadata.csv", "speciesStems.csv", "species.csv")
  if (any(vapply(inputs[required], is.null, logical(1)))) {
    stop("BAES requires: ", paste(required, collapse = ", "), call. = FALSE)
  }
  settings <- resolve_params(config$indicator_defaults, config$indicator_settings$BAES)
  exclude.exotics <- settings$exclude_exotics
  ei.multiplier <- settings$multiplier


  cat("Loading raw data:\n")

  # Import plot data
  cat("  Plot data...")
  plot.dat <- inputs[["plotMetadata.csv"]]
  cat("done\n")

  # Import stem data
  cat("  Plot data...")
  dat <- inputs[["speciesStems.csv"]]
  dat <- dat
  cat("done\n")

  # Import species list and update species data with native status
  cat("  Species native status")
  df.species <- inputs[["species.csv"]]
  names(df.species)[names(df.species) == "isExotic"] <- "is_exotic" # rename just in case
  dat <- merge(dat, df.species[, c("species", "is_exotic")], by = "species", all.x = TRUE)

  if (config$missing_exotic_status_assume_native == TRUE) {
    # Will also mark "indet_sp." as native, but not a problem
    dat$is_exotic[is.na(dat$is_exotic)] <- 0
  }

  if (nrow(dat[is.na(dat$is_exotic), ])) {
    # Quit and warn about missing values
    stop("ERROR: value of 'is_exotic' missing for one or more species. Please correct.\n")
  }
  cat("done\n")

  # Exclude native species
  cat("Filtering data to exotic species only...")
  dat <- dat[dat$is_exotic == 1, ]
  cat("done\n")

  ######################################
  # Preliminary validations & standardizations
  ######################################

  # Check that plotCodes are unique across entire dataset.
  # If not, throw error message and stop
  plots <- unique(dat$plotCode)
  allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])
  if (length(plots) != nrow(allPlots)) stop("Plot codes not unique!")

  ######################################
  # Transformations
  ############################

  dat$ba <- dat$ba_m2 * ei.multiplier

  ############################
  # Sum basal area per plot
  ############################

  cat("Calculating  total BA per plot...")
  # Calculate total BA per plot
  ba.plot <- aggregate(
    ba ~ plotCode,
    data = dat[, c("plotCode", "ba")],
    FUN = sum
  )
  colnames(ba.plot) <- c("plotCode", "ba")

  # Add plot metadata separately from full plot df
  # This approach ensures that all plots end up in final matrix,
  # even those with zero basal area
  dat <- merge(
    plot.dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")],
    ba.plot,
    by = "plotCode",
    all.x = TRUE
  )
  if (nrow(dat[is.na(dat$ba), ]) > 0) {
    dat$ba[is.na(dat$ba)] <- 0
  }
  cat("done\n")

  ################################
  # Validations
  ################################

  cat("Performing validations and transformations...")
  # Check plotCodes are unique across both benchmark and focal data.
  # If not, stop and edit plot codes in original data matrix to ensure uniqueness
  plots <- unique(dat$plotCode)
  allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])
  if (length(plots) != nrow(allPlots)) stop("Plot codes not unique!")
  cat("done\n")

  #######################################################
  # Add pseudo-stratum column
  #######################################################

  # Get column names
  cols <- colnames(dat)

  # Reorder columns, keeping stratum column if present,
  # and keeping the transformed EI column
  if (!any("stratum" %in% cols)) {
    has.stratum <- FALSE
    dat$stratum <- "nostrata"
  }

  #######################################################
  # Rename indicator column to generic "EI" & tidy up
  #######################################################

  # Rename indicator column
  names(dat)[names(dat) == "ba"] <- "EI"

  # Tidy up
  dat <- dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI")]
  dat$EI.tr <- dat$EI # No transformations needed; EI.tr same as EI
  dat
}

# Write raw, benchmark, focal and bootstrap results, then generate configured figures.
run_exotic_basal_area <- function(inputs, config, output_paths, seed = config$seed) {
  config$seed <- seed
  nodata <- unlist(config$vegetation_without_data, use.names = FALSE)
  if (!length(nodata)) nodata <- ""
  curr.EI <- "BAES"

  ##############################
  # Names, locations of input file (raw data)
  ##############################

  # Get attributes for this indicator (set in params file)
  ei.pars <- indicator_parameters(curr.EI, config)
  ei.name <- ei.pars$name # Full name of this EI, for graphics

  source.file <- ei.pars$source_file # Stem measurement input filename
  prepare.raw <- ei.pars$prepare_raw # Prepare raw data from scratch?
  distn <- ei.pars$distribution # Distribution of the EI
  q.method <-
    ei.pars$quality_method # Quality method: "fixed" (fixed bm) | "empirical" (overlap)
  bm.val <- ei.pars$benchmark_value # Fixed bm value. Applies only if q.method="fixed"
  test.tail <- ei.pars$test_tail # Test tail for this EI
  has.strata <- ei.pars$has_stratum # True if this EI has strata, otherwise F
  if (has.strata == F) curr.stratum <- ""
  exclude.exotics <- ei.pars$exclude_exotics
  ei.multiplier <- ei.pars$multiplier

  ##############################
  # Indicator-specific graphing parameters
  ##############################

  # Group strata in single figure?
  # Set to true to make extra set of figures grouping
  # figures for different strata of same vegetation in
  # single figure. MUST be FALSE if no strata for this
  # indicator
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
  leg.x.extra <- -0.1
  leg.y.extra <- NA

  # Grouped figure legend additional offset NOT YET IMPLEMENTED!
  # Positive: move to right, negative: move to left
  # Comment out or set to "" or NA to use defaults
  leg.x.extra.g <- -0.2
  leg.y.extra.g <- NA

  ##############################
  # Names & locations of output files
  ##############################

  rawFileName <- paste(curr.EI, "_raw.csv", sep = "") # Validated raw data
  resultsFocal <- paste(curr.EI, "_focal.csv", sep = "") # Focal indicator file
  resultsBm <- paste(curr.EI, "_benchmark.csv", sep = "") # Bm indicator file

  #######################################################
  # Main
  #######################################################

  cat("\n")
  cat("###########################################################\n")
  cat("Prepare raw indicator values for indicator ", curr.EI, " (", ei.name, ")\n", sep = "")
  cat("###########################################################\n\n")

  if (config$plot_figures_only == FALSE) {
    cat("########################\n")
    cat("Preparing data\n")
    cat("########################\n\n")

    if (prepare.raw == TRUE || config$force_prepare_raw == TRUE) { # START prepare.raw
      dat <- exotic_basal_area_data(inputs, config)
      write.csv(dat, paste0(paste0(output_paths$results, "/"), rawFileName), row.names = FALSE)
    } # END prepare.raw

    #######################################################
    # Calculate and save benchmark values
    #######################################################

    # Re-import prepared raw data
    cat("Re-importing prepared raw data:\n", sep = "")
    inputFile <- paste(paste0(output_paths$results, "/"), rawFileName, sep = "")
    cat("  inputFile='", inputFile, "'...done\n", sep = "")
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
    EI.bm <- EI.bm[, c(
      "EI", "vegClass", "stratum", "n",
      "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min"
    )]

    # Make final, human-readable veg classes
    EI.bm$vegClass <- human.readable(EI.bm$vegClass)

    # Inspect the columns of EI.bm
    sorted.EI.bm <- EI.bm[order(EI.bm$vegClass), ]
    head(sorted.EI.bm)

    # save benchmark results file
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
    cat("########################\n")
    cat("Generating figures\n")
    cat("########################\n\n")
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
  # Graph the distributions for each vegetation type
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
    raw = read.csv(file.path(output_paths$results, "BAES_raw.csv")),
    focal = read.csv(file.path(output_paths$results, "BAES_focal.csv")),
    benchmark = read.csv(file.path(output_paths$results, "BAES_benchmark.csv"))
  ))
}

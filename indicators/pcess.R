# Quality Hectares Ecological Indicator: Percent Exotic Cover by Stratum (PCESS)
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
# 9 April 2013
#
# Purpose:
# Calculate exotic-species cover within each stratum relative to benchmark sites of
# the same target vegetation class. Scores are calculated separately for each stratum
# and contribute to the final quality coefficient.
#
# Input: exoticCoverByStratum.csv, plot metadata, and stratum inclusion tables.
# Cover rows contain plotCode (unique across focal and benchmark plots),
# focalOrBenchmark ('b' or 'f'), landCover, vegClass (target benchmark vegetation),
# stratum, and cover. Where exotic cover is derived from species records, prepare
# this input by summing the cover of exotic species within each plot and stratum.
# Percentages must be converted to proportions using convert_percent. Summed cover
# can exceed one; this calculation truncates it at one rather than scaling abundance.

# Prepare the raw indicator table without writing files; EI.tr holds transformed EI.
pcess_data <- function(inputs, config) {
  required <- c(
    "exoticCoverByStratum.csv", "plotMetadata.csv", "ei.stratum.include.csv",
    "ei.stratum.veg.include.csv"
  )
  if (any(vapply(inputs[required], is.null, logical(1)))) {
    stop("PCESS requires: ", paste(required, collapse = ", "), call. = FALSE)
  }
  INCLUDE.PLOTS.NODATA <- config$include_plots_nodata
  settings <- resolve_params(config$indicator_defaults, config$indicator_settings$PCESS)
  curr.EI <- "PCESS"
  inputFileName <- "exoticCoverByStratum.csv"
  convert.percent <- settings$convert_percent
  na.to.zero <- settings$na_to_zero

  cat("Preparing raw data from scratch:\n")
  cat("- Importing raw data...")
  dat <- inputs[[inputFileName]]
  plotMetadata <- inputs[["plotMetadata.csv"]]
  cat("- Checking for missing values of cover...")
  # Missing cover follows na_to_zero: replace with zero or delete the row.
  n_NA <- nrow(dat[is.na(dat$cover), ])
  if (n_NA == 0) {
    cat("no NAs found\n")
  } else {
    cat(n_NA, " NAs found!\n", sep = "")
    if (na.to.zero) {
      cat("- Setting NAs to 0...")
      dat$cover[is.na(dat$cover)] <- 0
    } else {
      cat("- Deleting ", n_NA, "rows with NA cover...", sep = "")
      dat <- dat[!is.na(dat$cover), ]
    }
    cat("done\n")
  }
  # Choose the plot universe before filtering excluded strata: either all metadata
  # plots or only plots represented in the cover input.
  if (INCLUDE.PLOTS.NODATA) {
    plots <- unique(plotMetadata$plotCode)
  } else {
    plots <- unique(dat$plotCode)
  }
  dat <- complete_cover_strata(dat, plots, inputs, curr.EI)
  cat("- Completing transformations...")
  plots <- unique(dat$plotCode)
  # Plot codes must identify a single plot across both benchmark and focal data.
  # Correct ambiguous codes in the input rather than combining the two samples.
  allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])
  if (length(plots) != nrow(allPlots)) stop("Plot codes not unique!")
  dat <- dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "cover")]
  colnames(dat) <- c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI")
  dat$EI.tr <- NA
  # Convert percentages to proportions when requested. Truncate values above one
  # for beta-distribution calculations; abundance scaling and logit are not applied.
  if (convert.percent) dat$EI <- dat$EI / 100
  dat$EI[dat$EI > 1] <- 1
  dat$EI.tr <- dat$EI
  cat("done\n")

  dat
}

# Calculate quality and write raw, benchmark, focal, bootstrap, and PDF tables,
# then generate the configured overlap and distribution figures. Return the raw,
# focal, and benchmark tables; figures-only runs use existing files and return NULL.
run_pcess <- function(inputs, config, output_paths, seed = config$seed) {
  state <- cover_indicator_state(inputs, config, output_paths, seed)
  evalq(
    {
      curr.EI <- "PCESS"
      # Indicator attributes select the distribution, quality method, fixed value, and test tail.
      ei.pars <- ei.params(curr.EI)
      ei.name <- ei.pars$ei.name
      EI.name <- ei.name
      prepare.raw <- ei.pars$prepare.raw
      distn <- ei.pars$distn
      q.method <- ei.pars$q.method
      bm.val <- ei.pars$bm.val
      test.tail <- ei.pars$test.tail
      has.strata <- ei.pars$has.stratum
      has.stratum <- has.strata
      if (has.strata == FALSE) curr.stratum <- ""
      # Plot different strata of the same vegetation class together in additional figures.
      allow.group.strata <- TRUE
      # Show sample sizes in every grouped panel when TRUE; ignored for ungrouped figures.
      group.strata.print.all.n <- FALSE
      # Minimum y-axis value; an empty string allows automatic limits.
      y.axis.fixed.min <- 0
      # Include land cover with the stratum in titles of single-stratum figures.
      lc.in.title <- TRUE
      # Established output filenames; the raw table includes EI and its transformed value EI.tr.
      rawFileName <- paste(curr.EI, "_raw.csv", sep = "")
      resultsFocal <- paste(curr.EI, "_focal.csv", sep = "")
      resultsBm <- paste(curr.EI, "_benchmark.csv", sep = "")

      cat("\n##################################################\n")
      cat("Operation: Calculating Quality for indicator group PCESS\n")
      cat("(Percent Cover Exotic Species by Stratum)\n")
      cat("##################################################\n\n")
      if (PLOT.FIGS.ONLY == FALSE) {
        cat("************************\n")
        cat("Preparing data\n")
        cat("************************\n")
        if (prepare.raw || force.prepare.raw) {
          dat <- pcess_data(inputs, config)
          write.csv(dat, paste0(RESULTSDIR, rawFileName), row.names = FALSE)
        }
        # Read back the prepared raw table, including when preparation is disabled.
        inputFile <- paste(RESULTSDIR, rawFileName, sep = "")
        cat("inputFile=", inputFile)
        dat <- read.csv(inputFile, header = T)
        # Summarize transformed benchmark cover by vegetation class and stratum.
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
          "vegClass", "stratum", "n", "EI.mean", "EI.med", "EI.sd",
          "EI.max", "EI.min"
        )
        EI.bm$EI <- curr.EI # add indicator
        EI.bm <- EI.bm[, c(
          "EI", "vegClass", "stratum", "n", "EI.mean", "EI.sd", "EI.med", "EI.max",
          "EI.min"
        )]
        # Use human-readable vegetation names in the exported benchmark table.
        EI.bm$vegClass <- human.readable(EI.bm$vegClass)
        sorted.EI.bm <- EI.bm[order(EI.bm$vegClass, EI.bm$stratum), ]
        sorted.EI.bm[1:12, ]
        resultsFile <- paste(RESULTSDIR, resultsBm, sep = "")
        write.csv(EI.bm, file = resultsFile, row.names = FALSE)
        # Prepare focal summary statistics, then calculate quality and bootstrap uncertainty.
        prepare_indicator_results(environment())
        calculate_indicator_quality(environment())
      } else {
        cat("************************\n")
        cat("Generating figures\n")
        cat("************************\n")
      } # END PLOT.FIGS.ONLY
      # Generate overlap figures and configured distribution figures for each vegetation type.
      if (plot.overlap == TRUE) plot_indicator_overlap(environment())
      plot_indicator_distributions(environment())
      cat("\n##################################################\n")
      cat("Operation completed\n")
      cat("##################################################\n\n")
    },
    envir = state
  )
  if (config$plot_figures_only) {
    return(invisible(NULL))
  }
  invisible(list(
    raw = read.csv(file.path(output_paths$results, "PCESS_raw.csv")),
    focal = read.csv(file.path(output_paths$results, "PCESS_focal.csv")),
    benchmark = read.csv(file.path(output_paths$results, "PCESS_benchmark.csv"))
  ))
}

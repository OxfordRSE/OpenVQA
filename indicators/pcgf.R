# Quality Hectares Ecological Indicator: Percent Cover by Growth Form (PCGF)
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
# 9 April 2013
#
# Purpose:
# Calculate cover of all vegetation within each stratum relative to benchmark sites
# of the same target vegetation class. Stratum-specific scores contribute to the
# final quality coefficient. Strata may represent growth forms or size classes.
# Zero benchmark cover is common, for example when grassland contains no trees;
# quality is evaluated by the configured fixed or empirical benchmark method.
#
# Input: coverByGrowthForm.csv, plot metadata, and stratum inclusion tables.
# Cover rows contain plotCode (unique across focal and benchmark plots),
# focalOrBenchmark ('b' or 'f'), landCover, vegClass (target benchmark vegetation),
# stratum, and cover. Percentages must be converted to proportions using
# convert_percent; cover above one is truncated for beta-distribution calculations.

# Prepare the raw indicator table without writing files; EI.tr holds transformed EI.
pcgf_data <- function(inputs, config) {
  required <- c(
    "coverByGrowthForm.csv", "plotMetadata.csv", "ei.stratum.include.csv",
    "ei.stratum.veg.include.csv"
  )
  if (any(vapply(inputs[required], is.null, logical(1)))) {
    stop("PCGF requires: ", paste(required, collapse = ", "), call. = FALSE)
  }
  INCLUDE.PLOTS.NODATA <- config$include_plots_nodata
  settings <- resolve_params(config$indicator_defaults, config$indicator_settings$PCGF)
  curr.EI <- "PCGF"
  inputFileName <- "coverByGrowthForm.csv"
  convert.percent <- settings$convert_percent
  na.to.zero <- settings$na_to_zero
  remove.zero.cover.plots <- settings$remove_zero_cover_plots
  # Sample-size threshold used in the low-count warning messages.
  n.min <- 6

  cat("Preparing raw data from scratch:\n")
  cat("- Importing raw data...")
  dat <- inputs[[inputFileName]]
  plotMetadata <- inputs[["plotMetadata.csv"]]
  cat("done\n")
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
  # Optionally remove plots whose total cover across strata is zero.
  # The sum ignores NA values (na.rm=TRUE, na.action=na.pass).
  if (remove.zero.cover.plots == TRUE) {
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
    # Count remaining focal plots by land cover after removing zero-cover plots.
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
    # Count remaining benchmark plots by target vegetation class.
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
        "WARNING: ", veg.n.b.low.cnt,
        " bm vegetation classes have less than n.min=", n.min, " bm plots!\n"
      )
      cat(msg)
    }
  } # END remove.zero.cover.plots
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
run_pcgf <- function(inputs, config, output_paths, seed = config$seed) {
  state <- cover_indicator_state(inputs, config, output_paths, seed)
  evalq(
    {
      curr.EI <- "PCGF"
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
      if (has.strata == F) curr.stratum <- ""
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
      cat("Operation: Calculating Quality for indicator group PCGF\n")
      cat("(Percent Cover by Growth Form)\n")
      cat("##################################################\n\n")
      if (PLOT.FIGS.ONLY == FALSE) {
        cat("************************\n")
        cat("Preparing data\n")
        cat("************************\n")
        if (prepare.raw || force.prepare.raw) {
          dat <- pcgf_data(inputs, config)
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
          "EI", "vegClass", "stratum", "n", "EI.mean", "EI.sd", "EI.med",
          "EI.max", "EI.min"
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
      if (plot.overlap == T) plot_indicator_overlap(environment())
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
    raw = read.csv(file.path(output_paths$results, "PCGF_raw.csv")),
    focal = read.csv(file.path(output_paths$results, "PCGF_focal.csv")),
    benchmark = read.csv(file.path(output_paths$results, "PCGF_benchmark.csv"))
  ))
}

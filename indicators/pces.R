# Quality Hectares Ecological Indicator: Total Percent Exotic Cover (PCES)
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
# 16 May 2025
#
# Purpose:
# Calculate total exotic-species cover at plot level and quality relative to a
# benchmark. A fixed benchmark of zero represents absence of exotic cover.
#
# Input: exoticCoverByStratum.csv and plotMetadata.csv. Cover rows contain plotCode,
# focalOrBenchmark ('b' or 'f'), landCover, vegClass (target benchmark vegetation),
# stratum, and cover. Exotic cover must already be prepared upstream; this function
# does not inspect species or is_exotic fields to remove native species.
#
# Cover must already be in proportions. The calculation sums input cover across
# strata within each plot, joins totals back to the input metadata rows, and optionally
# adds missing plots with zero cover. The join retains repeated plot rows. Cover is
# neither truncated at one nor transformed, and the output uses stratum='nostrata'.

# Prepare the raw indicator table without writing files; EI.tr holds transformed EI.
pces_data <- function(inputs, config) {
  required <- c("exoticCoverByStratum.csv", "plotMetadata.csv")
  if (any(vapply(inputs[required], is.null, logical(1)))) {
    stop("PCES requires: ", paste(required, collapse = ", "), call. = FALSE)
  }
  INCLUDE.PLOTS.NODATA <- config$include_plots_nodata
  settings <- resolve_params(config$indicator_defaults, config$indicator_settings$PCES)
  inputFileName <- "exoticCoverByStratum.csv"
  na.to.zero <- settings$na_to_zero

  cat("Importing raw data:\n")
  cat("- Plot data...")
  dat.plot <- inputs[["plotMetadata.csv"]]
  dat.plot$plotCode <- as.character(dat.plot$plotCode)
  cat("done\n")
  cat("- Species cover by stratum data...")
  dat <- inputs[[inputFileName]]
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
  cat("Calculating total exotic cover per plot:\n")
  cat("- Aggregating cover by plot using max function...")
  # Sum exotic cover to plot level across all input strata.
  df.plotCover <- aggregate(
    cover ~ plotCode,
    data = dat,
    FUN = sum,
    na.rm = TRUE,
    na.pass = NULL
  )
  cat("done\n")
  cat("- Merging into new data frame 'dat'...")
  # Join plot totals to the input plot metadata rows. Repeated rows are retained;
  # the benchmark sample count below therefore counts unique plots separately.
  df.plotCover <- merge(df.plotCover, dat[, c(
    "plotCode", "focalOrBenchmark", "landCover", "vegClass"
  )])
  df.plotCover <- df.plotCover[, c(
    "plotCode", "focalOrBenchmark", "landCover", "vegClass",
    "cover"
  )]
  dat <- df.plotCover
  cat("done\n")
  cat("- Renaming 'cover' to generic 'EI'...")
  names(dat)[names(dat) == "cover"] <- "EI"
  cat("done\n")
  # Optionally add metadata plots absent from the cover table, assigning zero cover.
  if (INCLUDE.PLOTS.NODATA) {
    cat("Adding missing plots...")
    curr.plots <- unique(dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")])
    curr.plots$in.curr.plots <- TRUE
    curr.plots <- curr.plots[, c("plotCode", "in.curr.plots")]
    all.plots <- unique(dat.plot[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")])
    all.plots <- merge(all.plots, curr.plots, by = "plotCode", all.x = TRUE)
    plots.missing <- all.plots[is.na(all.plots$in.curr.plots), ]
    n.missing <- nrow(plots.missing)
    if (n.missing > 0) {
      plots.missing$EI <- 0
      plots.missing <- plots.missing[, c(
        "plotCode", "focalOrBenchmark", "landCover",
        "vegClass", "EI"
      )]
      dat <- rbind(dat, plots.missing)
      dat <- dat[order(dat$vegClass, dat$landCover, dat$focalOrBenchmark, dat$plotCode), ]
      rownames(dat) <- NULL # Reset row numbers
      cat(n.missing, " plots added\n")
    } else {
      cat("all plots already present\n")
    }
  }
  cat("Adding pseudo-stratum...")
  # Input strata have been aggregated away; use the shared output pseudo-stratum.
  cols <- colnames(dat)
  if (!any("stratum" %in% cols)) {
    has.stratum <- FALSE
    dat$stratum <- "nostrata"
  }
  colnames(dat) <- c(
    "plotCode", "focalOrBenchmark", "landCover",
    "vegClass", "EI", "stratum"
  )
  dat <- dat[, c(
    "plotCode", "focalOrBenchmark", "landCover",
    "vegClass", "stratum", "EI"
  )]
  cat("done\n")
  cat("Completing final transformations...")
  # No transformation is applied: EI.tr duplicates the summed cover in EI.
  dat$EI.tr <- dat$EI
  cat("done\n")

  dat
}

# Calculate quality and write raw, benchmark, focal, bootstrap, and PDF tables,
# then generate the configured overlap and distribution figures. Return the raw,
# focal, and benchmark tables; figures-only runs use existing files and return NULL.
run_pces <- function(inputs, config, output_paths, seed = config$seed) {
  state <- cover_indicator_state(inputs, config, output_paths, seed)
  evalq(
    {
      curr.EI <- "PCES"
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
      # Plot-level cover has no strata, so grouped-stratum figures are disabled.
      allow.group.strata <- FALSE
      # Minimum y-axis value; an empty string allows automatic limits.
      y.axis.fixed.min <- 0
      # Include land cover with the stratum in titles of single-stratum figures.
      lc.in.title <- TRUE
      # Single-figure legend offset: positive moves right, negative moves left.
      leg.x.extra <- -0.2
      leg.y.extra <- NA
      leg.x.extra.g <- -0.2
      leg.y.extra.g <- NA
      # Established output filenames; the raw table includes EI and its transformed value EI.tr.
      rawFileName <- paste(curr.EI, "_raw.csv", sep = "")
      resultsFocal <- paste(curr.EI, "_focal.csv", sep = "")
      resultsBm <- paste(curr.EI, "_benchmark.csv", sep = "")

      cat("\n##################################################\n")
      cat("Operation: Calculating Quality for indicator group PCESS\n")
      cat("(Percent Cover Exotic Species by Stratum)\n")
      cat("##################################################\n\n")
      if (PLOT.FIGS.ONLY == FALSE) {
        cat("########################\n")
        cat("Preparing data\n")
        cat("########################\n\n")
        if (prepare.raw || force.prepare.raw) {
          dat <- pces_data(inputs, config)
          write.csv(dat, paste0(RESULTSDIR, rawFileName), row.names = FALSE)
        }
        # Read back the prepared raw table, including when preparation is disabled.
        inputFile <- paste(RESULTSDIR, rawFileName, sep = "")
        cat("inputFile=", inputFile)
        dat <- read.csv(inputFile, header = T)
        # Summarize benchmark cover by vegetation class; there is no output stratum.
        EI.bm <- dat[dat$focalOrBenchmark == "b", ]
        EI.bm <- with(EI.bm, aggregate(EI.tr, list(vegClass = vegClass),
          FUN = function(x) {
            c(
              n = length(x), # Replaced below with the number of unique benchmark plots.
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
        # Count unique benchmark plots rather than the repeated rows used for cover moments.
        EI.bm.plots <- unique(dat[dat$focalOrBenchmark == "b", c("plotCode", "vegClass")])
        EI.bm.n <- aggregate(
          plotCode ~ vegClass,
          data = EI.bm.plots,
          FUN = length
        )
        colnames(EI.bm.n) <- c("vegClass", "n.b")
        EI.bm <- merge(EI.bm, EI.bm.n, by = "vegClass")
        names(EI.bm)[names(EI.bm) == "n"] <- "n.bad"
        names(EI.bm)[names(EI.bm) == "n.b"] <- "n"
        EI.bm <- EI.bm[, !names(EI.bm) %in% c("n.bad")]
        EI.bm$stratum <- "nostrata"
        EI.bm$EI <- curr.EI # add indicator
        EI.bm <- EI.bm[, c(
          "EI", "vegClass", "stratum", "n", "EI.mean", "EI.sd", "EI.med", "EI.max",
          "EI.min"
        )]
        # Use human-readable vegetation names in the exported benchmark table.
        EI.bm$vegClass <- human.readable(EI.bm$vegClass)
        sorted.EI.bm <- EI.bm[order(EI.bm$vegClass), ]
        head(sorted.EI.bm)
        resultsFile <- paste(RESULTSDIR, resultsBm, sep = "")
        write.csv(EI.bm, file = resultsFile, row.names = FALSE)
        # Prepare focal summary statistics, then calculate quality and bootstrap uncertainty.
        prepare_indicator_results(environment())
        calculate_indicator_quality(environment())
      } else {
        cat("########################\n")
        cat("Generating figures\n")
        cat("########################\n\n")
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
    raw = read.csv(file.path(output_paths$results, "PCES_raw.csv")),
    focal = read.csv(file.path(output_paths$results, "PCES_focal.csv")),
    benchmark = read.csv(file.path(output_paths$results, "PCES_benchmark.csv"))
  ))
}

# Quality Hectares Ecological Indicator: Percent Ground Cover (GC)
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
# 9 April 2013
#
# Purpose:
# Calculate cover of different ground-cover types relative to benchmark sites of
# the same target vegetation class. Ground-cover types are treated as strata even
# though they occupy the same ground-level stratum. Scores are calculated separately
# for each cover class and contribute to the final quality coefficient.
#
# Input: groundCover.csv and stratum inclusion tables. Cover rows contain plotCode
# (unique across focal and benchmark plots), focalOrBenchmark ('b' or 'f'), landCover,
# vegClass (target benchmark vegetation), stratum (ground-cover type), and cover.
# Percentages must be converted to proportions using convert_percent; cover above
# one is truncated for beta-distribution calculations. Rows with missing cover
# are removed before constructing the plot-stratum combinations.

# Prepare the raw indicator table without writing files; EI.tr holds transformed EI.
gc_data <- function(inputs, config) {
  required <- c("groundCover.csv", "ei.stratum.include.csv", "ei.stratum.veg.include.csv")
  if (any(vapply(inputs[required], is.null, logical(1)))) {
    stop("GC requires: ", paste(required, collapse = ", "), call. = FALSE)
  }
  settings <- resolve_params(config$indicator_defaults, config$indicator_settings$GC)
  curr.EI <- "GC"
  inputFileName <- "groundCover.csv"
  convert.percent <- settings$convert_percent

  cat(":\n")
  cat("- Importing raw data...")
  dat <- inputs[[inputFileName]]
  n.before <- nrow(dat)
  dat <- dat[!is.na(dat$cover), ]
  n.after <- nrow(dat)
  n.diff <- n.before - n.after
  if (n.diff > 0) {
    cat("WARNING: ", n.diff, " of ", n.before, " rows deleted due to missing data!\n")
  }
  cat("done\n")
  # Use plots with observed cover after removing missing rows, before stratum filtering.
  plots <- unique(dat$plotCode)
  dat <- complete_cover_strata(dat, plots, inputs, curr.EI)
  cat("- Completing transformations...")
  plots <- unique(dat$plotCode)
  # Plot codes must identify a single plot across both benchmark and focal data.
  # Correct ambiguous codes in the input rather than combining the two samples.
  allPlots <- unique(dat[, c("focalOrBenchmark", "plotCode")])
  if (length(plots) != nrow(allPlots)) stop("Plot codes not unique!")
  dat <- dat[, c(
    "plotCode", "focalOrBenchmark", "landCover",
    "vegClass", "stratum", "cover"
  )]
  colnames(dat) <- c(
    "plotCode", "focalOrBenchmark", "landCover",
    "vegClass", "stratum", "EI"
  )
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
run_gc <- function(inputs, config, output_paths, seed = config$seed) {
  state <- cover_indicator_state(inputs, config, output_paths, seed)
  evalq(
    {
      curr.EI <- "GC"
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
      # Single-figure legend offset: positive moves right, negative moves left.
      leg.x.extra <- -0.1
      # Established output filenames; the raw table includes EI and its transformed value EI.tr.
      rawFileName <- paste(curr.EI, "_raw.csv", sep = "")
      resultsFocal <- paste(curr.EI, "_focal.csv", sep = "")
      resultsBm <- paste(curr.EI, "_benchmark.csv", sep = "")


      cat("\n")
      cat("#######################################################\n")
      cat("Calculating Quality for indicator group GC (Ground Cover)\n")
      cat("#######################################################\n")
      cat("\n")
      if (PLOT.FIGS.ONLY == FALSE) {
        cat("************************\n")
        cat("Preparing data\n")
        cat("************************\n")
        cat("Calculating indicator values from scratch")
        if (prepare.raw || force.prepare.raw) {
          dat <- gc_data(inputs, config)
          write.csv(dat, paste0(RESULTSDIR, rawFileName), row.names = FALSE)
        }
        cat("Preparing benchmark indicator files...")
        # Read back the prepared raw table, including when preparation is disabled.
        inputFile <- paste(RESULTSDIR, rawFileName, sep = "")
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
        resultsFile <- paste(RESULTSDIR, resultsBm, sep = "")
        write.csv(EI.bm, file = resultsFile, row.names = FALSE)
        cat("done\n")
        cat("Preparing focal results files...")
        # Prepare focal summary statistics, then calculate quality and bootstrap uncertainty.
        prepare_indicator_results(environment())
        cat("done\n")
        calculate_indicator_quality(environment())
      } # END PLOT.FIGS.ONLY
      # Generate overlap figures and configured distribution figures for each vegetation type.
      if (plot.overlap == T) plot_indicator_overlap(environment())
      cat("************************\n")
      cat("Generating figures\n")
      cat("************************\n")
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
    raw = read.csv(file.path(output_paths$results, "GC_raw.csv")),
    focal = read.csv(file.path(output_paths$results, "GC_focal.csv")),
    benchmark = read.csv(file.path(output_paths$results, "GC_benchmark.csv"))
  ))
}

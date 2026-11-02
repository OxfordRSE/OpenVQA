# Shared cover-indicator evaluation state and plot-stratum inclusion rules.

cover_indicator_state <- local({
  helpers <- new.env(parent = environment())
  sys.source(file.path("includes", "functions.R"), envir = helpers)
  function(inputs, config, output_paths, seed) {
    # shortcut: shared stages need local parameter names; remove bindings when stages accept config.
    state <- list2env(c(legacy_common_params(config), legacy_project_params(config)),
      parent = helpers
    )
    for (name in ls(helpers)) {
      helper <- helpers[[name]]
      environment(helper) <- state
      state[[name]] <- helper
    }
    state$fitdistr <- MASS::fitdistr
    state$RESULTSDIR <- paste0(output_paths$results, "/")
    state$FIGDIR <- state$FIGDIR_REL <- paste0(output_paths$figures, "/")
    state$FILENAME.EI.SUMMARY <- "ei.summary.csv"
    # Keep per-operation seeding: NULL disables bootstrap resets, while permutation
    # tests still receive the default seed of 123. Do not reset the RNG at entry.
    state$set.seed <- !is.null(seed)
    state$seed <- if (is.null(seed)) 123 else seed
    state$inputs <- inputs
    state$config <- config
    state
  }
})

# Build all plot-stratum combinations BEFORE filtering the source rows so missing
# included strata can be added with zero cover. Apply the same exclusions and
# vegetation-specific exceptions to both observed rows and the completion grid.
complete_cover_strata <- function(dat, plots, inputs, indicator) {
  curr.EI <- indicator
  # Capture all input strata before applying the indicator inclusion rules.
  strata <- unique(dat$stratum)
  plot.strata <- expand.grid(plots, strata, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  colnames(plot.strata) <- c("plotCode", "stratum")
  plot.strata$plot.stratum <- paste0(plot.strata$plotCode, "-", plot.strata$stratum)
  # Plot metadata supplies the benchmark vegetation class used to match exceptions.
  plot.meta <- unique(dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")])
  plot.strata <- merge(plot.strata, plot.meta, by = "plotCode", all.x = TRUE)
  cat("- Importing and preparing stratum include files...")
  # Blacklist: indicator-stratum combinations, with excluded categories flagged include=FALSE.
  blacklist <- inputs[["ei.stratum.include.csv"]]
  blacklist <- blacklist[, c("EI", "stratum", "f.group", "include")]
  # Whitelist: exceptions for individual benchmark vegetation classes; ignore missing include flags.
  whitelist <- inputs[["ei.stratum.veg.include.csv"]]
  whitelist <- whitelist[!is.na(whitelist$include), ]
  whitelist <- whitelist[, c("EI", "stratum", "bm.vegetation", "include")]
  cat("done\n")
  cat("- Filtering excluded strata...")
  # Apply the general stratum rule, then restore explicitly included vegetation exceptions.
  strata.include <- blacklist[blacklist$EI == curr.EI, c("stratum", "include")]
  dat <- merge(dat, strata.include, by = "stratum", all.x = TRUE)
  strata.include.exceptions <- unique(whitelist[
    whitelist$EI == curr.EI & whitelist$include == TRUE,
    c("stratum", "bm.vegetation", "include")
  ])
  colnames(strata.include.exceptions) <-
    c("stratum", "vegClass", "include.exceptions")
  dat <- merge(dat, strata.include.exceptions,
    by = c("stratum", "vegClass"), all.x = TRUE
  )
  if (nrow(strata.include.exceptions) > 0) {
    dat$include[dat$include.exceptions == TRUE] <- TRUE
  }
  dat$include <- as.logical(dat$include)
  dat <- dat[dat$include == TRUE, ]
  cat("done\n")
  cat("- Adding missing plot strata...")
  # Filter the completion grid using the same rules as the observed cover rows.
  plot.strata <- merge(plot.strata, strata.include, by = "stratum", all.x = TRUE)
  plot.strata <- merge(plot.strata, strata.include.exceptions,
    by = c("stratum", "vegClass"), all.x = TRUE
  )
  plot.strata$include[plot.strata$include.exceptions == TRUE] <- TRUE
  plot.strata <- plot.strata[plot.strata$include == TRUE, ]
  # Drop inclusion bookkeeping before appending missing records.
  drop.cols <- c("include", "include.exceptions")
  plot.strata <- plot.strata[, !names(plot.strata) %in% drop.cols]
  # Remove combinations already observed so zero filling does not replace measured cover.
  dat$plot.stratum <- paste0(dat$plotCode, "-", dat$stratum)
  dat.plot.strata <- unique(dat[, c("plot.stratum"), drop = FALSE])
  dat.plot.strata$in.dat <- 1
  plot.strata <- merge(plot.strata, dat.plot.strata, by = "plot.stratum", all.x = TRUE)
  plot.strata <- plot.strata[is.na(plot.strata$in.dat), ]
  drop.cols <- c("in.dat")
  plot.strata <- plot.strata[, !(names(plot.strata) %in% drop.cols)]
  dat <- dat[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "cover")]
  if (nrow(plot.strata > 0)) {
    dat.missing <- plot.strata
    # Missing included strata receive zero cover, retaining the established column order.
    dat.missing$cover <- 0
    dat.missing <- dat.missing[, c(
      "plotCode", "focalOrBenchmark", "landCover", "vegClass",
      "stratum", "cover"
    )]
    dat <- rbind(dat, dat.missing)
    cat("done\n")
  } else {
    cat("all strata already present...done\n")
  }
  dat
}

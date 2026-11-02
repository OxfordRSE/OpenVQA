# Local parameter bindings for the shared quality and plotting stages used by stem indicators.
stem_indicator_state <- local({
  helpers <- new.env(parent = environment())
  sys.source(file.path("includes", "functions.R"), envir = helpers)
  function(inputs, config, output_paths, seed = config$seed) {
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
    state$output_paths <- output_paths
    state
  }
})

check_legacy_inputs <- function(run_env) {
  evalq(
    {
      cat("Checking for required input files...")
      if (QH.METHOD == "empirical") {
        required_files <- input_file_list(DF.LANDCOVER, DF.PLOTMETADATA, DF.SPECIES, EI.vec)
      } else {
        required_files <- DF.LANDCOVER
      }
      for (fname in required_files) {
        if (!file.exists(file.path(INPUTDIR, paste0(fname, ".csv")))) {
          stop_quietly(
            "ERROR: one or more required input files missing!\nHave you run import.R yet?\n"
          )
        }
      }
      cat("done\n")
    },
    envir = run_env
  )
}

run_legacy_fixed_quality <- function(run_env) {
  run_fixed_quality(
    list(landCover.csv = read.csv(file.path(run_env$INPUTDIR, run_env$LANDCOVER.FILE))),
    run_env$analysis_config, run_env$analysis_paths
  )
}

run_legacy_indicators <- function(run_env) {
  if (!run_env$VQA.SUMMARY.ONLY) {
    cat("******************************************\nCalculate indicator quality\n\n")
    if (is.null(run_env$TD.ONLY)) run_env$TD.ONLY <- FALSE
    if (run_env$TD.ONLY) {
      cat("*** Running indicator TD only ***\n\n")
      run_taxonomic_distance(
        load_assessment_inputs(build_file_map(run_env$INPUTDIR)),
        run_env$analysis_config, run_env$analysis_paths
      )
    } else {
      for (indicator in run_env$EI.vec) {
        run_env$EI <- indicator
        runner <- switch(indicator,
          SR = run_species_richness,
          PCGF = run_pcgf,
          PCS = run_pcs,
          GC = run_gc,
          PCESS = run_pcess,
          PCES = run_pces,
          BA = run_basal_area,
          ASC = run_stem_abundance,
          TS = run_taxonomic_similarity,
          TD = run_taxonomic_distance,
          CH = run_canopy_height,
          BAES = run_exotic_basal_area,
          stop("Unknown indicator: ", indicator, call. = FALSE)
        )
        runner(
          load_assessment_inputs(build_file_map(run_env$INPUTDIR)),
          run_env$analysis_config, run_env$analysis_paths
        )
      }
    }
  }
}

run_legacy_summaries <- function(run_env) {
  config <- run_env$analysis_config
  if (!run_env$PLOT.FIGS.ONLY && !run_env$TD.ONLY && run_env$QH.METHOD == "empirical") {
    paths <- run_env$analysis_paths
    paths$input <- run_env$INPUTDIR
    write_assessment_reports(config, paths)
  }
}

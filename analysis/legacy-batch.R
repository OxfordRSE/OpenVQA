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
      evalq(source(file.path("indicators", "td.R"), local = TRUE), envir = run_env)
    } else {
      for (indicator in run_env$EI.vec) {
        run_env$EI <- indicator
        if (indicator == "SR") {
          run_species_richness(
            load_assessment_inputs(build_file_map(run_env$INPUTDIR)),
            run_env$analysis_config, run_env$analysis_paths
          )
        } else if (indicator == "PCGF") {
          run_pcgf(
            load_assessment_inputs(build_file_map(run_env$INPUTDIR)),
            run_env$analysis_config, run_env$analysis_paths
          )
        } else if (indicator == "GC") {
          run_gc(
            load_assessment_inputs(build_file_map(run_env$INPUTDIR)),
            run_env$analysis_config, run_env$analysis_paths
          )
        } else {
          evalq(source(
            file.path("indicators", paste0(tolower(EI), ".R")),
            local = TRUE
          ), envir = run_env)
        }
      }
    }
  }
}

run_legacy_summaries <- function(run_env) {
  evalq(
    {
      if (PLOT.FIGS.ONLY == FALSE && TD.ONLY == FALSE && QH.METHOD == "empirical") {
        source("vqa.summary.R", local = TRUE)
        source(file.path("vqa.summary", "vqa.summary.csv.R"), local = TRUE)
        source(file.path("vqa.summary", "vqa.summary.xl.R"), local = TRUE)
      }
    },
    envir = run_env
  )
}

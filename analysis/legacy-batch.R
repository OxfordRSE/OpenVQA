check_legacy_inputs <- function(run_env) {
  evalq({
    cat("Checking for required input files...")
    if (QH.METHOD == "empirical") {
      required_files <- input_file_list(DF.LANDCOVER, DF.PLOTMETADATA, DF.SPECIES, EI.vec)
    } else {
      required_files <- DF.LANDCOVER
    }
    for (fname in required_files) {
      if (!file.exists(file.path(INPUTDIR, paste0(fname, ".csv")))) {
        stop_quietly("ERROR: one or more required input files missing!\nHave you run import.R yet?\n")
      }
    }
    cat("done\n")
  }, envir = run_env)
}

run_legacy_fixed_quality <- function(run_env) {
  evalq(source("q.fixed.R"), envir = run_env)
}

run_legacy_indicators <- function(run_env) {
  evalq({
    if (VQA.SUMMARY.ONLY == FALSE) {
      cat("******************************************\nCalculate indicator quality\n\n")
      if (!exists("TD.ONLY")) TD.ONLY <- FALSE
      if (TD.ONLY == TRUE) {
        cat("*** Running indicator TD only ***\n\n")
        source(file.path("indicators", "td.R"), local = TRUE)
      } else {
        for (EI in EI.vec) source(file.path("indicators", paste0(tolower(EI), ".R")), local = TRUE)
      }
    }
  }, envir = run_env)
}

run_legacy_summaries <- function(run_env) {
  evalq({
    if (PLOT.FIGS.ONLY == FALSE && TD.ONLY == FALSE && QH.METHOD == "empirical") {
      source("vqa.summary.R", local = TRUE)
      source(file.path("vqa.summary", "vqa.summary.csv.R"), local = TRUE)
      source(file.path("vqa.summary", "vqa.summary.xl.R"), local = TRUE)
    }
  }, envir = run_env)
}

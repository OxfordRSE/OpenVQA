if (!exists("run_fixed_quality", mode = "function")) {
  source(file.path("analysis", "fixed-quality.R"), local = TRUE)
}
run_fixed_quality(
  list(landCover.csv = read.csv(file.path(INPUTDIR, LANDCOVER.FILE))),
  list(qh_method = QH.METHOD, bootstrap_reps = boot.reps),
  list(results = RESULTSDIR)
)

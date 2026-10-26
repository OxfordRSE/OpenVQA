if (!exists("run_species_richness", mode = "function")) {
  source(file.path("pipeline", "io.R"), local = TRUE)
  source(file.path("pipeline", "params.R"), local = TRUE)
  source(file.path("analysis", "legacy-params.R"), local = TRUE)
  source(file.path("analysis", "species-richness.R"), local = TRUE)
}
run_species_richness(
  load_assessment_inputs(build_file_map(INPUTDIR)),
  resolve_runtime_config(DATA_BASEDIR_PROJ, ASSESS, seed = if (set.seed) seed else NULL)$config,
  list(results = RESULTSDIR, figures = FIGDIR)
)

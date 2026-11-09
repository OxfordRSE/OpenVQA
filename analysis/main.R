source(file.path("includes", "quality-functions.R"), local = TRUE)
source(file.path("includes", "distribution-functions.R"), local = TRUE)
source(file.path("includes", "overlap-functions.R"), local = TRUE)
source(file.path("includes", "bootstrap-functions.R"), local = TRUE)
source(file.path("includes", "utility-functions.R"), local = TRUE)
source(file.path("includes", "formatting-functions.R"), local = TRUE)
source(file.path("includes", "spreadsheet-functions.R"), local = TRUE)
source(file.path("includes", "plotting-functions.R"), local = TRUE)
source(file.path("includes", "indicator-results-functions.R"), local = TRUE)
source(file.path("includes", "indicator-figure-functions.R"), local = TRUE)
source(file.path("reporting", "main.R"), local = TRUE)
source(file.path("analysis", "indicator-settings.R"), local = TRUE)
source(file.path("analysis", "libraries.R"), local = TRUE)
source(file.path("analysis", "fixed-quality.R"), local = TRUE)
source(file.path("indicators", "sr.R"), local = TRUE)
source(file.path("analysis", "cover-indicators.R"), local = TRUE)
source(file.path("analysis", "taxonomic-distance.R"), local = TRUE)
source(file.path("indicators", "pcgf.R"), local = TRUE)
source(file.path("indicators", "gc.R"), local = TRUE)
source(file.path("indicators", "pcess.R"), local = TRUE)
source(file.path("indicators", "pces.R"), local = TRUE)
source(file.path("indicators", "ba.R"), local = TRUE)
source(file.path("indicators", "asc.R"), local = TRUE)
source(file.path("indicators", "ts.R"), local = TRUE)
source(file.path("indicators", "td.R"), local = TRUE)
source(file.path("indicators", "pcs.R"), local = TRUE)
source(file.path("indicators", "ch.R"), local = TRUE)
source(file.path("indicators", "baes.R"), local = TRUE)

with_analysis_log <- function(config, paths, code) {
  suffix <- if (config$replace_log) "" else format(Sys.time(), "_%Y%m%d_%H%M%S")
  logfile <- file.path(paths$log, paste0(config$logfile_basename, suffix, ".txt"))
  connection <- file(logfile)
  depth <- sink.number(type = "output")
  on.exit(
    {
      if (sink.number(type = "output") > depth) sink(type = "output")
      close(connection)
    },
    add = TRUE
  )
  sink(connection, append = TRUE, type = "output", split = TRUE)
  force(code)
  logfile
}

check_analysis_inputs <- function(inputs, config) {
  required <- "landCover.csv"
  if (config$qh_method == "empirical") {
    codes <- if (config$td_only) "TD" else unlist(config$active_indicators, use.names = FALSE)
    required <- c(required, "plotMetadata.csv", "species.csv", vapply(codes, function(code) {
      indicator_parameters(code, config)$source_file
    }, character(1)))
  }
  missing <- setdiff(required, names(inputs))
  if (length(missing)) {
    stop("Missing required analysis inputs: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

run_analysis_indicators <- function(inputs, config, paths) {
  if (config$summary_only) {
    return(invisible(NULL))
  }
  codes <- if (config$td_only) "TD" else unlist(config$active_indicators, use.names = FALSE)
  for (code in codes) {
    runner <- switch(code,
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
      stop("Unknown indicator: ", code, call. = FALSE)
    )
    runner(inputs, config, paths)
  }
  invisible(NULL)
}

run_vqa_analysis <- function(context, config = context$config, seed = NULL,
                             generate_figures = TRUE) {
  stopifnot(is.logical(generate_figures), length(generate_figures) == 1L, !is.na(generate_figures))
  if (!is.list(config)) stop("config must be a list.", call. = FALSE)
  settings <- config
  settings$seed <- NULL
  resolved <- context$config
  resolved$seed <- NULL
  if (!identical(settings, resolved)) {
    stop("config must match the resolved run context.", call. = FALSE)
  }
  seed <- resolve_seed(if (is.null(seed)) config$seed else seed)
  config$seed <- seed
  context$config <- config
  paths <- context$paths
  paths$input <- paths$inputs
  paths$generate_figures <- generate_figures
  for (path in paths[c("results", "figures", "log")]) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  inputs <- load_assessment_inputs(build_file_map(paths$inputs))
  logfile <- with_analysis_log(config, paths, {
    check_analysis_inputs(inputs, config)
    load_analysis_libraries(config)
    if (config$qh_method %in% c("assume.0", "assume.1")) {
      run_fixed_quality(inputs, config, paths)
    } else {
      run_analysis_indicators(inputs, config, paths)
    }
    if (!config$plot_figures_only && !config$td_only && config$qh_method == "empirical") {
      write_assessment_reports(config, paths)
    }
  })
  output_paths <- list(
    results = paste0(paths$results, "/"),
    figures = paste0(paths$figures, "/"), log = logfile
  )
  metadata <- list(
    seed = seed, project = config$project,
    assessment = config$assessment, config = config
  )
  invisible(list(
    context = context, seed = seed, output_paths = output_paths,
    metadata = metadata
  ))
}

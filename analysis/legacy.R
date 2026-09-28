legacy_project_name <- function(context) {
  project <- context$params$project
  if (is.null(project) || !nzchar(project)) {
    stop("params.yaml must declare project for legacy analysis.", call. = FALSE)
  }
  sub("-min$", "", project)
}

prepare_legacy_environment <- function(context = NULL, config = list(), seed = NULL) {
  run_env <- new.env(parent = globalenv())
  run_env$wd <- getwd()
  run_env$SRCDIR <- paste0(normalizePath(run_env$wd, mustWork = TRUE), "/")
  run_env$job <- "vqa.batch"
  run_env$VQA_RUN_ENV <- run_env
  run_env$source <- evalq(
    function(file, ...) sys.source(file, envir = VQA_RUN_ENV, toplevel.env = VQA_RUN_ENV),
    envir = run_env
  )
  if (!is.null(context)) {
    data_root <- if (is.null(config$data_root)) context$data_root else config$data_root
    project <- if (is.null(config$project)) legacy_project_name(context) else config$project
    assessment <- if (is.null(config$assessment)) context$assessment else config$assessment
    run_env$VQA_DATA_ROOT <- normalizePath(data_root, mustWork = TRUE)
    run_env$VQA_PROJECT <- project
    run_env$VQA_ASSESSMENT <- assessment
    if (!is.null(seed)) run_env$VQA_TEST_SEED <- as.character(seed)
  }
  sys.source("includes/functions.R", envir = run_env, toplevel.env = run_env)
  sys.source("params.R", envir = run_env, toplevel.env = run_env)
  run_env
}

confirm_legacy_batch <- function(run_env) {
  evalq({
    cat(paste0(
      "Run VQA Batch for project '", 
      PROJ, 
      "', assessment '", 
      ASSESS, 
      "' using the following settings: \n"
    ))
    cat(paste0(MSG.CONF.START, MSG.CONF.BATCH))
    if (!interactive()) {
      cat("Continue? (y/n):")
      if (!readLines("stdin", n = 1L) %in% c("y", "Y", "Yes", "yes")) {
        stop_quietly("Operation cancelled\n\n")
      }
    } else cat("\n\n")
  }, envir = run_env)
}

execute_legacy_batch <- function(run_env) {
  legacy_functions <- c("gmean", "generalized_mean")
  had_function <- vapply(legacy_functions, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)
  old_functions <- mget(legacy_functions[had_function], envir = .GlobalEnv, inherits = FALSE)
  list2env(mget(legacy_functions, envir = run_env), envir = .GlobalEnv)
  on.exit(for (name in legacy_functions) {
    if (had_function[[name]]){
      assign(name, old_functions[[name]], envir = .GlobalEnv)
    } else {
      rm(list = name, envir = .GlobalEnv)
    }
  }, add = TRUE)
  logfile <- evalq(
    if(REPLACE.LOG){
      paste0(LOGDIR, LOGFILE.BASENAME, ".txt")
    } else {
      paste0(
        LOGDIR, 
        LOGFILE.BASENAME, 
        format(Sys.time(), "_%Y%m%d_%H%M%S"), ".txt"), 
        envir = run_env
      )
    }
  log <- file(logfile)
  sink_depth <- sink.number(type = "output")
  sink(log, append = TRUE, type = "output", split = TRUE)
  on.exit({
    while (sink.number(type = "output") > sink_depth) sink(type = "output")
    close(log)
  }, add = TRUE)
  sys.source("analysis/legacy-batch.R", envir = run_env, toplevel.env = run_env)
  invisible(list(
    results = evalq(RESULTSDIR, envir = run_env), 
    figures = evalq(FIGDIR, envir = run_env), 
    log = logfile
  ))
}

run_vqa_analysis <- function(context, config = list(), seed = NULL) {
  if (!is.list(config)) stop("config must be a list.", call. = FALSE)
  if (!is.null(seed) && (length(seed) != 1L || is.na(as.integer(seed)))) {
    stop("seed must be an integer.", call. = FALSE)
  }
  seed <- if (is.null(seed)) NULL else as.integer(seed)
  output_paths <- execute_legacy_batch(
    prepare_legacy_environment(context, config, seed)
  )
  invisible(list(context = context, seed = seed, output_paths = output_paths))
}

run_vqa_batch_cli <- function() {
  run_env <- prepare_legacy_environment()
  confirm_legacy_batch(run_env)
  execute_legacy_batch(run_env)
}

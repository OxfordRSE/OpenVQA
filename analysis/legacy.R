source(file.path("analysis", "legacy-batch.R"), local = TRUE)
source(file.path("pipeline", "params.R"), local = TRUE)
source(file.path("analysis", "legacy-params.R"), local = TRUE)
source(file.path("analysis", "fixed-quality.R"), local = TRUE)
source(file.path("analysis", "species-richness.R"), local = TRUE)
source(file.path("analysis", "cover-indicators.R"), local = TRUE)
source(file.path("indicators", "pcgf.R"), local = TRUE)
source(file.path("indicators", "gc.R"), local = TRUE)
source(file.path("indicators", "pcess.R"), local = TRUE)

legacy_project_name <- function(project) {
  if (!is.character(project) || length(project) != 1L || is.na(project) || !nzchar(project)) {
    stop("params.yaml must declare project for legacy analysis.", call. = FALSE)
  }
  # Only the compatibility adapter maps project labels to legacy parameter scripts.
  aliases <- c("vqa-demo1-min" = "vqa-demo1", "vqa-demo2-min" = "vqa-demo2")
  if (project %in% names(aliases)) unname(aliases[[project]]) else project
}

# This adapter alone loads legacy scientific parameters and translates runtime settings.
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
    project <- legacy_project_name(
      if (is.null(config$project)) {
        context$params$project
      } else {
        config$project
      }
    )
    assessment <- if (is.null(config$assessment)) context$assessment else config$assessment
    run_env$VQA_DATA_ROOT <- normalizePath(data_root, mustWork = TRUE)
    run_env$VQA_PROJECT <- project
    run_env$VQA_ASSESSMENT <- assessment
  }
  sys.source("includes/functions.R", envir = run_env, toplevel.env = run_env)
  run_env$VQA_COMMON_PARAMS <- legacy_common_params(
    if (is.null(context)) load_default_params() else config
  )
  if (!is.null(context) && project %in% c("vqa-demo1", "vqa-demo2")) {
    run_env$VQA_PROJECT_PARAMS <- legacy_project_params(config)
  }
  sys.source("params.R", envir = run_env, toplevel.env = run_env)
  resolved <- if (is.null(context)) {
    resolve_runtime_config(run_env$DATA_BASEDIR_PROJ, run_env$ASSESS, seed = seed)
  } else {
    list(config = config, paths = context$paths)
  }
  run_env$analysis_config <- resolved$config
  run_env$analysis_paths <- resolved$paths
  run_env$seed_enabled <- !is.null(seed) || isTRUE(run_env$set.seed)
  if (!is.null(seed)) run_env$seed <- seed
  # Legacy operations still reset the RNG individually using this compatibility symbol.
  run_env$set.seed <- run_env$seed_enabled
  run_env
}

confirm_legacy_batch <- function(run_env) {
  evalq(
    {
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
      } else {
        cat("\n\n")
      }
    },
    envir = run_env
  )
}

with_legacy_analysis_log <- function(run_env, code) {
  logfile <- evalq(
    {
      if (REPLACE.LOG) {
        file.path(LOGDIR, paste0(LOGFILE.BASENAME, ".txt"))
      } else {
        file.path(LOGDIR, paste0(
          LOGFILE.BASENAME,
          format(Sys.time(), "_%Y%m%d_%H%M%S"), ".txt"
        ))
      }
    },
    envir = run_env
  )
  log <- file(logfile)
  sink_depth <- sink.number(type = "output")
  on.exit(
    {
      if (sink.number(type = "output") > sink_depth) sink(type = "output")
      close(log)
    },
    add = TRUE
  )
  sink(log, append = TRUE, type = "output", split = TRUE)
  force(code)
  logfile
}

execute_legacy_batch <- function(run_env) {
  legacy_functions <- c("gmean", "generalized_mean")
  had_function <- vapply(legacy_functions, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)
  old_functions <- mget(legacy_functions[had_function], envir = .GlobalEnv, inherits = FALSE)
  list2env(mget(legacy_functions, envir = run_env), envir = .GlobalEnv)
  on.exit(for (name in legacy_functions) {
    if (had_function[[name]]) {
      assign(name, old_functions[[name]], envir = .GlobalEnv)
    } else {
      rm(list = name, envir = .GlobalEnv)
    }
  }, add = TRUE)

  logfile <- with_legacy_analysis_log(run_env, {
    cat("\n############################################\nBegin operation\n\n")
    check_legacy_inputs(run_env)
    evalq(source("libraries.R"), envir = run_env)

    if (run_env$QH.METHOD %in% c("assume.0", "assume.1")) {
      run_legacy_fixed_quality(run_env)
    } else {
      run_legacy_indicators(run_env)
    }
    run_legacy_summaries(run_env)
    cat("\nOperation completed\n############################################\n\n")
  })
  invisible(list(
    results = evalq(RESULTSDIR, envir = run_env),
    figures = evalq(FIGDIR, envir = run_env),
    log = logfile
  ))
}

run_vqa_analysis <- function(context, config = context$config, seed = NULL) {
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
  output_paths <- execute_legacy_batch(
    prepare_legacy_environment(context, config, seed)
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

run_vqa_batch_cli <- function() {
  run_env <- prepare_legacy_environment(seed = resolve_seed())
  confirm_legacy_batch(run_env)
  execute_legacy_batch(run_env)
}

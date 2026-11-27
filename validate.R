#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
})

source(file.path("validation", "helpers.R"), local = TRUE)
source(file.path("pipeline", "io.R"), local = TRUE)
source(file.path("pipeline", "params.R"), local = TRUE)
source(file.path("pipeline", "run-report.R"), local = TRUE)
source(file.path("validation", "requirements.R"), local = TRUE)
source(file.path("validation", "snapshot.R"), local = TRUE)
source(file.path("validation", "schema.R"), local = TRUE)
source(file.path("validation", "crossfile.R"), local = TRUE)

parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  # optparse handles `-h/--help` for us and accepts both
  # `--data-root /path` and `--data-root=/path`.
  option_list <- list(
    make_option(
      c("-d", "--data-root"),
      type = "character",
      help = "Root data directory that contains the assessment folders."
    ),
    make_option(
      c("-a", "--assessment"),
      type = "character",
      help = "Validate one assessment directory within the project; omit to validate the project."
    ),
    make_option(
      c("-x", "--exclude-dirs"),
      type = "character",
      default = NULL,
      help = paste(
        "Directory name to exclude from assessment scanning.",
        "Additional trailing arguments are also treated as excluded dirs."
      )
    ),
    make_option(
      "--report",
      type = "character",
      help = "Write a JSON run report to a new file in an existing directory."
    ),
    make_option(
      c("-v", "--verbose"),
      action = "store_true",
      default = FALSE,
      help = "Print every validation check instead of a compact summary."
    )
  )

  parser <- OptionParser(
    usage = "usage: %prog [options]",
    option_list = option_list,
    description = paste(
      "Validate processed CSV inputs against the modular JSON Schemas.",
      "Use --data-root to point at a project data root.",
      "By default, raw/ is excluded from assessment scanning.",
      "Exit codes: 0 success, 1 invalid data, 2 execution or report export error."
    )
  )

  parsed <- parse_args(parser, args = args, positional_arguments = TRUE)
  data_root <- parsed$options[["data-root"]]
  exclude_dirs <- c("raw", parsed$options[["exclude-dirs"]], parsed$args)
  exclude_dirs <- unique(stats::na.omit(exclude_dirs))

  list(
    data_root = data_root,
    assessment = parsed$options$assessment,
    report = parsed$options$report,
    exclude_dirs = exclude_dirs,
    verbose = isTRUE(parsed$options$verbose)
  )
}

read_project_params <- function(data_root, assessment = NULL) {
  project_config <- load_project_config(data_root, required = FALSE)
  yaml_path <- project_config$path
  if (isTRUE(project_config$exists)) {
    params <- resolve_assessment_params(project_config$data, assessment)
    active_indicators <- params$active_indicators
    if (is.null(active_indicators)) active_indicators <- character()
    if (!length(active_indicators) && !identical(params$qh_method, "assume.0")) {
      stop(
        sprintf("Missing active_indicators in %s.", yaml_path),
        call. = FALSE
      )
    }
    active_indicators <- unique(as.character(unlist(
      active_indicators,
      use.names = FALSE
    )))
    active_indicators <- active_indicators[nzchar(active_indicators)]
    if (!length(active_indicators) && !identical(params$qh_method, "assume.0")) {
      stop(
        sprintf("No active indicators declared in %s.", yaml_path),
        call. = FALSE
      )
    }
    return(list(
      mode = "yaml",
      path = yaml_path,
      data_type = if (!is.null(params$data_type)) {
        as.character(params$data_type)
      } else {
        NA_character_
      },
      qh_method = params$qh_method,
      active_indicators = active_indicators
    ))
  }

  list(
    mode = "legacy",
    path = "<legacy fixed list>",
    data_type = "cover",
    active_indicators = character()
  )
}

#' Resolve the concrete input files required for an assessment
#'
#' The shared requirements manifest describes capabilities rather than one
#' fixed file list. This function turns it into a file plan by always including
#' core files, retaining present optional files, and selecting an available
#' member of each indicator's alternatives. The explicit ambiguity and missing
#' checks prevent the validator from silently choosing a different measurement
#' representation than the active indicator family expects.
#'
#' @param active_indicators Character vector of active indicator codes.
#' @param data_type Project data type, used to prefer stems or cover where
#'   both representations are valid.
#' @param available_files Filenames present in the assessment input folder.
#' @return Unique character vector of files to validate.
#' @keywords internal
build_assessment_file_plan <- function(
  active_indicators,
  data_type,
  available_files,
  qh_method = "empirical"
) {
  if (identical(qh_method, "assume.0")) {
    return("landCover.csv")
  }
  manifest <- validation_requirements()
  manifest_core <- manifest$core
  manifest_indicators <- manifest$indicators

  required <- manifest_core$required
  required <- union(
    required,
    intersect(manifest_core$optional, available_files)
  )
  species_stem_inds <- intersect(active_indicators, c("BA", "BAES", "ASC"))

  for (indicator in active_indicators) {
    if (!indicator %in% names(manifest_indicators)) {
      stop(
        sprintf("No file mapping is defined for indicator '%s'.", indicator),
        call. = FALSE
      )
    }

    spec <- manifest_indicators[[indicator]]
    required <- union(required, spec$required)

    if (!is.null(spec$one_of)) {
      preferred <- if (identical(data_type, "ind")) {
        "speciesStems.csv"
      } else {
        "speciesCover.csv"
      }
      choice <- spec$one_of[[1L]]
      candidate_files <- intersect(choice, available_files)

      if (length(species_stem_inds) == 0) {
        if (length(candidate_files) == 0) {
          stop(
            sprintf(
              "Missing species input for %s; expected one of speciesCover.csv or speciesStems.csv.",
              indicator
            ),
            call. = FALSE
          )
        }
        if (length(candidate_files) > 1) {
          stop(
            paste0(
              "Ambiguous species inputs for ",
              indicator,
              ": both speciesCover.csv and speciesStems.csv are present."
            ),
            call. = FALSE
          )
        }
        required <- union(required, candidate_files)
        next
      }

      # If stem-based indicators are active, keep the indicator-family choice
      # aligned to the requested data type, but don't fail because the other
      # species file is also needed elsewhere.
      if (preferred %in% available_files) {
        required <- union(required, preferred)
      } else {
        fallback <- setdiff(choice, preferred)
        fallback <- fallback[fallback %in% available_files]
        if (!length(fallback)) {
          stop(
            sprintf(
              "Missing species input for %s; expected %s or %s.",
              indicator,
              "speciesCover.csv",
              "speciesStems.csv"
            ),
            call. = FALSE
          )
        }
        required <- union(required, fallback[1])
      }
      next
    }
  }

  unique(required)
}

# Each assessment is a complete report so callers can retain it independently of the project run.
run_assessment_validation <- function(data_root, assessment, schema_root, verbose = FALSE) {
  result <- new_run_report("validate", list(
    data_root = data_root, assessment = assessment
  ))
  result$report_type <- "assessment"
  rows <- list()
  stages <- data.frame(
    stage = c("schema", "crossfile"),
    status = "not_run", reason = "Validation stopped before this stage."
  )
  inputs <- list(algorithm = "csv-md5-v1", captured_at = NULL, stable = NULL, files = list())
  stage <- "setup"
  stage_index <- NULL
  # Store completed fragments outside the try block so a later exception cannot discard them.
  record <- function(fragment) rows[[length(rows) + 1L]] <<- fragment
  set_stage <- function(status, reason = NA_character_) {
    stages$status[stage_index] <<- status
    stages$reason[stage_index] <<- reason
  }
  execution_error <- tryCatch(
    {
      assessment_dir <- normalizePath(file.path(data_root, assessment),
        winslash = "/", mustWork = TRUE
      )
      if (!dir.exists(assessment_dir) || !identical(dirname(assessment_dir), data_root)) {
        stop("Assessment directory must belong to data_root.", call. = FALSE)
      }
      stage <- "configuration"
      assessment_params <- read_project_params(data_root, assessment)
      inputs_dir <- file.path(assessment_dir, "inputs")
      stage <- "input_snapshot"
      before <- snapshot_validation_inputs(inputs_dir)
      inputs$captured_at <- before$captured_at
      inputs$files <- before$files
      file_map <- build_file_map(inputs_dir)
      stage <- "file_plan"
      input_files <- build_assessment_file_plan(
        active_indicators = assessment_params$active_indicators,
        data_type = assessment_params$data_type, available_files = names(file_map),
        qh_method = assessment_params$qh_method
      )
      cat(sprintf(
        "Assessment: %s\n  Inputs: %s\n",
        assessment, paste(input_files, collapse = ", ")
      ))
      if ("coverByStratum.csv" %in% input_files) {
        cat("  Note: coverByStratum.csv schema is provisional.\n")
      }
      stage <- "schema"
      stage_index <- match(stage, stages$stage)
      assessment_report <- run_schema_validation(
        assessment, inputs_dir, input_files, schema_root,
        on_result = record
      )
      if (!nrow(assessment_report)) stop("Schema stage returned no checks.", call. = FALSE)
      set_stage("completed")
      cat("  Schema\n")
      print_validation_report(assessment_report, verbose = verbose, prefix = "schema")

      stage <- "crossfile"
      stage_index <- match(stage, stages$stage)
      if (any(assessment_report$status != "pass")) {
        set_stage("skipped", "Schema validation failed.")
        cat("  Crossfile: skipped because schema validation failed.\n\n")
      } else if (identical(assessment_params$qh_method, "assume.0")) {
        set_stage("not_applicable", "Assumed-zero assessment.")
        cat("  Crossfile: not applicable to an assumed-zero assessment.\n\n")
      } else {
        crossfile_report <- run_crossfile_validation(
          data_root, assessment, assessment_params$active_indicators,
          file_map, assessment_params,
          on_result = record
        )
        if (!nrow(crossfile_report)) stop("Crossfile stage returned no checks.", call. = FALSE)
        set_stage("completed")
        cat("  Crossfile\n")
        print_validation_report(crossfile_report, verbose = verbose, prefix = "crossfile")
        cat("\n")
      }
      NULL
    },
    error = function(e) {
      if (!is.null(stage_index)) set_stage("error", conditionMessage(e))
      list(message = conditionMessage(e), stage = stage)
    }
  )
  if (!is.null(inputs$captured_at)) {
    # shortcut: endpoint hashes miss reverted edits; use immutable inputs for stronger guarantees.
    snapshot_error <- tryCatch(
      {
        after <- snapshot_validation_inputs(inputs_dir)
        inputs$stable <- identical(inputs$files, after$files)
        if (!inputs$stable) stop("Input files changed during validation.", call. = FALSE)
        NULL
      },
      error = function(e) {
        list(message = conditionMessage(e), stage = "input_snapshot")
      }
    )
    if (!is.null(snapshot_error)) {
      inputs$stable <- FALSE
      if (is.null(execution_error)) execution_error <- snapshot_error
    }
  }
  # Single-bracket assignment retains a required null instead of deleting the error field.
  result["error"] <- list(execution_error)
  report <- bind_validation_reports(rows)
  # Assessment and stage context belongs to the enclosing groups, not every check.
  groups <- lapply(seq_len(nrow(stages)), function(i) {
    list(
      stage = stages$stage[i], status = stages$status[i], reason = stages$reason[i],
      checks = report[report$stage == stages$stage[i],
        setdiff(names(report), c("assessment", "stage")),
        drop = FALSE
      ]
    )
  })
  result$validation <- list(stages = groups, inputs = inputs)
  result$outcome <- if (!is.null(execution_error)) {
    "execution_error"
  } else if (any(report$status != "pass")) {
    "invalid_data"
  } else {
    "success"
  }
  result$ended_at <- run_report_time()
  validated_run_report_json(result)
  result
}

# Only the CLI exits; callers retain shared errors and independently completed assessment reports.
run_validation <- function(data_root, exclude_dirs = "raw", verbose = FALSE, assessment = NULL) {
  valid_data_root <- is.character(data_root) && length(data_root) == 1L &&
    !is.na(data_root) && nzchar(data_root)
  assessment_name <- is.character(assessment) && length(assessment) == 1L && !is.na(assessment)
  valid_assessment <- assessment_name && grepl("^[[:alnum:]_-]+$", assessment)
  # I() keeps empty and singleton scope collections as JSON arrays.
  result <- new_run_report("validate", list(
    type = if (is.null(assessment)) "project" else "assessment",
    data_root = if (valid_data_root) data_root else NULL,
    requested_assessment = if (assessment_name) assessment else NULL,
    assessments = I(character()), excluded_dirs = I(exclude_dirs)
  ))
  result$validation <- list(assessments = list())
  stage <- "setup"
  execution_error <- tryCatch(
    {
      if (!valid_data_root) {
        stop("Please provide --data-root pointing to the root of the data folder.", call. = FALSE)
      }
      if (!dir.exists(data_root)) stop("Data directory does not exist: ", data_root, call. = FALSE)
      data_root <- normalizePath(data_root, winslash = "/", mustWork = TRUE)
      result$scope$data_root <- data_root
      schema_root <- file.path(getwd(), "schema")
      if (!dir.exists(schema_root)) stop("Run `validate.R` from VQA root.", call. = FALSE)
      stage <- "selection"
      assessments <- list_assessments(data_root, exclude_dirs)
      if (!is.null(assessment)) {
        if (!valid_assessment) {
          stop("assessment must be one directory name within data_root.", call. = FALSE)
        }
        if (assessment %in% exclude_dirs) stop("Requested assessment is excluded.", call. = FALSE)
        if (!assessment %in% assessments) {
          stop("Requested assessment does not exist.", call. = FALSE)
        }
        assessments <- assessment
      }
      result$scope$assessments <- I(assessments)
      if (!length(assessments)) stop("No assessments found in the selected scope.", call. = FALSE)
      stage <- "configuration"
      # Catch shared YAML parse errors before assessment-local resolution and validation.
      load_project_config(data_root, required = FALSE)
      cat("Validating processed inputs with jsonvalidate (ajv engine)\n")
      cat(sprintf("Schema root: %s\nData root: %s\n\n", schema_root, data_root))
      stage <- "assessment_reporting"
      for (name in assessments) {
        # Each child catches its own execution errors so later assessments still get checked.
        result$validation$assessments[[length(result$validation$assessments) + 1L]] <-
          run_assessment_validation(data_root, name, schema_root, verbose)
      }
      NULL
    },
    error = function(e) {
      list(message = conditionMessage(e), stage = stage)
    }
  )
  result["error"] <- list(execution_error)
  outcomes <- vapply(result$validation$assessments, function(child) child$outcome, character(1))
  result$outcome <- if (!is.null(execution_error) || "execution_error" %in% outcomes) {
    "execution_error"
  } else if ("invalid_data" %in% outcomes) {
    "invalid_data"
  } else {
    "success"
  }
  result$ended_at <- run_report_time()
  validated_run_report_json(result)
  result
}

# Reconstruct transient flat rows so the existing console renderer can keep its context labels.
validation_report_rows <- function(result) {
  children <- if (result$report_type == "assessment") {
    list(result)
  } else {
    result$validation$assessments
  }
  bind_validation_reports(lapply(children, function(child) {
    bind_validation_reports(lapply(child$validation$stages, function(group) {
      checks <- group$checks
      checks$assessment <- rep(child$scope$assessment, nrow(checks))
      checks$stage <- rep(group$stage, nrow(checks))
      checks[names(bind_validation_reports())]
    }))
  }))
}

print_validation_summary <- function(result, verbose = FALSE) {
  children <- result$validation$assessments
  report <- validation_report_rows(result)
  counts <- report_counts(report)
  cat("Summary\n")
  cat(sprintf(
    "  Passed: %d\n  Failed: %d\n  Warnings: %d\n",
    counts$pass, counts$fail, counts$warn
  ))
  failures <- report[report$status != "pass", , drop = FALSE]
  if (nrow(failures)) {
    cat("\nFailed checks\n")
    print_validation_report(failures, verbose = verbose, prefix = "failed")
  }
  for (child in children) {
    cat("  Assessment ", child$scope$assessment, ": ", child$outcome, "\n", sep = "")
  }
  cat("  Outcome: ", result$outcome, "\n", sep = "")
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  tryCatch(
    {
      cli <- parse_cli_args(args)
      destination <- if (!is.null(cli$report)) prepare_report_destination(cli$report) else NULL
      result <- run_validation(cli$data_root, cli$exclude_dirs, cli$verbose, cli$assessment)
      print_validation_summary(result)
      if (!is.null(result$error)) message("Validation error: ", result$error$message)
      for (child in result$validation$assessments) {
        if (!is.null(child$error)) {
          message("Assessment ", child$scope$assessment, ": ", child$error$message)
        }
      }
      if (!is.null(destination)) write_run_report(result, destination)
      run_report_exit_status(result)
    },
    error = function(e) {
      message("Validation/export error: ", conditionMessage(e))
      2L
    }
  )
}

if (sys.nframe() == 0L) {
  # Sourcing defines the callable workflow without running it or exiting R.
  quit(status = main())
}

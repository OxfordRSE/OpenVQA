#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
})

source(file.path("validation", "helpers.R"))
source(file.path("pipeline", "io.R"))
source(file.path("validation", "requirements.R"))
source(file.path("validation", "schema.R"))
source(file.path("validation", "crossfile.R"))

parse_cli_args <- function() {
  # optparse handles `-h/--help` for us and accepts both
  # `--data-root /path` and `--data-root=/path`.
  option_list <- list(
    make_option(
      c("-d", "--data-root"),
      type = "character",
      help = "Root data directory that contains the assessment folders."
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
      "By default, raw/ is excluded from assessment scanning."
    )
  )

  parsed <- parse_args(parser, positional_arguments = TRUE)
  data_root <- parsed$options[["data-root"]]
  if (is.null(data_root) || !nzchar(data_root)) {
    stop(
      "Please provide --data-root pointing to the root of the data folder.",
      call. = FALSE
    )
  }

  exclude_dirs <- c("raw", parsed$options[["exclude-dirs"]], parsed$args)
  exclude_dirs <- unique(stats::na.omit(exclude_dirs))

  list(
    data_root = normalizePath(data_root, mustWork = TRUE),
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

main <- function() {
  # Check script is being run from the VQA root by verifying the expected schema path exists
  cwd <- getwd()
  schema_root <- file.path(cwd, "schema")
  if (!check_dir_exist(schema_root)) {
    sprintf("Schema directory does not exist: %s", schema_root)
    stop("Run `validate.R` from VQA root.", call. = FALSE)
  }

  cli <- parse_cli_args()
  data_root <- cli$data_root
  if (!check_dir_exist(data_root)) {
    stop(sprintf("Data directory does not exist: %s", data_root), call. = FALSE)
  }

  project_params <- read_project_params(data_root)

  # Explore the assessment folders in the data root
  assessments <- list.dirs(data_root, full.names = FALSE, recursive = FALSE)
  assessments <- setdiff(assessments, cli$exclude_dirs)
  report <- list()

  cat("Validating processed inputs with jsonvalidate (ajv engine)\n")
  cat(sprintf("Schema root: %s\n", schema_root))
  cat(sprintf("Data root: %s\n\n", data_root))
  cat(sprintf("Params source: %s\n", project_params$path))
  cat(sprintf("Params mode: %s\n", project_params$mode))
  if (length(project_params$active_indicators) > 0) {
    cat(sprintf(
      "Active indicators: %s\n\n",
      paste(project_params$active_indicators, collapse = ", ")
    ))
  } else {
    cat("\n")
  }
  cat("Excluded directories:\n")
  for (dir_name in cli$exclude_dirs) {
    cat(sprintf(" - %s\n", dir_name))
  }
  cat("\n")

  for (assessment in assessments) {
    assessment_params <- read_project_params(data_root, assessment)
    # Keep the report grouped by assessment so failures are easy to scan.
    inputs_dir <- file.path(data_root, assessment, "inputs")
    file_map <- build_file_map(inputs_dir)
    available_files <- names(file_map)
    input_files <- build_assessment_file_plan(
      active_indicators = assessment_params$active_indicators,
      data_type = assessment_params$data_type,
      available_files = available_files,
      qh_method = assessment_params$qh_method
    )

    cat(sprintf("Assessment: %s\n", assessment))
    cat(sprintf("  Inputs: %s\n", paste(input_files, collapse = ", ")))
    if ("coverByStratum.csv" %in% input_files) {
      cat("  Note: coverByStratum.csv schema is provisional.\n")
    }

    assessment_report <- run_schema_validation(
      assessment = assessment,
      inputs_dir = inputs_dir,
      input_files = input_files,
      schema_root = schema_root
    )
    report[[length(report) + 1L]] <- assessment_report

    cat("  Schema\n")
    print_validation_report(
      assessment_report[assessment_report$stage == "schema", , drop = FALSE],
      verbose = cli$verbose,
      prefix = "schema"
    )

    if (any(assessment_report$status != "pass")) {
      cat("  Crossfile: skipped because schema validation failed.\n\n")
      next
    }

    if (identical(assessment_params$qh_method, "assume.0")) {
      cat("  Crossfile: not applicable to an assumed-zero assessment.\n\n")
      next
    }

    crossfile_report <- run_crossfile_validation(
      data_root = data_root,
      assessment_path = assessment,
      active_indicators = assessment_params$active_indicators,
      file_map = file_map,
      project_params = assessment_params
    )
    if (nrow(crossfile_report) > 0) {
      report[[length(report) + 1L]] <- crossfile_report
    }
    cat("  Crossfile\n")
    print_validation_report(
      crossfile_report,
      verbose = cli$verbose,
      prefix = "crossfile"
    )
    cat("\n")
  }

  report_df <- bind_validation_reports(report)
  failures <- report_df[report_df$status != "pass", , drop = FALSE]
  summary_counts <- report_counts(report_df)

  cat("Summary\n")
  cat(sprintf("  Passed: %d\n", summary_counts$pass))
  cat(sprintf("  Failed: %d\n", summary_counts$fail))
  cat(sprintf("  Warnings: %d\n", summary_counts$warn))

  if (nrow(failures) > 0) {
    # Non-zero exit keeps this usable in shell scripts and CI.
    cat("\nFailed checks\n")
    print_validation_report(failures, verbose = FALSE, prefix = "failed")
    quit(status = 1)
  }

  invisible(report_df)
}

if (Sys.getenv("VQA_VALIDATE_NO_AUTORUN", "") != "1") {
  # Allow sourcing the file without immediately running validation, which is
  # useful for debugging helpers and inspecting resolved schemas.
  main()
}

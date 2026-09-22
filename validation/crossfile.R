# Stage 2 cross-file validation helpers.

source(file.path("pipeline", "io.R"))
source(file.path("validation", "requirements.R"))

#' Construct a cross-file validation result
#'
#' Cross-file checks need stable severity and status semantics so missing
#' dependencies, hard relationship failures, and warning-level consistency
#' diagnostics can be combined with schema results.
#'
#' @param check_id Stable check identifier.
#' @param severity Severity label.
#' @param status `pass` or `fail`.
#' @param message Human-readable result.
#' @param file Related input filename.
#' @param details Optional diagnostic details.
#' @return One-row cross-file report data frame.
#' @keywords internal
make_result <- function(
  check_id,
  severity,
  status,
  message,
  file = NULL,
  details = NULL
) {
  validation_report_row(
    stage = "crossfile",
    check_id = check_id,
    severity = severity,
    status = status,
    message = message,
    file = if (is.null(file)) NA_character_ else as.character(file),
    details = if (is.null(details)) NA_character_ else as.character(details)
  )
}

#' Create an informational passing result
#'
#' A dedicated constructor keeps successful checks identical to failures in
#' structure, so callers can append results without special cases while still
#' preserving the check's stable identifier and diagnostic context.
#'
#' @inheritParams make_result
#' @return One-row passing report data frame.
#' @keywords internal
pass_result <- function(check_id, message, file = NULL, details = NULL) {
  make_result(check_id, "info", "pass", message, file = file, details = details)
}

#' Create a warning-severity result
#'
#' Warning checks remain visible to users even when their status is `pass`,
#' which is useful for soft consistency checks that should not block analysis.
#'
#' @inheritParams make_result
#' @return One-row warning report data frame.
#' @keywords internal
warn_result <- function(check_id, status, message, file = NULL, details = NULL) {
  make_result(check_id, "warning", status, message, file = file, details = details)
}

#' Create a failing cross-file result
#'
#' Cross-file failures are represented as report rows rather than exceptions
#' so one assessment can show all broken relationships in a single run and
#' the command-line entry point can decide the final exit status centrally.
#'
#' @inheritParams make_result
#' @return One-row error report data frame.
#' @keywords internal
error_result <- function(check_id, message, file = NULL, details = NULL) {
  make_result(check_id, "error", "fail", message, file = file, details = details)
}

#' Check that a table has one row per declared key
#'
#' Duplicate lookup or grain keys can multiply later joins and invalidate
#' indicator results. The check reports representative duplicate rows so the
#' source table can be corrected before relational checks proceed.
#'
#' @param df Data frame to inspect.
#' @param keys Columns that should be unique together.
#' @param file_name Contract filename for diagnostics.
#' @return One-row validation report.
#' @keywords internal
check_unique_keys <- function(df, keys, file_name) {
  if (is.null(df)) {
    return(error_result(
      paste0(file_name, ".present"),
      sprintf("File is missing: %s", file_name),
      file = file_name
    ))
  }

  dup_rows <- duplicate_rows(df, keys)
  if (nrow(dup_rows) > 0) {
    details <- paste(head(format_key_values(dup_rows, keys), 10), collapse = " | ")
    return(error_result(
      paste0(file_name, ".unique_keys"),
      sprintf(
        "%s has duplicate rows for key(s): %s.",
        file_name,
        paste(keys, collapse = ", ")
      ),
      file = file_name,
      details = details
    ))
  }

  pass_result(
    paste0(file_name, ".unique_keys"),
    sprintf("%s has unique key(s): %s.", file_name, paste(keys, collapse = ", ")),
    file = file_name
  )
}

#' Check that every child key has a parent lookup row
#'
#' Measurement and metadata tables are expected to reference standardized
#' lookup rows. This check reports missing parent keys explicitly instead of
#' allowing an outer or incomplete merge to create silent `NA` metadata.
#'
#' @param child Child data frame.
#' @param parent Parent/lookup data frame.
#' @param by Shared key columns.
#' @param child_name Child filename for diagnostics.
#' @param parent_name Parent filename for diagnostics.
#' @return One-row validation report.
#' @keywords internal
check_fk_exists <- function(child, parent, by, child_name, parent_name) {
  missing <- anti_join_rows(child, parent, by)
  if (nrow(missing) > 0) {
    details <- paste(head(format_key_values(missing, by), 10), collapse = " | ")
    return(error_result(
      paste0(child_name, ".fk_", parent_name),
      sprintf(
        "%s contains %d row(s) without a matching %s row on (%s).",
        child_name,
        nrow(missing),
        parent_name,
        paste(by, collapse = ", ")
      ),
      file = child_name,
      details = details
    ))
  }

  pass_result(
    paste0(child_name, ".fk_", parent_name),
    sprintf(
      "%s matches %s on (%s).",
      child_name,
      parent_name,
      paste(by, collapse = ", ")
    ),
    file = child_name
  )
}

#' Check that a lookup merge preserves the left table's row count
#'
#' A foreign-key membership check alone does not detect duplicate parent keys.
#' This check catches the row multiplication that would occur when downstream
#' code performs the same many-to-one lookup merge.
#'
#' @param left Left/child data frame.
#' @param right Right/lookup data frame.
#' @param by Join key columns.
#' @param left_name Left filename for diagnostics.
#' @param right_name Right filename for diagnostics.
#' @return One-row validation report.
#' @keywords internal
check_merge_cardinality <- function(left, right, by, left_name, right_name) {
  joined <- merge(left, unique(right[, by, drop = FALSE]), by = by, all.x = TRUE, sort = FALSE)
  if (nrow(joined) != nrow(left)) {
    return(error_result(
      paste0(left_name, ".merge_", right_name),
      sprintf(
        "Joining %s to %s on (%s) changes row count from %d to %d.",
        left_name,
        right_name,
        paste(by, collapse = ", "),
        nrow(left),
        nrow(joined)
      ),
      file = left_name
    ))
  }

  pass_result(
    paste0(left_name, ".merge_", right_name),
    sprintf(
      "Joining %s to %s on (%s) preserves row count.",
      left_name,
      right_name,
      paste(by, collapse = ", ")
    ),
    file = left_name
  )
}

#' Check and report a mutually exclusive input choice
#'
#' Some indicators can consume either cover or stem data, but accepting both
#' would make the chosen analysis path ambiguous. This helper gives callers a
#' reusable hard failure for missing or ambiguous alternatives.
#'
#' @param file_choices Allowed alternative filenames.
#' @param file_map Named available-file map.
#' @param context_label Indicator or dependency label.
#' @return One-row validation report.
#' @keywords internal
check_only_one_present <- function(file_choices, file_map, context_label) {
  present <- file_choices[vapply(file_choices, function(x) is_present(file_map, x), logical(1))]

  if (length(present) == 0) {
    return(error_result(
      paste0(context_label, ".choice"),
      sprintf(
        "Expected one of %s for %s, but none are present.",
        paste(file_choices, collapse = ", "),
        context_label
      ),
      file = context_label
    ))
  }

  if (length(present) > 1) {
    return(error_result(
      paste0(context_label, ".choice"),
      sprintf(
        "Expected only one of %s for %s, but found: %s.",
        paste(file_choices, collapse = ", "),
        context_label,
        paste(present, collapse = ", ")
      ),
      file = context_label
    ))
  }

  pass_result(
    paste0(context_label, ".choice"),
    sprintf("Selected input for %s: %s.", context_label, present[[1]]),
    file = context_label,
    details = present[[1]]
  )
}

#' Validate core and active-indicator file dependencies
#'
#' The shared requirements contract is consumed here so cross-file reporting
#' agrees with the schema-stage file plan. Core files are checked once, while
#' indicator-specific required and one-of dependencies are reported per active
#' indicator.
#'
#' @param active_indicators Indicator codes selected for the assessment.
#' @param file_map Named available-file map.
#' @param project_params Parsed project parameters (reserved for future
#'   conditional requirements).
#' @return Cross-file dependency report.
#' @keywords internal
validate_indicator_dependencies <- function(
  active_indicators,
  file_map,
  project_params
) {
  manifest <- validation_requirements()
  manifest_core <- manifest$core
  manifest_indicators <- manifest$indicators

  results <- list()
  idx <- 0L

  core_required <- manifest_core$required
  missing_core <- core_required[
    !vapply(core_required, function(x) is_present(file_map, x), logical(1))
  ]
  idx <- idx + 1L
  if (length(missing_core) > 0) {
    results[[idx]] <- error_result(
      "core.required",
      sprintf(
        "Assessment is missing required core file(s): %s.",
        paste(missing_core, collapse = ", ")
      ),
      details = paste(missing_core, collapse = ", ")
    )
  } else {
    results[[idx]] <- pass_result(
      "core.required",
      sprintf(
        "Required core files are present: %s.",
        paste(core_required, collapse = ", ")
      ),
      details = paste(core_required, collapse = ", ")
    )
  }

  unknown <- setdiff(active_indicators, names(manifest_indicators))
  if (length(unknown) > 0) {
    idx <- idx + 1L
    results[[idx]] <- error_result(
      "indicator.unknown",
      sprintf("Unknown active indicator(s): %s.", paste(unknown, collapse = ", ")),
      details = paste(unknown, collapse = ", ")
    )
  } else {
    idx <- idx + 1L
    results[[idx]] <- pass_result(
      "indicator.known",
      sprintf("Recognized active indicators: %s.", paste(active_indicators, collapse = ", "))
    )
  }

  for (indicator in active_indicators) {
    spec <- manifest_indicators[[indicator]]
    if (is.null(spec)) {
      next
    }

    required <- spec$required
    missing_required <- required[!vapply(required, function(x) is_present(file_map, x), logical(1))]

    idx <- idx + 1L
    if (length(missing_required) > 0) {
      results[[idx]] <- error_result(
        paste0("indicator.", indicator, ".required"),
        sprintf(
          "%s is missing required file(s): %s.",
          indicator,
          paste(missing_required, collapse = ", ")
        ),
        details = paste(missing_required, collapse = ", ")
      )
      next
    }

    if (!is.null(spec$one_of)) {
      choice_results <- lapply(
        seq_along(spec$one_of),
        function(i) {
          choice <- spec$one_of[[i]]
          present <- choice[vapply(choice, function(x) is_present(file_map, x), logical(1))]
          if (!length(present)) {
            return(NULL)
          }
          pass_result(
            paste0("indicator.", indicator, ".choice.", i),
            sprintf("%s can use %s.", indicator, paste(present, collapse = ", ")),
            details = paste(present, collapse = ", ")
          )
        }
      )
      choice_results <- Filter(Negate(is.null), choice_results)
      results <- c(results, choice_results)
    } else {
      results[[idx]] <- pass_result(
        paste0("indicator.", indicator, ".required"),
        sprintf("%s required files are present.", indicator)
      )
    }
  }

  bind_validation_reports(results)
}

#' Validate the plot metadata lookup key
#'
#' plotMetadata is a core table because every indicator joins measurement rows
#' to it. Enforcing unique `plotCode` prevents those joins from multiplying
#' rows and changing indicator grain.
#'
#' @param plot_metadata Plot metadata data frame.
#' @return Cross-file validation report.
#' @keywords internal
validate_plot_metadata <- function(plot_metadata) {
  results <- list(
    check_unique_keys(plot_metadata, "plotCode", "plotMetadata.csv")
  )
  bind_validation_reports(results)
}

#' Validate the optional species lookup key
#'
#' Species lookup identity is useful when supplied, but the legacy contract
#' allows files without `speciesID`; in that case membership checks still use
#' species names and the unavailable identity check is reported informationally.
#'
#' @param species_df Species lookup data frame.
#' @return Cross-file validation report.
#' @keywords internal
validate_species_lookup <- function(species_df) {
  if ("speciesID" %in% names(species_df)) {
    return(bind_validation_reports(
      check_unique_keys(species_df, "speciesID", "species.csv")
    ))
  }

  pass_result(
    "species.csv.lookup",
    "species.csv does not expose speciesID; lookup uniqueness is not checked."
  )
}

#' Validate vegetation lookup relationships when the optional file is present
#'
#' `vegetation.csv` is a one-row-per-`vegClass` lookup. Plot metadata and
#' land-cover summaries may contain many rows per class, so both membership and
#' row-preserving lookup behavior must be checked before report enrichment.
#'
#' @param vegetation Vegetation lookup data frame.
#' @param plot_metadata Plot metadata data frame, if available.
#' @param land_cover Land-cover summary data frame, if available.
#' @return Cross-file validation report.
#' @keywords internal
validate_vegetation_lookup <- function(
  vegetation,
  plot_metadata,
  land_cover
) {
  results <- list(
    check_unique_keys(vegetation, "vegClass", "vegetation.csv")
  )

  if (!is.null(plot_metadata)) {
    results <- c(
      results,
      list(
        check_fk_exists(
          child = plot_metadata,
          parent = vegetation,
          by = "vegClass",
          child_name = "plotMetadata.csv",
          parent_name = "vegetation.csv"
        ),
        check_merge_cardinality(
          left = plot_metadata,
          right = vegetation,
          by = "vegClass",
          left_name = "plotMetadata.csv",
          right_name = "vegetation.csv"
        )
      )
    )
  }

  if (!is.null(land_cover)) {
    results <- c(
      results,
      list(
        check_fk_exists(
          child = land_cover,
          parent = vegetation,
          by = "vegClass",
          child_name = "landCover.csv",
          parent_name = "vegetation.csv"
        ),
        check_merge_cardinality(
          left = land_cover,
          right = vegetation,
          by = "vegClass",
          left_name = "landCover.csv",
          right_name = "vegetation.csv"
        ),
        check_vegetation_type_consistency(land_cover, vegetation)
      )
    )
  }

  bind_validation_reports(results)
}

#' Compare land-cover and vegetation structural type metadata
#'
#' Both tables carry `lc.type` because the import materializes lookup metadata
#' into the land-cover summary. Differences are reported as warnings: they are
#' important for detecting stale metadata but do not invalidate indicator rows.
#'
#' @param land_cover Land-cover summary data frame.
#' @param vegetation Vegetation lookup data frame.
#' @return One-row warning-level validation report.
#' @keywords internal
check_vegetation_type_consistency <- function(land_cover, vegetation) {
  land_cover_types <- unique(
    land_cover[, c("vegClass", "lc.type"), drop = FALSE]
  )
  vegetation_types <- vegetation[, c("vegClass", "lc.type"), drop = FALSE]
  names(vegetation_types)[names(vegetation_types) == "lc.type"] <-
    "vegetation_lc.type"

  joined <- merge(
    land_cover_types,
    vegetation_types,
    by = "vegClass",
    all.x = TRUE,
    sort = FALSE
  )

  mismatch_rows <- with(
    joined,
    is.na(vegetation_lc.type) |
      lc.type != vegetation_lc.type
  )

  mismatch <- joined[mismatch_rows, , drop = FALSE]

  if (nrow(mismatch) > 0) {
    return(warn_result(
      "landCover.csv.vegetation_lc_type",
      "pass",
      "landCover.csv lc.type values differ from vegetation.csv.",
      file = "landCover.csv",
      details = paste(
        head(
          apply(
            mismatch[, c("vegClass", "lc.type", "vegetation_lc.type")],
            1L,
            function(row) paste(sprintf("%s=%s", names(row), row), collapse = ", ")
          ),
          10
        ),
        collapse = " | "
      )
    ))
  }

  warn_result(
    "landCover.csv.vegetation_lc_type",
    "pass",
    "landCover.csv lc.type values agree with vegetation.csv.",
    file = "landCover.csv"
  )
}

#' Run metadata-driven checks for one measurement table
#'
#' Measurement files differ in grain and foreign keys, so their checks are
#' described by a specification rather than duplicated in each indicator
#' validator. This function applies those checks and any file-specific callback
#' while preserving a common report format.
#'
#' @param df Measurement data frame.
#' @param plot_metadata Core plot metadata.
#' @param spec File validation specification.
#' @param inputs All loaded inputs for callback checks.
#' @return Cross-file validation report.
#' @keywords internal
validate_measurement_file <- function(df, plot_metadata, spec, inputs = list()) {
  results <- list()
  idx <- 0L

  if (!is.null(spec$grain_keys)) {
    idx <- idx + 1L
    results[[idx]] <- check_unique_keys(df, spec$grain_keys, spec$file_name)
  }

  if (!is.null(spec$plot_fk_keys)) {
    idx <- idx + 1L
    results[[idx]] <- check_fk_exists(
      child = df,
      parent = plot_metadata,
      by = spec$plot_fk_keys,
      child_name = spec$file_name,
      parent_name = "plotMetadata.csv"
    )

    idx <- idx + 1L
    results[[idx]] <- check_merge_cardinality(
      left = df,
      right = plot_metadata,
      by = spec$plot_fk_keys,
      left_name = spec$file_name,
      right_name = "plotMetadata.csv"
    )
  }

  if (!is.null(spec$extra_check) && is.function(spec$extra_check)) {
    extra <- spec$extra_check(df = df, inputs = inputs, spec = spec)
    if (!is.null(extra)) {
      results <- c(results, extra)
    }
  }

  bind_validation_reports(results)
}

#' Check species values against the species lookup
#'
#' Missing species values are allowed by some measurement schemas, but any
#' observed non-missing species name must resolve to the lookup used for exotic
#' status and related classifications.
#'
#' @param df Measurement data frame containing `species`.
#' @param species_df Species lookup data frame.
#' @param file_name Measurement filename.
#' @return One-row validation report, or `NULL` without a lookup.
#' @keywords internal
check_species_lookup_membership <- function(df, species_df, file_name) {
  if (is.null(species_df)) {
    return(NULL)
  }

  observed <- unique(df$species[!is.na(df$species)])
  allowed <- unique(species_df$species[!is.na(species_df$species)])
  missing <- setdiff(observed, allowed)
  if (length(missing) > 0) {
    return(error_result(
      paste0(file_name, ".species_lookup"),
      sprintf(
        "%s contains species values not found in species.csv: %s.",
        file_name,
        paste(missing, collapse = ", ")
      ),
      file = file_name,
      details = paste(missing, collapse = ", ")
    ))
  }

  pass_result(
    paste0(file_name, ".species_lookup"),
    sprintf("%s species values are present in species.csv.", file_name),
    file = file_name
  )
}

#' Check exotic cover against growth-form cover totals
#'
#' Exotic cover should be a subset of the corresponding total growth-form
#' cover. The comparison is warning-level because source projects may have
#' legitimate aggregation differences that require review rather than an
#' automatic validation stop.
#'
#' @param df Exotic-cover data frame.
#' @param growth_form_df Growth-form cover data frame.
#' @param file_name Exotic-cover filename.
#' @return Warning or passing validation report.
#' @keywords internal
check_exotic_subset <- function(df, growth_form_df, file_name) {
  if (is.null(growth_form_df)) {
    return(warn_result(
      paste0(file_name, ".growth_form_subset"),
      "pass",
      sprintf(
        "%s growth-form comparison skipped because coverByGrowthForm.csv is unavailable.",
        file_name
      ),
      file = file_name
    ))
  }

  by <- c("plotCode", "stratum")
  gf_keys <- paste(
    ifelse(is.na(growth_form_df$plotCode), "<NA>", as.character(growth_form_df$plotCode)),
    ifelse(is.na(growth_form_df$stratum), "<NA>", as.character(growth_form_df$stratum)),
    sep = "||"
  )
  exotic_keys <- paste(
    ifelse(is.na(df$plotCode), "<NA>", as.character(df$plotCode)),
    ifelse(is.na(df$stratum), "<NA>", as.character(df$stratum)),
    sep = "||"
  )

  missing <- df[!(exotic_keys %in% unique(gf_keys)), , drop = FALSE]
  if (nrow(missing) > 0) {
    return(warn_result(
      paste0(file_name, ".growth_form_subset"),
      "pass",
      sprintf(
        "%s has rows not present in coverByGrowthForm.csv on (%s).",
        file_name,
        paste(by, collapse = ", ")
      ),
      file = file_name,
      details = paste(head(format_key_values(missing, by), 10), collapse = " | ")
    ))
  }

  merged <- merge(
    df[, c("plotCode", "stratum", "cover"), drop = FALSE],
    growth_form_df[, c("plotCode", "stratum", "cover"), drop = FALSE],
    by = by,
    suffixes = c(".exotic", ".growth"),
    all.x = TRUE,
    sort = FALSE
  )
  if (any(is.na(merged$cover.growth) | merged$cover.exotic > merged$cover.growth, na.rm = TRUE)) {
    offenders <- merged[
      is.na(merged$cover.growth) | merged$cover.exotic > merged$cover.growth, ,
      drop = FALSE
    ]
    return(warn_result(
      paste0(file_name, ".cover_subset"),
      "pass",
      sprintf(
        "%s has cover values greater than coverByGrowthForm.csv on shared keys.",
        file_name
      ),
      file = file_name,
      details = paste(
        head(
          apply(
            offenders[, c("plotCode", "stratum", "cover.exotic", "cover.growth")],
            1L,
            function(row) {
              paste(sprintf("%s=%s", names(row), row), collapse = ", ")
            }
          ),
          10
        ),
        collapse = " | "
      )
    ))
  }

  warn_result(
    paste0(file_name, ".growth_form_subset"),
    "pass",
    sprintf("%s rows are compatible with coverByGrowthForm.csv.", file_name),
    file = file_name
  )
}

#' Validate the land-cover summary against plot metadata
#'
#' `landCover.csv` is a derived summary, but it remains part of the core
#' contract because quality-hectare calculations need its land-cover records.
#' The validator therefore checks both its own summary grain and whether its
#' focal and benchmark counts agree with the plot-level source. Count
#' differences are warnings: they identify stale or independently generated
#' summaries without treating a derived-table discrepancy as a malformed row.
#'
#' @param land_cover Land-cover summary data frame.
#' @param plot_metadata Plot-level metadata used as the source for expected
#'   counts.
#' @return Cross-file validation report.
#' @keywords internal
validate_land_cover <- function(land_cover, plot_metadata) {
  results <- list()
  idx <- 0L

  idx <- idx + 1L
  results[[idx]] <- check_unique_keys(land_cover, "landCover", "landCover.csv")

  focal_counts <- aggregate(
    plotCode ~ landCover,
    data = plot_metadata[plot_metadata$focalOrBenchmark == "f", , drop = FALSE],
    FUN = length
  )
  bm_counts <- aggregate(
    plotCode ~ vegClass,
    data = plot_metadata[plot_metadata$focalOrBenchmark == "b", , drop = FALSE],
    FUN = length
  )
  names(focal_counts)[2] <- "expected_focal_plots"
  names(bm_counts)[2] <- "expected_bm_plots"

  expected <- merge(
    land_cover[, c("landCover", "vegClass", "focal_plots", "bm_plots"), drop = FALSE],
    focal_counts,
    by = "landCover",
    all.x = TRUE,
    sort = FALSE
  )
  expected <- merge(
    expected,
    bm_counts,
    by = "vegClass",
    all.x = TRUE,
    sort = FALSE
  )

  mismatch <- expected[
    is.na(expected$expected_focal_plots) |
      is.na(expected$expected_bm_plots) |
      expected$focal_plots != expected$expected_focal_plots |
      expected$bm_plots != expected$expected_bm_plots, ,
    drop = FALSE
  ]

  idx <- idx + 1L
  if (nrow(mismatch) > 0) {
    results[[idx]] <- warn_result(
      "landCover.summary_counts",
      "pass",
      "landCover.csv summary counts differ from plotMetadata-derived counts.",
      file = "landCover.csv",
      details = paste(
        head(
          apply(
            mismatch[, c(
              "landCover",
              "vegClass",
              "focal_plots",
              "expected_focal_plots",
              "bm_plots",
              "expected_bm_plots"
            )],
            1L,
            function(row) paste(sprintf("%s=%s", names(row), row), collapse = ", ")
          ),
          10
        ),
        collapse = " | "
      )
    )
  } else {
    results[[idx]] <- warn_result(
      "landCover.summary_counts",
      "pass",
      "landCover.csv summary counts match plotMetadata-derived counts.",
      file = "landCover.csv"
    )
  }

  bind_validation_reports(results)
}

#' Validate stem observations and their lookup relationships
#'
#' Stem rows carry plot identity and species identity that are consumed by
#' stem-based indicators. Checking the stem grain prevents duplicate stems,
#' checking plot keys prevents observations from silently attaching to the
#' wrong plot, and checking the species lookup prevents downstream joins from
#' producing missing classifications.
#'
#' @param species_stems Stem-observation data frame.
#' @param plot_metadata Plot metadata used as the parent table.
#' @param species_df Species lookup data frame, if available.
#' @return Cross-file validation report.
#' @keywords internal
validate_species_stems <- function(species_stems, plot_metadata, species_df) {
  results <- list()
  idx <- 0L

  idx <- idx + 1L
  results[[idx]] <- check_unique_keys(
    species_stems, c("plotCode", "stem_id"), "speciesStems.csv"
  )

  idx <- idx + 1L
  results[[idx]] <- check_fk_exists(
    child = species_stems,
    parent = plot_metadata,
    by = c("plotCode", "focalOrBenchmark", "landCover", "vegClass"),
    child_name = "speciesStems.csv",
    parent_name = "plotMetadata.csv"
  )

  idx <- idx + 1L
  results[[idx]] <- check_merge_cardinality(
    left = species_stems,
    right = plot_metadata,
    by = c("plotCode", "focalOrBenchmark", "landCover", "vegClass"),
    left_name = "speciesStems.csv",
    right_name = "plotMetadata.csv"
  )

  idx <- idx + 1L
  results[[idx]] <- check_species_lookup_membership(
    species_stems,
    species_df,
    "speciesStems.csv"
  )

  bind_validation_reports(results)
}

#' Validate species-cover observations and their lookup relationships
#'
#' Species cover is keyed by plot and species, while its plot classification
#' comes from `plotMetadata.csv`. These checks make the implicit joins used by
#' indicator calculations explicit, so duplicate observations, orphaned plot
#' keys, and unknown species cannot be hidden by a later merge.
#'
#' @param species_cover Species-cover data frame.
#' @param plot_metadata Plot metadata used as the parent table.
#' @param species_df Species lookup data frame, if available.
#' @return Cross-file validation report.
#' @keywords internal
validate_species_cover <- function(species_cover, plot_metadata, species_df) {
  results <- list()
  idx <- 0L

  idx <- idx + 1L
  results[[idx]] <- check_unique_keys(species_cover, c("plotCode", "species"), "speciesCover.csv")

  idx <- idx + 1L
  results[[idx]] <- check_fk_exists(
    child = species_cover,
    parent = plot_metadata,
    by = c("plotCode", "focalOrBenchmark", "landCover", "vegClass"),
    child_name = "speciesCover.csv",
    parent_name = "plotMetadata.csv"
  )

  idx <- idx + 1L
  results[[idx]] <- check_merge_cardinality(
    left = species_cover,
    right = plot_metadata,
    by = c("plotCode", "focalOrBenchmark", "landCover", "vegClass"),
    left_name = "speciesCover.csv",
    right_name = "plotMetadata.csv"
  )

  idx <- idx + 1L
  results[[idx]] <- check_species_lookup_membership(
    species_cover,
    species_df,
    "speciesCover.csv"
  )

  bind_validation_reports(results)
}

#' Validate growth-form cover against plot metadata
#'
#' Growth-form cover is later combined with plot classifications and may also
#' serve as the total against which exotic cover is compared. Its uniqueness
#' and plot-key checks protect both uses by ensuring that a merge adds context
#' without multiplying rows or admitting observations for unknown plots.
#'
#' @param cover_by_growth_form Growth-form cover data frame.
#' @param plot_metadata Plot metadata used as the parent table.
#' @return Cross-file validation report.
#' @keywords internal
validate_cover_by_growth_form <- function(cover_by_growth_form, plot_metadata) {
  results <- list()
  idx <- 0L

  idx <- idx + 1L
  results[[idx]] <- check_unique_keys(
    cover_by_growth_form,
    c("plotCode", "stratum"),
    "coverByGrowthForm.csv"
  )

  idx <- idx + 1L
  results[[idx]] <- check_fk_exists(
    child = cover_by_growth_form,
    parent = plot_metadata,
    by = c("plotCode", "focalOrBenchmark", "landCover", "vegClass"),
    child_name = "coverByGrowthForm.csv",
    parent_name = "plotMetadata.csv"
  )

  idx <- idx + 1L
  results[[idx]] <- check_merge_cardinality(
    left = cover_by_growth_form,
    right = plot_metadata,
    by = c("plotCode", "focalOrBenchmark", "landCover", "vegClass"),
    left_name = "coverByGrowthForm.csv",
    right_name = "plotMetadata.csv"
  )

  bind_validation_reports(results)
}

#' Validate exotic cover and its relationship to total cover
#'
#' Exotic cover shares plot and stratum identity with growth-form cover. The
#' common plot checks protect joins to metadata, while the subset comparison
#' catches impossible totals without making the derived comparison a hard
#' failure for projects whose aggregation conventions differ.
#'
#' @param exotic_cover_by_stratum Exotic-cover data frame.
#' @param plot_metadata Plot metadata used as the parent table.
#' @param inputs All loaded inputs, including growth-form cover when present.
#' @return Cross-file validation report.
#' @keywords internal
validate_exotic_cover_by_stratum <- function(exotic_cover_by_stratum, plot_metadata, inputs) {
  results <- list()
  idx <- 0L

  idx <- idx + 1L
  results[[idx]] <- check_unique_keys(
    exotic_cover_by_stratum,
    c("plotCode", "stratum"),
    "exoticCoverByStratum.csv"
  )

  idx <- idx + 1L
  results[[idx]] <- check_fk_exists(
    child = exotic_cover_by_stratum,
    parent = plot_metadata,
    by = c("plotCode", "focalOrBenchmark", "landCover", "vegClass"),
    child_name = "exoticCoverByStratum.csv",
    parent_name = "plotMetadata.csv"
  )

  idx <- idx + 1L
  results[[idx]] <- check_merge_cardinality(
    left = exotic_cover_by_stratum,
    right = plot_metadata,
    by = c("plotCode", "focalOrBenchmark", "landCover", "vegClass"),
    left_name = "exoticCoverByStratum.csv",
    right_name = "plotMetadata.csv"
  )

  idx <- idx + 1L
  results[[idx]] <- check_exotic_subset(
    exotic_cover_by_stratum,
    load_input(inputs, "coverByGrowthForm.csv"),
    "exoticCoverByStratum.csv"
  )

  bind_validation_reports(results)
}

#' Validate ground-cover observations against plot metadata
#'
#' Ground-cover records are keyed at plot and stratum level and inherit plot
#' classifications from `plotMetadata.csv`. Enforcing that grain and foreign
#' key relationship keeps downstream indicator merges one-to-one and prevents
#' unrecognised plot observations from being silently dropped.
#'
#' @param ground_cover Ground-cover data frame.
#' @param plot_metadata Plot metadata used as the parent table.
#' @return Cross-file validation report.
#' @keywords internal
validate_ground_cover <- function(ground_cover, plot_metadata) {
  results <- list()
  idx <- 0L

  idx <- idx + 1L
  results[[idx]] <- check_unique_keys(
    ground_cover,
    c("plotCode", "stratum"),
    "groundCover.csv"
  )

  idx <- idx + 1L
  results[[idx]] <- check_fk_exists(
    child = ground_cover,
    parent = plot_metadata,
    by = c("plotCode", "focalOrBenchmark", "landCover", "vegClass"),
    child_name = "groundCover.csv",
    parent_name = "plotMetadata.csv"
  )

  idx <- idx + 1L
  results[[idx]] <- check_merge_cardinality(
    left = ground_cover,
    right = plot_metadata,
    by = c("plotCode", "focalOrBenchmark", "landCover", "vegClass"),
    left_name = "groundCover.csv",
    right_name = "plotMetadata.csv"
  )

  bind_validation_reports(results)
}

#' Run relational validation for one assessment
#'
#' This is the second validation stage: schema validation has already checked
#' row-level structure, so this function loads the assessment once and checks
#' relationships that JSON Schema cannot express, such as foreign keys,
#' merge cardinality, indicator dependencies, and derived-table consistency.
#' Optional inputs are validated when present; missing optional inputs do not
#' create failures or prevent unrelated checks from running.
#'
#' @param data_root Root data directory. Kept in the interface for the
#'   assessment-level runner and future path-aware checks.
#' @param assessment_path Assessment directory name used in the report.
#' @param active_indicators Indicators selected for the assessment.
#' @param file_map Named input-file map for the assessment.
#' @param project_params Project parameters used by dependency validation.
#' @return Cross-file validation report with an assessment column.
#' @keywords internal
run_crossfile_validation <- function(
  data_root,
  assessment_path,
  active_indicators,
  file_map,
  project_params
) {
  inputs <- load_assessment_inputs(file_map)
  results <- list()
  idx <- 0L

  idx <- idx + 1L
  results[[idx]] <- validate_indicator_dependencies(
    active_indicators = active_indicators,
    file_map = file_map,
    project_params = project_params
  )

  plot_metadata <- load_input(inputs, "plotMetadata.csv")
  species_df <- load_input(inputs, "species.csv")
  land_cover <- load_input(inputs, "landCover.csv")
  vegetation <- load_input(inputs, "vegetation.csv")

  if (!is.null(plot_metadata)) {
    idx <- idx + 1L
    results[[idx]] <- validate_plot_metadata(plot_metadata)
  }

  if (!is.null(species_df)) {
    idx <- idx + 1L
    results[[idx]] <- validate_species_lookup(species_df)
  }

  if (!is.null(land_cover) && !is.null(plot_metadata)) {
    idx <- idx + 1L
    results[[idx]] <- validate_land_cover(land_cover, plot_metadata)
  }

  if (!is.null(vegetation)) {
    idx <- idx + 1L
    results[[idx]] <- validate_vegetation_lookup(
      vegetation = vegetation,
      plot_metadata = plot_metadata,
      land_cover = land_cover
    )
  }

  species_cover <- load_input(inputs, "speciesCover.csv")
  if (!is.null(species_cover) && !is.null(plot_metadata)) {
    idx <- idx + 1L
    results[[idx]] <- validate_species_cover(species_cover, plot_metadata, species_df)
  }

  cover_by_growth_form <- load_input(inputs, "coverByGrowthForm.csv")
  if (!is.null(cover_by_growth_form) && !is.null(plot_metadata)) {
    idx <- idx + 1L
    results[[idx]] <- validate_cover_by_growth_form(cover_by_growth_form, plot_metadata)
  }

  exotic_cover_by_stratum <- load_input(inputs, "exoticCoverByStratum.csv")
  if (!is.null(exotic_cover_by_stratum) && !is.null(plot_metadata)) {
    idx <- idx + 1L
    results[[idx]] <- validate_exotic_cover_by_stratum(
      exotic_cover_by_stratum,
      plot_metadata,
      inputs
    )
  }

  ground_cover <- load_input(inputs, "groundCover.csv")
  if (!is.null(ground_cover) && !is.null(plot_metadata)) {
    idx <- idx + 1L
    results[[idx]] <- validate_ground_cover(ground_cover, plot_metadata)
  }

  species_stems <- load_input(inputs, "speciesStems.csv")
  if (!is.null(species_stems) && !is.null(plot_metadata)) {
    idx <- idx + 1L
    results[[idx]] <- validate_species_stems(species_stems, plot_metadata, species_df)
  }

  report <- bind_validation_reports(results)
  if (nrow(report) == 0) {
    return(report)
  }

  report$assessment <- assessment_path
  report$extra_columns <- NA_character_
  report
}

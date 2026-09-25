# Stage 1 schema validation helpers.

#' Read a JSON Schema as a reference-preserving R object
#'
#' The validator expands local modular references itself before calling
#' `jsonvalidate`; disabling vector simplification preserves the object/array
#' distinction needed by the JSON Pointer resolver.
#'
#' @param path Schema JSON path.
#' @return Nested R list representing the schema.
#' @keywords internal
read_schema_file <- function(path) {
  # Read the schema as nested lists so we can resolve the modular $ref tree
  # in memory before handing a self-contained schema to jsonvalidate.
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

#' Resolve a local JSON Pointer in a schema object
#'
#' The schema files use a small, local `$ref` vocabulary. Resolving it in R
#' avoids relying on filesystem-relative reference behavior inside the AJV
#' call and lets the validator submit one self-contained schema.
#'
#' @param x Nested JSON-like object.
#' @param pointer JSON Pointer such as `#/\$defs/speciesRow`.
#' @return The referenced schema fragment.
#' @keywords internal
json_pointer_get <- function(x, pointer) {
  # Minimal JSON Pointer resolver for the small set of local #/$defs refs used
  # by the schema files.

  # x: A JSON-like R object (nested lists and vectors) to resolve the pointer against.
  # pointer: A JSON Pointer string (e.g. "#/definitions/foo") indicating the
  #   location of the target value within x.

  if (is.null(pointer) || !nzchar(pointer) || pointer %in% c("#", "/")) {
    return(x)
  }

  # Remove the leading "#" or "/" if present, since we'll split on "/" next
  pointer <- sub("^#", "", pointer)
  pointer <- sub("^/", "", pointer)
  # An empty pointer (after removing leading characters) refers to the whole document.
  if (!nzchar(pointer)) {
    return(x)
  }

  # Split the pointer into parts
  parts <- strsplit(pointer, "/", fixed = TRUE)[[1]]
  current <- x

  #
  for (part in parts) {
    part <- gsub("~1", "/", part, fixed = TRUE)
    part <- gsub("~0", "~", part, fixed = TRUE)

    if (is.list(current) && !is.null(names(current)) && part %in% names(current)) {
      current <- current[[part]]
      next
    }

    if (is.list(current) && is.null(names(current))) {
      index <- suppressWarnings(as.integer(part))
      if (!is.na(index) && index >= 1 && index <= length(current)) {
        current <- current[[index]]
        next
      }
    }

    stop(sprintf("Cannot resolve JSON pointer segment '%s' in '%s'.", part, pointer), call. = FALSE)
  }

  current
}

#' Build a cached resolver for the modular schema set
#'
#' A resolver indexes only schema files belonging to this contract, recursively
#' expands their local references, and caches both raw and expanded forms. The
#' cache matters because every planned CSV is validated against related shared
#' definitions during a single assessment run.
#'
#' @param schema_root Root `schema/` directory.
#' @return List containing the indexed paths and `resolve_schema()` closure.
#' @keywords internal
build_schema_resolver <- function(schema_root) {
  # Index the schema files that belong to this contract. The current
  # schema set only uses these files, so a fixed index is simpler and safer
  # than trying to infer arbitrary file references.
  schema_files <- c(
    file.path(schema_root, "base", "common.defs.schema.json"),
    file.path(schema_root, "base", "row-patterns.schema.json"),
    file.path(schema_root, "inputs", "coverByGrowthForm.schema.json"),
    file.path(schema_root, "inputs", "coverByStratum.schema.json"),
    file.path(schema_root, "inputs", "exoticCoverByStratum.schema.json"),
    file.path(schema_root, "inputs", "groundCover.schema.json"),
    file.path(schema_root, "inputs", "landCover.schema.json"),
    file.path(schema_root, "inputs", "plotMetadata.schema.json"),
    file.path(schema_root, "inputs", "species.schema.json"),
    file.path(schema_root, "inputs", "speciesCover.schema.json"),
    file.path(schema_root, "inputs", "speciesStems.schema.json"),
    file.path(schema_root, "inputs", "vegetation.schema.json")
  )
  schema_index <- setNames(schema_files, basename(schema_files))

  # Store parsed schema files and fully expanded schemas by file path so we
  # only read and resolve each schema once.
  schema_store_cache <- new.env(parent = emptyenv())
  expanded_schema_cache <- new.env(parent = emptyenv())

  load_schema <- function(path) {
    # Read and memoize a schema file the first time it is requested.
    if (!exists(path, envir = schema_store_cache, inherits = FALSE)) {
      assign(path, read_schema_file(path), envir = schema_store_cache)
    }
    get(path, envir = schema_store_cache, inherits = FALSE)
  }

  is_ref_node <- function(node) {
    # A local schema reference is always represented as a single-field object
    # whose only key is "$ref".
    is.list(node) &&
      !is.null(names(node)) &&
      length(node) == 1 &&
      identical(names(node), "$ref")
  }

  resolve_ref_node <- function(ref_node, current_path) {
    # Resolve a "$ref" object into the schema content it points at.
    ref <- ref_node[["$ref"]]
    parts <- strsplit(ref, "#", fixed = TRUE)[[1]]
    ref_path <- parts[[1]]
    ref_pointer <- if (length(parts) > 1) paste(parts[-1], collapse = "#") else ""

    if (!nzchar(ref_path)) {
      # A bare "#/..." ref points inside the current schema file.
      target_path <- current_path
    } else {
      # Modular schemas only reference the shared base files by name.
      target_file <- basename(ref_path)
      target_path <- schema_index[target_file]
      if (!length(target_path) || is.na(target_path)) {
        stop(
          sprintf(
            "Unknown schema reference target '%s' (basename '%s').",
            ref_path,
            target_file
          ),
          call. = FALSE
        )
      }
      target_path <- unname(target_path)
    }

    target_schema <- load_schema(target_path)
    resolve_node(json_pointer_get(target_schema, ref_pointer), target_path)
  }

  resolve_children <- function(node, current_path) {
    # Recurse through every child value in a JSON object or array.
    lapply(node, resolve_node, current_path = current_path)
  }

  resolve_node <- function(node, current_path) {
    # Expand the schema tree in place:
    # - leaf values are returned unchanged,
    # - "$ref" objects are replaced with the referenced schema fragment,
    # - arrays and objects are traversed recursively.
    if (is.atomic(node) || is.null(node)) {
      return(node)
    }

    if (is_ref_node(node)) {
      return(resolve_ref_node(node, current_path))
    }

    if (is.list(node)) {
      return(resolve_children(node, current_path))
    }

    node
  }

  resolve_schema <- function(path) {
    # Cache the fully expanded schema as well so each file is resolved once.
    if (exists(path, envir = expanded_schema_cache, inherits = FALSE)) {
      return(get(path, envir = expanded_schema_cache, inherits = FALSE))
    }

    schema <- load_schema(path)
    resolved <- resolve_node(schema, path)
    assign(path, resolved, envir = expanded_schema_cache)
    resolved
  }

  list(
    schema_files = schema_files,
    resolve_schema = resolve_schema
  )
}

#' Collect declared object properties from a resolved schema
#'
#' CSV columns outside the schema are not automatically failures in this
#' project, but reporting them exposes fields that the standardized contract
#' does not describe. This walker gathers those property names after reference
#' expansion so the comparison is meaningful.
#'
#' @param node Resolved schema fragment.
#' @return Character vector of declared property names.
#' @keywords internal
collect_schema_properties <- function(node) {
  # Walk the resolved schema tree and collect any object property names.
  # This lets us detect CSV columns that are present in the data but ignored
  # by downstream analysis.
  props <- character()

  # If the node is NULL or atomic, return empty properties.
  if (is.null(node) || is.atomic(node)) {
    return(props)
  }

  # If the node is not a list, return empty properties - defensive check for unexpected types.
  if (!is.list(node)) {
    return(props)
  }

  # If the node has a "properties" field, collect its names.
  if (!is.null(names(node)) && "properties" %in% names(node)) {
    props <- union(props, names(node$properties))
  }


  child_names <- names(node)
  if (is.null(child_names)) {
    for (child in node) {
      props <- union(props, collect_schema_properties(child))
    }
    return(props)
  }

  # If the node has named children, iterate over them and collect properties recursively.
  # If the schema changes, this might need updating.
  skip_fields <- c(
    "properties",
    "required",
    "additionalProperties",
    "type",
    "title",
    "description",
    "$schema",
    "$id"
  )

  for (nm in child_names) {
    if (nm %in% skip_fields) {
      next
    }
    props <- union(props, collect_schema_properties(node[[nm]]))
  }

  props
}

#' Read a CSV and serialize its rows for JSON Schema validation
#'
#' `jsonvalidate` validates JSON rather than data frames. Keeping the data frame
#' alongside the serialized rows allows later diagnostics to inspect columns
#' while preserving CSV missing values as JSON `null`.
#'
#' @param file_path CSV path.
#' @return List containing the loaded data frame and JSON row text.
#' @keywords internal
load_csv_as_json_rows <- function(file_path) {
  data <- read_csv_table(file_path)

  json_text <- jsonlite::toJSON(
    data,
    dataframe = "rows",
    auto_unbox = TRUE,
    na = "null",
    pretty = FALSE
  )

  list(
    data = data,
    json_text = json_text
  )
}

#' Map a standardized input filename to its JSON Schema
#'
#' Explicit mapping prevents a typo or unsupported file from being silently
#' accepted. It also keeps schema filenames independent from the assessment's
#' physical input directory.
#'
#' @param file_name Standardized CSV filename.
#' @param schema_root Root `schema/` directory.
#' @return Path to the matching schema file.
#' @keywords internal
resolve_schema_for_file <- function(file_name, schema_root) {
  schema_inputs_root <- file.path(schema_root, "inputs")
  schema_paths <- c(
    "coverByGrowthForm.csv" = file.path(schema_inputs_root, "coverByGrowthForm.schema.json"),
    "coverByStratum.csv" = file.path(schema_inputs_root, "coverByStratum.schema.json"),
    "exoticCoverByStratum.csv" = file.path(schema_inputs_root, "exoticCoverByStratum.schema.json"),
    "groundCover.csv" = file.path(schema_inputs_root, "groundCover.schema.json"),
    "landCover.csv" = file.path(schema_inputs_root, "landCover.schema.json"),
    "plotMetadata.csv" = file.path(schema_inputs_root, "plotMetadata.schema.json"),
    "species.csv" = file.path(schema_inputs_root, "species.schema.json"),
    "speciesCover.csv" = file.path(schema_inputs_root, "speciesCover.schema.json"),
    "speciesStems.csv" = file.path(schema_inputs_root, "speciesStems.schema.json"),
    "vegetation.csv" = file.path(schema_inputs_root, "vegetation.schema.json")
  )

  schema_path <- schema_paths[[file_name]]
  if (is.null(schema_path) || !nzchar(schema_path)) {
    stop(sprintf("No schema is defined for file '%s'.", file_name), call. = FALSE)
  }

  schema_path
}

#' Validate one CSV against its resolved JSON Schema
#'
#' Missing files are returned as report failures rather than raised as errors,
#' allowing the assessment summary to identify the exact missing contract file.
#' Existing files are converted to JSON and validated with AJV, while unknown
#' columns are retained as diagnostic metadata rather than rejected outright.
#'
#' @param file_name Contract filename.
#' @param file_path Physical CSV path.
#' @param schema_path Schema path.
#' @param schema_resolver Resolver from [build_schema_resolver()].
#' @return List describing the schema result.
#' @keywords internal
validate_schema_file <- function(file_name, file_path, schema_path, schema_resolver) {
  if (!file.exists(file_path)) {
    return(list(
      stage = "schema",
      check_id = file_name,
      status = "fail",
      severity = "error",
      message = sprintf("Missing input file: %s", file_path),
      extra_columns = character(),
      file = file_name
    ))
  }

  loaded <- load_csv_as_json_rows(file_path)
  schema_obj <- schema_resolver$resolve_schema(schema_path)
  schema_props <- collect_schema_properties(schema_obj)
  extra_columns <- setdiff(names(loaded$data), schema_props)

  result <- tryCatch(
    {
      # Use the ajv engine because this schema set includes modular refs.
      jsonvalidate::json_validate(
        loaded$json_text,
        jsonlite::toJSON(schema_obj, auto_unbox = TRUE, pretty = FALSE),
        engine = "ajv",
        strict = TRUE,
        error = TRUE,
        verbose = TRUE
      )
      list(
        stage = "schema",
        check_id = file_name,
        status = "pass",
        severity = "error",
        message = "ok",
        extra_columns = extra_columns,
        file = file_name
      )
    },
    error = function(e) {
      list(
        stage = "schema",
        check_id = file_name,
        status = "fail",
        severity = "error",
        message = conditionMessage(e),
        extra_columns = extra_columns,
        file = file_name
      )
    }
  )
  result
}

#' Convert an internal schema result to the common report format
#'
#' Keeping this conversion separate means schema-specific details such as
#' extra columns do not leak into the generic validation result constructors.
#'
#' @param assessment Assessment name.
#' @param file_name Contract filename.
#' @param result Result list from [validate_schema_file()].
#' @return One-row schema-stage report data frame.
#' @keywords internal
schema_validate_report <- function(assessment, file_name, result) {
  validation_report_row(
    assessment = assessment,
    stage = result$stage,
    check_id = result$check_id,
    severity = result$severity,
    status = result$status,
    message = result$message,
    file = file_name,
    details = if (!is.null(result$details)) result$details else NA_character_,
    extra_columns = if (length(result$extra_columns) > 0) {
      paste(result$extra_columns, collapse = ", ")
    } else {
      NA_character_
    }
  )
}

#' Run schema validation for an assessment's planned files
#'
#' The file plan is assembled before this stage so required, alternative, and
#' present-optional inputs are treated consistently. This stage intentionally
#' stops before cross-file checks when any schema result fails, preventing
#' relational diagnostics from being based on structurally invalid tables.
#'
#' @param assessment Assessment name.
#' @param inputs_dir Assessment `inputs/` directory.
#' @param input_files Planned contract filenames.
#' @param schema_root Root `schema/` directory.
#' @return Schema-stage validation report.
#' @keywords internal
run_schema_validation <- function(assessment, inputs_dir, input_files, schema_root) {
  schema_resolver <- build_schema_resolver(schema_root)
  report <- list()
  idx <- 0L

  for (file_name in input_files) {
    idx <- idx + 1L
    file_path <- file.path(inputs_dir, file_name)
    schema_path <- resolve_schema_for_file(file_name, schema_root)
    result <- validate_schema_file(file_name, file_path, schema_path, schema_resolver)
    report[[idx]] <- schema_validate_report(assessment, file_name, result)
  }

  bind_validation_reports(report)
}

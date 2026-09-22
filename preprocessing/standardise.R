# Preprocessing step: shape loaded data into analysis-ready standard tables.

standardise_preprocessed_data <- function(context) {
  # Keep the standardisation stage separate from pruning and file export.
  context <- standardise_plot_metadata(context)
  context <- standardise_species_tables(context)
  context <- standardise_cover_tables(context)
  context
}

standardise_plot_metadata <- function(context) {
  metadata <- context_input(context, "plotMetadata.csv")
  if (is.null(metadata)) stop("plotMetadata.csv is required for preprocessing.", call. = FALSE)
  required <- c("plotCode", "focalOrBenchmark", "vegClass", "landCover")
  missing <- setdiff(required, names(metadata))
  if (length(missing)) {
    stop(sprintf("plotMetadata.csv is missing: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
  context$inputs[["plotMetadata.csv"]] <- unique(
    metadata[, unique(c(required, names(metadata))), drop = FALSE]
  )
  context
}

standardise_species_tables <- function(context) {
  species <- context_input(context, "species.csv")
  source_tables <- Filter(Negate(is.null), list(
    context_input(context, "speciesCover.csv"), context_input(context, "speciesStems.csv")
  ))
  if (is.null(species)) {
    species_names <- unique(unlist(lapply(source_tables, function(x) x$species), use.names = FALSE))
    species_names <- sort(as.character(species_names[!is.na(species_names)]))
    species <- data.frame(
      genus = sub(" .*$", "", species_names), species = species_names,
      is_exotic = 0, stringsAsFactors = FALSE
    )
  }
  if (!"species" %in% names(species)) stop("species.csv must contain species.", call. = FALSE)
  if (!"genus" %in% names(species)) species$genus <- sub(" .*$", "", as.character(species$species))
  if (!"is_exotic" %in% names(species)) species$is_exotic <- 0
  species$is_exotic <- as.integer(as.character(species$is_exotic) %in% c("1", "TRUE", "T", "true"))
  first <- c("speciesID", "genus", "species", "is_exotic", "growthForm")
  context$inputs[["species.csv"]] <- unique(
    species[, c(
      intersect(first, names(species)),
      setdiff(names(species), first)
    ),
    drop = FALSE
    ]
  )
  context
}

standardise_cover_tables <- function(context) {
  cover_files <- intersect(
    c("coverByGrowthForm.csv", "coverByStratum.csv", "exoticCoverByStratum.csv", "groundCover.csv"),
    names(context$inputs)
  )
  for (file_name in cover_files) {
    table <- context_input(context, file_name)
    if (!"cover" %in% names(table)) {
      stop(sprintf("%s must contain cover.", file_name), call. = FALSE)
    }
    values <- suppressWarnings(as.numeric(as.character(table$cover)))
    finite_values <- values[is.finite(values)]
    if (length(finite_values) && (any(finite_values < 0) || any(finite_values > 1))) {
      stop(sprintf(
        "%s must contain cover proportions in [0, 1]; convert percentages before preprocessing.",
        file_name
      ), call. = FALSE)
    }
    table$cover <- values
    context$inputs[[file_name]] <- table
  }
  if (
    !has_input(context$inputs, "coverByStratum.csv") &&
      has_input(context$inputs, "coverByGrowthForm.csv")
  ) {
    context$inputs[["coverByStratum.csv"]] <- context$inputs[["coverByGrowthForm.csv"]]
  }
  if (has_input(context$inputs, "speciesStems.csv")) {
    stems <- context_input(context, "speciesStems.csv")
    if (!"dbh_cm" %in% names(stems)) stop("speciesStems.csv must contain dbh_cm.", call. = FALSE)
    stems$ba_m2 <- pi * (suppressWarnings(as.numeric(as.character(stems$dbh_cm))) / 2)^2 / 10000
    stems <- stems[, c(setdiff(names(stems), "ba_m2"), "ba_m2"), drop = FALSE]
    if ("dbh_cm" %in% names(stems)) {
      stem_names <- setdiff(names(stems), "ba_m2")
      stems <- stems[, append(
        stem_names,
        "ba_m2",
        after = match("dbh_cm", stem_names) - 1L
      ),
      drop = FALSE
      ]
    }
    context$inputs[["speciesStems.csv"]] <- stems
  }
  context
}

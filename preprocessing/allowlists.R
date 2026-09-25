# Preprocessing step: generate indicator include / blocklist tables.

build_allowlists <- function(context) {
  context$allowlists <- list(
    blocklist = build_indicator_blocklist(context),
    allowlist = build_indicator_allowlist(context)
  )
  context
}

indicator_function_groups <- c(
  SR = "composition", TD = "composition", TS = "composition",
  PCGF = "structure", PCS = "structure", ASC = "structure",
  GC = "function", BA = "structure", CH = "structure",
  PCES = "integrity", PCESS = "integrity", BAES = "integrity"
)

indicator_has_stratum <- c(
  SR = FALSE, TD = FALSE, TS = FALSE, PCES = FALSE, PCESS = TRUE,
  PCGF = TRUE, PCS = TRUE, GC = TRUE, BA = FALSE, ASC = TRUE,
  CH = FALSE, BAES = FALSE
)

preprocessing_indicators <- function(context) {
  configured <- context$params$active_indicators
  if (!is.null(configured)) {
    return(unique(as.character(unlist(configured, use.names = FALSE))))
  }

  file_to_indicator <- c(
    coverByGrowthForm.csv = "PCGF", coverByStratum.csv = "PCS",
    groundCover.csv = "GC", exoticCoverByStratum.csv = "PCESS",
    speciesCover.csv = "SR", speciesStems.csv = "BA"
  )
  unique(unname(file_to_indicator[names(context$inputs)]))
}

indicator_input_name <- function(indicator, context) {
  candidates <- switch(indicator,
    PCGF = "coverByGrowthForm.csv",
    PCS = "coverByStratum.csv",
    GC = "groundCover.csv",
    PCES = "exoticCoverByStratum.csv",
    PCESS = "exoticCoverByStratum.csv",
    BA = "speciesStems.csv",
    BAES = "speciesStems.csv",
    ASC = "speciesStems.csv",
    CH = "speciesStems.csv",
    SR = c("speciesCover.csv", "speciesStems.csv"),
    TD = c("speciesCover.csv", "speciesStems.csv"),
    TS = c("speciesCover.csv", "speciesStems.csv"),
    character()
  )
  present <- candidates[candidates %in% names(context$inputs)]
  if (length(present)) present[[1]] else NA_character_
}

build_indicator_blocklist <- function(context) {
  indicators <- preprocessing_indicators(context)
  rows <- lapply(indicators, function(indicator) {
    input_name <- indicator_input_name(indicator, context)
    table <- if (!is.na(input_name)) context_input(context, input_name) else NULL
    strata <- if (isTRUE(indicator_has_stratum[indicator]) &&
      !is.null(table) && "stratum" %in% names(table)) {
      unique(as.character(table$stratum))
    } else {
      "nostrata"
    }
    data.frame(
      EI = indicator,
      stratum = strata,
      f.group = unname(indicator_function_groups[indicator]),
      include = "T",
      edit.notes = "",
      stringsAsFactors = FALSE
    )
  })
  if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(
      EI = character(), stratum = character(), f.group = character(),
      include = character(), edit.notes = character(), stringsAsFactors = FALSE
    )
  }
}

build_indicator_allowlist <- function(context) {
  indicators <- preprocessing_indicators(context)
  rows <- lapply(indicators, function(indicator) {
    input_name <- indicator_input_name(indicator, context)
    table <- if (!is.na(input_name)) context_input(context, input_name) else NULL
    if (is.null(table) || !all(c("vegClass") %in% names(table))) {
      return(data.frame(
        EI = indicator, stratum = "nostrata", bm.vegetation = character(),
        include = character(), edit.notes = character(), stringsAsFactors = FALSE
      ))
    }
    if (isTRUE(indicator_has_stratum[indicator]) &&
      "stratum" %in% names(table)) {
      keys <- unique(table[, c("stratum", "vegClass"), drop = FALSE])
      names(keys) <- c("stratum", "bm.vegetation")
    } else {
      keys <- data.frame(
        stratum = "nostrata", bm.vegetation = unique(table$vegClass),
        stringsAsFactors = FALSE
      )
    }
    data.frame(EI = indicator, keys, include = "", edit.notes = "", stringsAsFactors = FALSE)
  })
  if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(
      EI = character(), stratum = character(), bm.vegetation = character(),
      include = character(), edit.notes = character(), stringsAsFactors = FALSE
    )
  }
}

# Backwards-compatible names used by the implementation plan and older callers.
build_indicator_blocklist <- build_indicator_blocklist
build_indicator_allowlist <- build_indicator_allowlist

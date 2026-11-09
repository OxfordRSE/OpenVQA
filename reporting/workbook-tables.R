rename_workbook_headers <- function(table) {
  headers <- c(
    "landcover" = "Land Cover Class",
    "bm.veg" = "Benchmark Vegetation",
    "indicator.group" = "Indicator Group",
    "indicator" = "Indicator",
    "f.group" = "Functional Group",
    "dist" = "Distribution",
    "test.tail" = "Test Tail",
    "q.method" = "Quality Method",
    "f.n" = "Focal N",
    "f.mean" = "Focal Mean",
    "f.sd" = "Focal SD",
    "b.n" = "Benchmark N",
    "b.mean" = "Benchmark Mean",
    "b.sd" = "Benchmark SD",
    "q" = "Q",
    "q.lcl" = "Q.lcl",
    "q.ucl" = "Q.ucl",
    "mdes" = "MDES",
    "q.full" = "Q [Lower, Upper 95% CLs]",
    "area_ha" = "Area (ha)",
    "qh" = "QH",
    "qh.lcl" = "QH.lcl",
    "qh.ucl" = "QH.ucl",
    "qh.full" = "QH [Lower, Upper 95% CLs]",
    "ind.name" = "Indicator",
    "ind.group" = "Indicator Group",
    "distn" = "Distribution",
    "bm.val" = "Fixed Benchmark Value",
    "landCover" = "Land Cover Class",
    "type" = "Land Cover Type",
    "vegClass" = "Benchmark Vegetation",
    "vegName" = "Vegetation Name",
    "lc.type" = "Structural Class",
    "focal_plots" = "Focal Plots (N.f)",
    "bm_plots" = "Benchmark Plots (N.b)",
    "all.n.OK" = "Run VQA?"
  )
  selected <- names(table) %in% names(headers)
  names(table)[selected] <- unname(headers[names(table)[selected]])
  table
}

build_workbook_tables <- function(summary, inputs, config, analysis_time) {
  # Input files
  file.qh.bm <- "summary_focal_qh_bm.csv"
  file.q.overall <- "summary_focal_q_qh.csv"
  file.q.fg <- "summary_focal_fg.csv"
  file.q.ei <- "summary_focal_ei.csv"

  ##########################################
  # Begin
  ##########################################

  cat("******************************************\n")
  cat("Exporting VQA results to Excel\n\n")
  cat("Preparing workbook summary tables:\n")

  ##########################################
  # Overall quality results
  ##########################################

  cat("  ", basename(file.q.overall), "...", sep = "")
  q.overall <- summary[[file.q.overall]]

  if (config$omit_quality_hectares == TRUE) {
    # Drop area and quality hectares columns
    drop.cols <- c("area_ha", "qh", "qh.lcl", "qh.ucl")
    q.overall <- q.overall[, !names(q.overall) %in% c(drop.cols)]
  }

  if (config$workbook_drop_notes == TRUE) {
    q.overall <- q.overall[, !names(q.overall) == "notes"]
  }

  if (config$workbook_remove_missing_quality == TRUE) {
    q.overall <- q.overall[!is.na(q.overall$q), ]
  }

  # Add MDES column
  q.overall$mdes <- NA
  for (i in 1:nrow(q.overall)) {
    q.overall$mdes[i] <- max(
      q.overall$q[i] - q.overall$q.lcl[i],
      q.overall$q.ucl[i] - q.overall$q[i]
    )
  }

  # Set precision of quality and MDES
  q.overall <- format_table_columns(q.overall, c(q = 2, q.lcl = 2, q.ucl = 2, mdes = 2))

  # Set precision for area and quality hectares if applicable
  if (config$omit_quality_hectares == FALSE) {
    q.overall <- format_table_columns(q.overall, c(area_ha = 1, qh = 1, qh.lcl = 1, qh.ucl = 1))
  }

  cat("done\n")

  ##########################################
  # Benchmark vegetation quality results
  ##########################################

  if (config$omit_quality_hectares == FALSE) {
    cat("  ", basename(file.qh.bm), "...", sep = "")
    qh.bm <- summary[[file.qh.bm]]

    if (config$workbook_drop_notes == TRUE) {
      qh.bm <- qh.bm[, !names(qh.bm) == "notes"]
    }

    # Set precision for area and quality hectares if applicable
    qh.bm <- format_table_columns(qh.bm, c(area_ha = 1, qh = 1, qh.lcl = 1, qh.ucl = 1))

    cat("done\n")
  }

  ##########################################
  # Fuctional group quality results
  ##########################################

  cat("  ", basename(file.q.fg), "...", sep = "")
  q.fg <- summary[[file.q.fg]]

  # rename q.fg
  names(q.fg)[names(q.fg) == "q.fg"] <- "q"

  # Add MDES column
  q.fg$mdes <- NA
  for (i in 1:nrow(q.fg)) {
    q.fg$mdes[i] <- max(
      q.fg$q[i] - q.fg$q.lcl[i],
      q.fg$q.ucl[i] - q.fg$q[i]
    )
  }

  # Set precision of quality and MDES
  q.fg <- format_table_columns(q.fg, c(q = 2, q.lcl = 2, q.ucl = 2, mdes = 2))

  cat("done\n")

  ##########################################
  # Indicator quality results
  ##########################################

  cat("  ", basename(file.q.ei), "...", sep = "")
  q.ei <- summary[[file.q.ei]]

  # Drop unwanted columns
  drop.cols <- c("lc.ig", "lc.fg")
  q.ei <- q.ei[, !(names(q.ei) %in% drop.cols)]

  # Drop excluded indicator + column "include", if requested
  if (config$workbook_drop_excluded == TRUE) {
    q.ei <- q.ei[!q.ei$include == FALSE, ]
    q.ei <- q.ei[, !names(q.ei) %in% c("include")]
  }

  # Add ei.prefix & indicator.group (mandatory)
  q.ei$ei.prefix <- ""
  for (i in 1:nrow(q.ei)) {
    if (q.ei$EI[i] == "SR") {
      q.ei$indicator.group[i] <- "Species Richness"
      q.ei$ei.prefix[i] <- "Species Richness"
    } else if (q.ei$EI[i] == "TD") {
      q.ei$indicator.group[i] <- "Taxonomic Distance"
      q.ei$ei.prefix[i] <- "Taxonomic Distance"
    } else if (q.ei$EI[i] == "PCESS") {
      q.ei$indicator.group[i] <- "Percent Cover Exotic Species"
      q.ei$ei.prefix[i] <- "Percent Cover Exotic"
    } else if (q.ei$EI[i] == "PCES") {
      q.ei$indicator.group[i] <- "Percent Cover Exotic Species"
      q.ei$ei.prefix[i] <- "Percent Cover Exotic"
    } else if (q.ei$EI[i] == "PCGF") {
      q.ei$indicator.group[i] <- "Percent Cover by Growth Form"
      q.ei$ei.prefix[i] <- "Percent Cover"
    } else if (q.ei$EI[i] == "PCS") {
      q.ei$indicator.group[i] <- "Percent Cover by Stratum"
      q.ei$ei.prefix[i] <- "Percent Cover"
    } else if (q.ei$EI[i] == "GC") {
      q.ei$indicator.group[i] <- "Ground Cover"
      q.ei$ei.prefix[i] <- "Percent Cover"
    } else if (q.ei$EI[i] == "BA") {
      q.ei$indicator.group[i] <- "Basal Area"
      q.ei$ei.prefix[i] <- "Basal Area"
    } else if (q.ei$EI[i] == "ASC") {
      q.ei$indicator.group[i] <- "Abundance by Size Class"
      q.ei$ei.prefix[i] <- "Abundance"
    } else if (q.ei$EI[i] == "CH") {
      q.ei$indicator.group[i] <- "Canopy Height"
      q.ei$ei.prefix[i] <- "Canopy Height"
    } else if (q.ei$EI[i] == "BAES") {
      q.ei$indicator.group[i] <- "Basal Area Exotic Species"
      q.ei$ei.prefix[i] <- "Basal Area Exotic Species"
    } else if (q.ei$EI[i] == "TS") {
      q.ei$indicator.group[i] <- "Taxonomic Similarity"
      q.ei$ei.prefix[i] <- "Taxonomic Similarity"
    } else {
      q.ei$indicator.group[i] <- "[Unknown indicator group!]"
      q.ei$ei.prefix[i] <- "[Unknown indicator prefix!"
    }
  }

  # Make strata more readable (optional)
  # List all stratum names or codes that you are
  # likely to want to change to a more readable alias
  for (i in 1:nrow(q.ei)) {
    if (q.ei$stratum[i] == "Herb") {
      q.ei$stratum[i] <- "Herbs"
    } else if (q.ei$stratum[i] == "Shrub") {
      q.ei$stratum[i] <- "Shrubs"
    } else if (q.ei$stratum[i] == "Tree") {
      q.ei$stratum[i] <- "Trees"
    } else if (q.ei$stratum[i] == "D_Rock") {
      q.ei$stratum[i] <- "Rock Decomposers"
    } else if (q.ei$stratum[i] == "D_Soil") {
      q.ei$stratum[i] <- "Soil Decomposers"
    } else if (q.ei$stratum[i] == "D_Wood") {
      q.ei$stratum[i] <- "Wood Decomposers"
    } else if (q.ei$stratum[i] == "Dominant_Trees") {
      q.ei$stratum[i] <- "Dominant Trees"
    } else if (q.ei$stratum[i] == "Epiphyte") {
      q.ei$stratum[i] <- "Epiphytes"
    } else if (q.ei$stratum[i] == "Herb_Dwarf") {
      q.ei$stratum[i] <- "Dwarf Herbs"
    } else if (q.ei$stratum[i] == "Low_Shrub") {
      q.ei$stratum[i] <- "Low Shrubs"
    } else if (q.ei$stratum[i] == "Tall_Shrub") {
      q.ei$stratum[i] <- "Tall Shrubs"
    } else if (q.ei$stratum[i] == "Main_Canopy") {
      q.ei$stratum[i] <- "Main Canopy"
    } else if (q.ei$stratum[i] == "Subcanopy_Trees") {
      q.ei$stratum[i] <- "Subcanopy Trees"
    } else if (q.ei$stratum[i] == "SubstrateDecWood") {
      q.ei$stratum[i] <- "Decomposing Wood"
    } else if (q.ei$stratum[i] == "SubstrateOrganicMatter") {
      q.ei$stratum[i] <- "Organic Matter"
    } else if (q.ei$stratum[i] == "Height <0.05m") {
      q.ei$stratum[i] <- "Height1: <0.05m"
    } else if (q.ei$stratum[i] == "Height 0.05-0.25m") {
      q.ei$stratum[i] <- "Height2: 0.05-0.25m"
    } else if (q.ei$stratum[i] == "Height 0.25-1m") {
      q.ei$stratum[i] <- "Height3: 0.25-1m"
    } else if (q.ei$stratum[i] == "Height >1m") {
      q.ei$stratum[i] <- "Height4: >1m"
    }
  }

  # Add indicator-group and indicator-stratum columns
  q.ei$indicator <- paste0(q.ei$ei.prefix, " ", q.ei$stratum)
  q.ei$indicator[q.ei$stratum == "nostrata"] <- q.ei$indicator.group[q.ei$stratum == "nostrata"]

  # Add MDES column
  q.ei$mdes <- NA
  for (i in 1:nrow(q.ei)) {
    q.ei$mdes[i] <- max(
      q.ei$q[i] - q.ei$q.lcl[i],
      q.ei$q.ucl[i] - q.ei$q[i]
    )
  }

  # Remove superfluous columns
  drop.cols <- c(
    "EI", "ei.prefix", "bm.val",
    "lc.fg", "bm.fg", "stratum",
    "f.med", "f.max", "f.min",
    "b.med", "b.max", "b.min",
    "diff", "q.diff.raw", "p.diff",
    "q.diff", "q.diff.method", "q.wt.method",
    "ol", "ol.lcl", "ol.ucl", "ol.w",
    "q.raw"
  )
  q.ei <- q.ei[, !names(q.ei) %in% c(drop.cols)]

  # Rearrange columns
  q.ei <- df.reorder(q.ei, col.move = "indicator.group", col.before = "bm.veg")
  q.ei <- df.reorder(q.ei, col.move = "indicator", col.before = "indicator.group")

  # Set precision of quality and MDES
  q.ei <- format_table_columns(q.ei, c(q = 2, q.lcl = 2, q.ucl = 2, mdes = 2))

  cat("done\n")

  df.meta <- workbook_metadata(config, analysis_time)

  df.settings <- workbook_parameters(config)

  df.ind <- workbook_indicators(inputs, config)

  df.lc <- workbook_landcover(inputs, config)

  #####################################################
  # Make data frames more Excel-friendly
  #####################################################

  cat("Making data frames Excel-friendly:\n")

  cat("  Renaming data frames...")
  # Prefix dfs to distinguish them from sheet objects
  df.q.ei <- q.ei
  df.q.fg <- q.fg
  df.q.overall <- q.overall
  if (config$omit_quality_hectares) qh.bm <- summary[[file.qh.bm]]
  df.qh.bm <- qh.bm
  rm(q.ei, q.fg, q.overall, qh.bm)
  cat("done\n")

  cat("  Combining quality scores and confidences limis in single column...")
  df.q.ei$q.full <- format_quality_interval(df.q.ei$q, df.q.ei$q.lcl, df.q.ei$q.ucl)
  df.q.fg$q.full <- format_quality_interval(df.q.fg$q, df.q.fg$q.lcl, df.q.fg$q.ucl)
  df.q.overall$q.full <- format_quality_interval(
    df.q.overall$q, df.q.overall$q.lcl,
    df.q.overall$q.ucl
  )
  if (("qh" %in% colnames(df.q.overall)) && ("qh.lcl" %in% colnames(df.q.overall)) &&
    ("qh.ucl" %in% colnames(df.q.overall))) {
    df.q.overall$qh.full <- format_quality_interval(
      df.q.overall$qh, df.q.overall$qh.lcl,
      df.q.overall$qh.ucl
    )
  }
  if (config$omit_quality_hectares == FALSE) {
    df.qh.bm$qh.full <- format_quality_interval(df.qh.bm$qh, df.qh.bm$qh.lcl, df.qh.bm$qh.ucl)
  }
  cat("done\n")

  cat("  Reordering columns...")
  df.q.ei <- df.reorder(df.q.ei, col.move = "q.full", col.before = "q.ucl")
  df.q.fg <- df.reorder(df.q.fg, col.move = "q.full", col.before = "q.ucl")
  df.q.overall <- df.reorder(df.q.overall, col.move = "q.full", col.before = "q.ucl")
  df.q.overall <- df.reorder(df.q.overall, col.move = "mdes", col.before = "q.full")
  if (("qh.full" %in% colnames(df.q.overall)) && ("area_ha" %in% colnames(df.q.overall))) {
    df.q.overall <- df.reorder(df.q.overall, col.move = "qh.full", col.before = "area_ha")
  }
  if (config$omit_quality_hectares == FALSE) {
    if (("qh.full" %in% colnames(df.qh.bm)) && ("area_ha" %in% colnames(df.qh.bm))) {
      df.qh.bm <- df.reorder(df.qh.bm, col.move = "qh.full", col.before = "area_ha")
    }
  }
  cat("done\n")

  cat("  Sorting data frames...")
  df.q.ei <- df.q.ei[order(
    df.q.ei$indicator.group, df.q.ei$indicator, df.q.ei$bm.veg,
    df.q.ei$landcover
  ), ]
  cat("done\n")

  cat("  Dropping superfluous columns...")
  drop.cols <- c("q", "q.lcl", "q.ucl", "qh", "qh.lcl", "qh.ucl")
  df.q.ei <- df.q.ei[, !(names(df.q.ei) %in% drop.cols)]
  df.q.fg <- df.q.fg[, !(names(df.q.fg) %in% drop.cols)]
  df.q.overall <- df.q.overall[, !(names(df.q.overall) %in% drop.cols)]
  if (config$omit_quality_hectares == FALSE) {
    df.qh.bm <- df.qh.bm[
      ,
      !(names(df.qh.bm) %in% drop.cols)
    ]
  }
  cat("done\n")

  cat("  Renaming headers...")
  # Keep all to support the variable content of
  # these dfs, especially q.overall

  df.q.ei <- rename_workbook_headers(df.q.ei)
  df.q.fg <- rename_workbook_headers(df.q.fg)
  df.q.overall <- rename_workbook_headers(df.q.overall)
  df.ind <- rename_workbook_headers(df.ind)
  df.lc <- rename_workbook_headers(df.lc)
  if (!config$omit_quality_hectares) df.qh.bm <- rename_workbook_headers(df.qh.bm)

  cat("done\n")

  sheets <- list(
    Meta = df.meta, Parameters = df.settings, Indicators = df.ind, LandCover = df.lc,
    Q.i = df.q.ei, Q.fg = df.q.fg, Q.overall = df.q.overall
  )
  if (!config$omit_quality_hectares) sheets$QH.bm <- df.qh.bm
  sheets
}

workbook_metadata <- function(config, analysis_time) {
  ##########################################
  # Metadata
  ##########################################

  cat("Preparing metadata...")

  # Get latest analysis date/time from one of the CSVs
  latest.run <- analysis_time
  latest.run <- format(latest.run, "%Y-%m-%d %H:%M:%S %Z")

  df.meta <- data.frame(
    header = character(0),
    value = character(0),
    stringsAsFactors = FALSE
  )
  df.meta <- rbind(
    df.meta,
    data.frame(
      head = c("Project:", "Assessment:", "Analysis date:"),
      value = c(sub("-min$", "", config$project), config$assessment, latest.run),
      stringsAsFactors = FALSE
    )
  )
  cat("done\n")

  df.meta
}

workbook_parameters <- function(config) {
  ##########################################
  # Parameters
  ##########################################

  cat("Preparing summary of selected parameters...")

  df.settings <- data.frame(
    Parameter = character(0),
    Category = character(0),
    Value = character(0),
    Notes = character(0),
    stringsAsFactors = FALSE
  )

  # Pipe-delimited vector of category, name and value of selected parameters
  params.list <- c(
    paste0(
      "RAW_PLOTDATA_FILENAME|Raw data|", config$source_plot_data_filename,
      "|Raw plot data file"
    ),
    paste0(
      "RAW_SPP_FILENAME|Raw data|", config$source_plot_data_filename,
      "|Raw species attribute file "
    ),
    paste0(
      "N.MIN.ABS|Data validation|", config$min_plots,
      "|Minimum plots to run VQA for land cover class"
    ),
    paste0(
      "REMOVE.TD.OUTLIERS|Indicator options|", config$remove_td_outliers,
      "|Remove TD outliers?"
    ),
    paste0(
      "IS.EXOTIC.MISSING.ASSUME.NATIVE|Indicator options|",
      config$missing_exotic_status_assume_native, "| "
    ),
    paste0(
      "Include no-data plots|Model settings|", config$include_plots_nodata,
      "|Include plots with no data, assuming indicator value=0"
    ),
    paste0(
      "Q.FG.GROUP.BY|Model settings|", config$functional_group_group_by,
      "|Calculate Q.fg as mean of indicator qualities or indicator group qualities? "
    ),
    paste0(
      "Q.FG.METHOD|Model settings|", config$functional_group_method,
      "|Type of mean used to calculate Q.fg"
    ),
    paste0(
      "Q.OVERALL.METHOD|Model settings|", config$overall_quality_method,
      "|Type of mean used to calculate Q.overall"
    ),
    paste0("QH.METHOD|Model settings|", "Empirical", "|Quality Hectares method"),
    paste0("boot.reps|Model settings|", config$bootstrap_reps, "|Number of bootstrap replicates")
  )

  # Split the elements into separate vectors
  # Source: https://stackoverflow.com/a/19410256/2757825
  p.names <- sapply(strsplit(params.list, split = "|", fixed = TRUE), `[`, 1)
  p.categories <- sapply(strsplit(params.list, split = "|", fixed = TRUE), `[`, 2)
  p.values <- sapply(strsplit(params.list, split = "|", fixed = TRUE), `[`, 3)
  p.notes <- sapply(strsplit(params.list, split = "|", fixed = TRUE), `[`, 4)

  # Compose the final data frame
  df.settings <- rbind(
    df.settings,
    data.frame(
      Parameter = p.names,
      Category = p.categories,
      Value = p.values,
      Notes = p.notes,
      stringsAsFactors = FALSE
    )
  )
  cat("done\n")

  df.settings
}

workbook_indicators <- function(inputs, config) {
  EI.F.GROUPS <- cbind(
    EI = unlist(config$active_indicators),
    f.group = unlist(config$indicator_groups[unlist(config$active_indicators)])
  )
  file.ei.stratum.include <- "ei.stratum.include.csv"
  file.ei.stratum.veg.include <- "ei.stratum.veg.include.csv"
  ##########################################
  # Indicators
  ##########################################

  cat("Preparing indicator summary:\n")

  cat("  Loading parameters for indicator groups...")
  df.ind <- as.data.frame(EI.F.GROUPS) # Original indicator df, from params file

  # Initialize remaining columns
  df.ind$ind.name <- as.character("")
  df.ind$ind.group <- as.character("")
  df.ind$has.strata <- as.logical(FALSE)
  df.ind$stratum <- as.character("n/a")
  df.ind$distn <- as.character("")
  df.ind$q.method <- as.character("")
  df.ind$test.tail <- as.character("")
  df.ind$bm.val <- as.character("n/a")

  # Populate values for indicator groups
  for (i in 1:nrow(df.ind)) {
    # Retrieve parameters for this indicator group
    curr.ei <- df.ind[i, c("EI")]
    curr.ei.params <- resolve_params(
      config$indicator_defaults,
      config$indicator_settings[[curr.ei]]
    )

    # Load indicator parameters
    df.ind$ind.name[df.ind$EI == curr.ei] <- curr.ei.params$name # Temporary
    df.ind$ind.group[df.ind$EI == curr.ei] <- curr.ei.params$name
    df.ind$has.strata[df.ind$EI == curr.ei] <- curr.ei.params$has_stratum
    df.ind$distn[df.ind$EI == curr.ei] <- curr.ei.params$distribution
    df.ind$q.method[df.ind$EI == curr.ei] <- curr.ei.params$quality_method
    df.ind$test.tail[df.ind$EI == curr.ei] <- curr.ei.params$test_tail

    if (!is.null(curr.ei.params$benchmark_value) && !is.na(curr.ei.params$benchmark_value)) {
      df.ind$bm.val[df.ind$EI == curr.ei] <- curr.ei.params$benchmark_value
    }
  }

  # Set column order
  df.ind <- df.ind[, c(
    "EI", "ind.group", "ind.name", "f.group", "has.strata", "stratum",
    "distn", "q.method", "test.tail", "bm.val"
  )]
  cat("done\n")

  # Load ei.stratum.include file (="blacklist file")
  cat("  Loading ei.stratum.include file...")
  df.strata <- inputs[[file.ei.stratum.include]]
  # df.strata <- df.strata[ df.strata$include==TRUE & !df.strata$stratum=='nostrata', ]
  df.strata <- df.strata[df.strata$include == TRUE, ]
  if (config$pcess_herbs_only == TRUE) {
    df.strata <- df.strata[
      !df.strata$EI == "PCESS" | (
        df.strata$EI == "PCESS" & tolower(df.strata$stratum) %in%
          c("herb", "herbs", "hierba", "hierbas", "forb", "forbs", "grasses", "graminoids")
      ),
    ]
  }
  df.strata$stratum <- capFirst(df.strata$stratum)
  df.strata <- df.strata[, c("EI", "stratum")]
  cat("done\n")

  # Load ei.stratum.include file (="whitelist file")
  cat("  Loading ei.stratum.veg.include file...")
  df.strata.wl <- inputs[[file.ei.stratum.veg.include]]
  df.strata.wl <- df.strata.wl[df.strata.wl$include == TRUE & !df.strata.wl$stratum == "nostrata", ]
  # Omit bm.veg; we just want a list of all strata included, even if some were
  # excluded for some types of vegetation
  df.strata.wl <- df.strata.wl[!is.na(df.strata.wl$EI), c("EI", "stratum")]
  df.strata.wl <- unique(df.strata.wl)
  cat("done\n")

  # Combine unique strata from blacklist and whitelist files
  cat("  Combining all unique indicators+strata combinations from blacklist and whitelist files...")
  df.strata <- rbind(df.strata, df.strata.wl)
  df.strata <- unique(df.strata)
  df.strata <- df.strata[order(df.strata$EI, df.strata$stratum), ]
  cat("done\n")

  # Combine unique strata from blacklist and whitelist files
  cat("  Add functional group to df.strata...")
  df.strata <- merge(df.strata, EI.F.GROUPS, by = "EI", all.x = TRUE)
  if (nrow(df.strata[is.na(df.strata$f.group), ]) > 1) {
    cat("WARNING: f.group missing for one or more indicators in df.strata ")
  } else {
    (
      cat("done\n")
    )
  }

  if (config$summary_pluralize_strata == TRUE) {
    # Make selected indicators plural
    for (i in 1:nrow(df.ind)) {
      if (curr.ei %in% c("PCGF", "GC", "PCESS")) {
        df.ind$stratum[i] <- pluralize(df.ind$stratum[i])
      }
    }
  }

  cat("  Adding indicator group strata...")
  EIs <- unique(df.ind$EI)

  # Split indicators with strata into component indicators (indicator-strata)
  for (curr.ei in EIs) {
    if (df.ind$has.strata[df.ind$EI == curr.ei] == FALSE) {
      next # Skip to next if no strata
    } else {
      df.ind.old.row <- df.ind[df.ind$EI == curr.ei, ]
      old.ind.name <- df.ind.old.row$ind.name
      ind.name.prefix <- ""
      # if ( grepl( "Percent Cover", old.ind.name )) {
      #   ind.name.prefix <- "Percent Cover "
      # }
      # Set indicator name prefix, if any
      if (curr.ei %in% c("PCGF", "GC")) {
        ind.name.prefix <- "Percent Cover "
      } else if (curr.ei %in% c("PCESS")) {
        ind.name.prefix <- "Percent Cover Exotic "
      } else if (curr.ei %in% c("ASC")) {
        ind.name.prefix <- "Abundance "
      } else if (curr.ei %in% c("GC")) {
        ind.name.prefix <- "Abundance "
      }

      # Flag the current row for deletion when done
      df.ind$ind.name[df.ind$EI == curr.ei] <- "DELETE"
      df.ind.old.row <- df.ind.old.row[, !(names(df.ind.old.row) == "stratum")]
      df.ind.old.row$ind.name <- ind.name.prefix
    }

    df.curr.strata <- df.strata[df.strata$EI == curr.ei, c("EI", "stratum")]
    df.ind.new.rows <- merge(df.ind.old.row, df.curr.strata, by = "EI")

    if (config$summary_pluralize_strata == TRUE) {
      # Make selected indicators plural
      if (curr.ei %in% c("PCGF", "PCESS")) {
        for (j in 1:nrow(df.ind.new.rows)) {
          df.ind.new.rows$stratum[j] <- capFirst(pluralize(tolower(df.ind.new.rows$stratum[j])))
        }
      }
    }
    df.ind.new.rows$ind.name <- paste0(df.ind.new.rows$ind.name, df.ind.new.rows$stratum)

    # Set column order of new rows, append to df.ind and drop old row
    df.ind.new.rows <- df.ind.new.rows[, c(
      "EI", "ind.group", "ind.name", "f.group",
      "has.strata", "stratum", "distn", "q.method", "test.tail", "bm.val"
    )]

    df.ind <- rbind(df.ind, df.ind.new.rows)
  }

  # Remove old rows and sort
  df.ind <- df.ind[!df.ind$ind.name == "DELETE", ]
  df.ind <- df.ind[order(df.ind$f.group, df.ind$ind.group, df.ind$ind.name), ]

  # Make distribution values more readable
  df.ind <- df.ind %>%
    mutate(distn = case_when(
      distn == "NBin" ~ "Negative binomial",
      distn == "Bet" ~ "Beta",
      distn == "gamma" ~ "Gamma",
      TRUE ~ distn
    ))

  # Set final df but keep original
  df.ind.all <- df.ind
  df.ind <- df.ind[, c(
    "ind.name", "ind.group", "f.group", "distn", "q.method", "test.tail",
    "bm.val"
  )]

  cat("done\n")

  df.ind
}

workbook_landcover <- function(inputs, config) {
  file.lc <- "landCover.csv"
  file.veg <- "vegetation.csv"
  ##########################################
  # Land cover
  ##########################################

  cat("Preparing land cover summary:\n")

  cat("  Loading input file '", file.lc, "'...")
  df.lc <- inputs[[file.lc]]
  if (!is.null(inputs[[file.veg]])) df.veg <- inputs[[file.veg]]
  cat("done\n")

  if (!is.null(inputs[[file.veg]])) {
    cat("  Adding bm vegetation full names...")
    df.lc <- merge(df.lc, df.veg[, c("vegClass", "vegName")], by = "vegClass", all.x = TRUE)
    df.lc <- df.reorder(df.lc, col.move = "vegName", col.before = "vegClass")
    cat("done\n")
  }
  df.lc <- df.reorder(df.lc, col.move = "landCover", move.first = TRUE)

  if (config$omit_quality_hectares == TRUE && "area_ha" %in% colnames(df.lc)) {
    df.lc <- df.lc[, !(names(df.lc) == "area_ha")]
  }

  cat("  Setting NAs to zero...")
  # Set NAs to zero
  if ("area_ha" %in% colnames(df.lc) &&
    any(is.na(df.lc$area_ha))) {
    df.lc$area_ha[is.na(df.lc$area_ha)] <- 0
  }
  if (any(is.na(df.lc$focal_plots))) df.lc$focal_plots[is.na(df.lc$focal_plots)] <- 0
  if (any(is.na(df.lc$bm_plots))) df.lc$bm_plots[is.na(df.lc$bm_plots)] <- 0
  cat("done\n")

  # Set precision for area if applicable
  if ("area_ha" %in% colnames(df.lc)) {
    cat("  Setting area_ha precision...")
    df.lc <- format_table_columns(df.lc, c(area_ha = 1))
    cat("done\n")
  }

  cat("  Setting final sort order...")
  df.lc <- df.lc[order(df.lc$vegClass, df.lc$landCover), ]
  cat("done\n")

  df.lc
}

################################################
# Import VQA Demo #2 raw data
#
# Imports raw data, performs source-specific
# correction, and outputs as CSV files to data
# directory "inputs/" as generic VQA input files
################################################

# Loading script-specific library here until find
# solution to warnings for this library
cat("Loading package 'sqldf'...")
suppressWarnings(suppressPackageStartupMessages(library(sqldf)))
cat("Ignore preceding warning!\n")

# Set up error message df
curr.date <- Sys.Date()
df.err <- as.data.frame(paste0("Import errors for project ", PROJ, " on ", curr.date, "."))
colnames(df.err) <- "message"
df.err$message <- as.character(df.err$message)
df.err$action <- as.character("")

# Force detailed screen echo
# Cluttered output but sometimes useful
echo.objects <- FALSE

###############################
# Import raw data
###############################

db_raw_compressed_filename <- paste0(RAW_PLOTDATA_FILENAME, ".zip")
db_raw_compressed_file <- paste0(RAWDATADIR, db_raw_compressed_filename)
cat("Uncompressing zipped database file '", db_raw_compressed_filename, "'...", sep = "")
if (file.exists(db_raw_compressed_file)) {
  unzip(zipfile = db_raw_compressed_file, exdir = RAWDATADIR)
  cat("done\n")
} else {
  msg_err <- paste0("ERROR: file '", db_raw_compressed_file, "' not found!\n")
  stop_quietly(msg_err)
}

db_raw_filename <- RAW_PLOTDATA_FILENAME
db_raw_file <- paste0(RAWDATADIR, RAW_PLOTDATA_FILENAME)
cat("Importing MS Access database file '", db_raw_filename, "'...", sep = "")
db_raw <- quiet(mdb.get(db_raw_file))
cat("done\n")

# List the data frames (db is a list of dfs):
cat("Extracting tables:\n")
for (i in 1:length(names(db_raw))) cat("- ", names(db_raw)[i], "\n", sep = "")

# Extract the tables to data frames
plot_raw <- db_raw$DPm_PlotData
veg_raw <- db_raw$DPm_PlotVegetation
df.spp.lookup_raw <- db_raw$DPm_VegetationSpecies_Lookup
df.lc.lookup_raw <- db_raw$VQACORPORATELOOKUP_MASTER
df.veg.layers_raw <- db_raw$DPm_VegetationLayers

cat("Removing uncompressed database file...")
file.remove(db_raw_file)
cat("done\n")

# Save the raw data unchanged and work with the copies
plot <- plot_raw
veg <- veg_raw
df.spp.lookup <- df.spp.lookup_raw
df.lc.lookup <- df.lc.lookup_raw # Not used in this application (values already transferred to plot table)
df.veg.layers <- df.veg.layers_raw # Not used in this application

##########################################
# Standardizations
##########################################

cat("Performing general standardizations:\n")

cat("- Converting factors to characters...")
plot <- factors.to.chr(plot)
veg <- factors.to.chr(veg)
df.spp.lookup <- factors.to.chr(df.spp.lookup)
# df.lc.lookup <- factors.to.chr(df.lc.lookup)
cat("done\n")

# Convert literal "<Null>" to NA
# Literal "<Null>" is an artifact of import from Access
cat("- Converting literal '<Null>' to NA...")
plot <- replace(plot, plot == "<Null>", NA)
veg <- replace(veg, veg == "<Null>", NA)
df.spp.lookup <- replace(df.spp.lookup, df.spp.lookup == "<Null>", NA)
# df.lc.lookup <- replace(df.lc.lookup, df.lc.lookup =="<Null>", NA)
cat("done\n")

cat("- Renaming fields...")
names(plot)[names(plot) == "FocalorBenchmark"] <- "focalOrBenchmark"
names(df.spp.lookup)[names(df.spp.lookup) == "Native.Status"] <- "NativeStatus"
names(plot)[names(plot) == "OBJECTID.."] <- "OBJECTID"
names(plot)[names(plot) == "SHAPE.."] <- "SHAPE"
names(veg)[names(veg) == "OBJECTID.."] <- "OBJECTID"
names(veg)[names(veg) == "PlotID.."] <- "PlotID"
names(veg)[names(veg) == "CommonName.."] <- "CommonName"
names(veg)[names(veg) == "GlobalID.."] <- "GlobalID"
names(df.spp.lookup)[names(df.spp.lookup) == "OBJECTID.."] <- "OBJECTID"
names(df.spp.lookup)[names(df.spp.lookup) == "ScientificName.."] <- "ScientificName"
names(df.spp.lookup)[names(df.spp.lookup) == "CommonName.."] <- "CommonName"
# names(df.lc.lookup)[names(df.lc.lookup) == 'OBJECTID..'] <- 'OBJECTID'
cat("done\n")

cat("- Dropping some unwanted columns...")
drop.cols <- c("GUID", "GlobalID", "created.user", "created.date", "last.edited.user", "last.edited.date")
veg <- veg[, !(names(veg) %in% drop.cols)]
cat("done\n")

cat("- Standardizing key values...")
plot$focalOrBenchmark[plot$focalOrBenchmark == "Focal"] <- "f"
plot$focalOrBenchmark[plot$focalOrBenchmark == "Benchmark"] <- "b"
cat("done\n")

# Make corrections provided in advance by client
cat("- Performing known corrections:\n")
cat("-- Setting DisturbanceHistory for plotID  'TS_4_t' to 'Mining'...")
plot$DisturbanceHistory[plot$PlotID == "TS_4_t"] <- "Mining"
plot$PlotStatus[plot$PlotID == "TS_4_t"] <- "Mining"
cat("done\n")

if (dpm_conflicting_disturbance_set_undisturbed == TRUE) {
  # See explanation for this parameter in ps-params file
  cat("-- Setting DPm plots with conflicting PlotStatus & DisturbanceHistory to 'Undisturbed'...")
  plot$DisturbanceHistory[plot$PlotID %in% c("TV13_A", "TV_12", "TV11")] <- "Undisturbed"
  plot$PlotStatus[plot$PlotID %in% c("TV13_A", "TV_12", "TV11")] <- "Undisturbed"
  cat("done\n")
}

cat("- Creating new plot code field, keeping PlotID as backup...")
plot$plot_code <- plot$PlotID
plot <- df.reorder(plot, col.move = "plot_code", col.before = "PlotID")
veg$plot_code <- veg$PlotID
veg <- df.reorder(veg, col.move = "plot_code", col.before = "PlotID")
cat("done\n")
cat("Filtering out known bad plots...")
plot <- plot[!plot$PlotID %in% known.bad.plots, ]
veg <- veg[!veg$PlotID %in% known.bad.plots, ]
cat("done\n")

cat("- Adding column 'Site' and updating focalOrBenchmark accordingly...")
plot$site <- "Main"
plot$site[plot$focalOrBenchmark == "Focal_Offset"] <- "Offset1"
plot$site[plot$focalOrBenchmark == "b"] <- "Benchmark"
plot$focalOrBenchmark[plot$focalOrBenchmark == "Focal_Offset"] <- "f"
plot <- df.reorder(plot, col.move = "site", col.before = "PlotID")
cat("done\n")

##########################################
# Filter plots to appropriate scope
##########################################

cat("Filtering by scope '", scope, "'...", sep = "")
scope <- tolower(scope)
if (scope %in% c("baseline", "current")) {
  plot <- plot[plot$site == "Main" | plot$site == "Benchmark", ]
  plot.ids <- unique(plot$PlotID)
  veg <- veg[veg$PlotID %in% plot.ids, ]
} else if (scope %in% c("offset1", "offset", "offset_current", "offset_baseline")) {
  plot <- plot[plot$site == "Offset1" | plot$site == "Benchmark", ]
  plot.ids <- unique(plot$PlotID)
  veg <- veg[veg$PlotID %in% plot.ids, ]
} else {
  stop("Unknown scope!\n")
}
cat("done\n")

##########################################
# Add land cover information to plot table
##########################################

cat("Adding land cover information to plot table:\n")

cat("- Adding land cover fields...")
# Define land cover metadata columns
veg.meta.cols <- c("disturbance", "eg.type", "bm.veg", "landCover") # Land cover metadata columns
plot.meta.cols <- c("OBJECTID", "plot_code", "focalOrBenchmark") # Plot metadata cols
meta.cols <- c(plot.meta.cols, veg.meta.cols) # All metadata cols
cat("done\n")

# Some reordering first
plot <- df.reorder(df = plot, col.move = "EcosystemCode", move.last = TRUE)
plot <- df.reorder(df = plot, col.move = "EcosystemClass", move.last = TRUE)
plot <- df.reorder(df = plot, col.move = "EcosystemGrouping", move.last = TRUE)
plot <- df.reorder(df = plot, col.move = "DisturbanceHistory", move.last = TRUE)

cat("- Adding default land cover and bm vegetation to plots...")
plot$bm.veg <- NA
plot$landCover <- NA

#
# Benchmark plots
#

# Assign bm veg
plot$bm.veg[plot$focalOrBenchmark == "b"] <-
  plot$EcosystemGrouping[plot$focalOrBenchmark == "b"]
# Assign land cover
plot$landCover[plot$focalOrBenchmark == "b"] <-
  plot$EcosystemGrouping[plot$focalOrBenchmark == "b"]

#
# Focal plots
#

# Assign bm veg
plot$bm.veg[plot$focalOrBenchmark == "f"] <-
  plot$EcosystemGrouping[plot$focalOrBenchmark == "f"]

# Assign land cover
plot$landCover[plot$focalOrBenchmark == "f"] <- paste0(
  plot$EcosystemGrouping[plot$focalOrBenchmark == "f"],
  " - ",
  plot$PlotStatus[plot$focalOrBenchmark == "f"]
)
plot$eg <- plot$EcosystemGrouping
cat("done\n")

cat("- Adding disturbance column, setting to DisturbanceHistory...")
plot$disturbance[!is.na(plot$DisturbanceHistory)] <- plot$DisturbanceHistory[!is.na(plot$DisturbanceHistory)]
plot$disturbance[is.na(plot$disturbance) & !is.na(plot$PlotStatus)] <-
  plot$PlotStatus[is.na(plot$disturbance) & !is.na(plot$PlotStatus)]
plot$disturbance[is.na(plot$disturbance)] <- ""
cat("done\n")

cat("- Adding reclamation column (setting to ReclamationMethod)...")
plot$reclamation <- plot$ReclamationMethod
plot$reclamation[is.na(plot$reclamation)] <- "" # Empty string for "Not reclaimed"
cat("done\n")

cat("- Adding disturbance+reclamation column...")
# Prepr the component fields
plot$PlotStatus[is.na(plot$PlotStatus)] <- ""
plot$DisturbanceHistory[is.na(plot$DisturbanceHistory)] <- ""
plot$ReclamationMethod[is.na(plot$ReclamationMethod)] <- ""

# Alt disturbance-rec code
plot$dist.rec <- paste0(plot$DisturbanceHistory, "-", plot$PlotStatus)

# Remove orphan hyphens
for (i in 1:nrow(plot)) {
  str <- plot$dist.rec[i]
  first <- substring(str, 1, 1)
  if (first == "-") str <- substring(str, 2, nchar(str))
  last <- substring(str, nchar(str), nchar(str))
  if (last == "-") str <- substring(str, 1, nchar(str) - 1)
  plot$dist.rec[i] <- str
}

plot <- plot %>%
  mutate(dist.rec = case_when(
    dist.rec == "Undisturbed-Undisturbed" ~ "Undisturbed",
    dist.rec == "Logging-Disturbed" ~ "Disturbed",
    dist.rec == "Logging-Undisturbed" ~ "Disturbed",
    dist.rec == "Mining-Natural Regeneration" ~ "Established Rec",
    dist.rec == "Mining-Reclaimed" ~ "Reclaimed",
    dist.rec == "Disturbed" ~ "Disturbed",
    dist.rec == "Undisturbed" ~ "Undisturbed",
    dist.rec == "Mining-Disturbed" ~ "Mining",
    dist.rec == "Mining-Mining" ~ "Mining",
    TRUE ~ dist.rec
  ))

# Add reclamation code, if any
plot$dist.rec <- paste0(plot$dist.rec, "-", plot$ReclamationMethod)

# Remove orphan hyphens
for (i in 1:nrow(plot)) {
  str <- plot$dist.rec[i]
  first <- substring(str, 1, 1)
  if (first == "-") str <- substring(str, 2, nchar(str))
  last <- substring(str, nchar(str), nchar(str))
  if (last == "-") str <- substring(str, 1, nchar(str) - 1)
  plot$dist.rec[i] <- str
}

plot <- plot %>%
  mutate(dist.rec = case_when(
    dist.rec == "Undisturbed-Undisturbed" ~ "", # No code if undisturbed
    dist.rec == "Undisturbed" ~ "", # No code if undisturbed
    dist.rec == "Disturbed" ~ "Disturbed",
    dist.rec == "Established Rec-Established Rec" ~ "Established Rec",
    dist.rec == "Reclaimed-Reclaimed" ~ "Reclaimed",
    dist.rec == "Logging-Disturbed" ~ "Disturbed",
    dist.rec == "Logging-Undisturbed" ~ "Disturbed",
    dist.rec == "Mining-Natural Regeneration" ~ "Established Rec",
    dist.rec == "Mining-Reclaimed" ~ "Reclaimed",
    dist.rec == "Mining-Disturbed" ~ "Mining",
    dist.rec == "Mining-Mining" ~ "Mining",
    TRUE ~ dist.rec
  ))
cat("done\n")

#############################
# Ecosystem Type (eg.type)
#############################

cat("Populating eg.type (ecosystem type codes):\n")

cat("-- Assigning eg.type codes...")
# Vectors eg.anthro, eg.non.veg.strict and eg.forested defined in params file
plot$cnt <- 1
plot$eg.type <- "vegetated/non-forest"
plot$eg.type[plot$eg %in% eg.anthro] <- "anthropogenic"
plot$eg.type[plot$eg %in% eg.non.veg.strict] <- "non-vegetated"
plot$eg.type[plot$eg %in% eg.forested] <- "vegetated/forest"
cat("done\n")

cat("-- Checking for missing values of eg.type...")
plots.eg.type <- plot[, c(veg.meta.cols, "eg.type")]
plots.eg.type[is.na(plots.eg.type) | plots.eg.type == ""] <- "[unknown]"
plots.eg.type$cnt <- 1
plots.eg.type.summary <- aggregate(
  cnt ~ eg.type,
  data = plots.eg.type,
  FUN = sum,
  na.rm = TRUE,
  na.pass = NULL
)
if (echo.objects == TRUE) {
  cat("\n")
  print(plots.eg.type.summary, row.names = FALSE)
}

n.eg.type.missing <- length(plots.eg.type$eg.type[plots.eg.type$eg.type == "[unknown]"])
if (n.eg.type.missing > 0) {
  err.msg <- paste0(n.eg.type.missing, " plots are missing eg.type")
  err.action <- "[No action]"
  cat("FAIL: ", err.msg, "\n")

  # Set missing values to [unknown]
  plot$eg.type[is.na(plot$eg.type) | plot$eg.type == ""] <- "[unknown]"

  if (bad.plots.delete == TRUE) {
    err.action0 <- paste0(
      "- Deleting ", n.eg.type.missing,
      " plots due to missing missing values of eg.type (ecosystem type)"
    )
    cat("\n- WARNING: ", err.action0, "...done\n", sep = "")
    p.before <- nrow(plot)
    bad.plots.eg.type.missing <- plot[
      plot$eg.type == "[unknown]",
      veg.meta.cols
    ] # Save bad plots
    plot <- plot[!plot$eg.type == "[unknown]", ] # Delete bad plots from main df
    p.after <- nrow(plot)
    p.deleted <- p.before - p.after
    err.action1 <- paste0("Plots before: ", p.before)
    err.action2 <- paste0("Plots deleted: ", p.deleted)
    err.action3 <- paste0("Plots remaining: ", p.after)
    err.action <- paste0(
      err.action0, "; ", err.action1, ", ", err.action2, ", ",
      err.action3
    )
    cat("- ", err.action1, "\n- ", err.action2, "\n- ", err.action3, "\n")
    cat("- NOTE: plots with missing eg.type saved to 'bad.plots.eg.type.missing'\n", sep = "")
  }

  df.err <- rbind(df.err, c(err.msg, err.action))
  err.msg <- ""
  err.action <- ""
} else {
  cat("done\n")
}

#################################
# Confirm PKs still unique
#################################

cat("Checking primary keys unique:\n")
pk <- "plot_code"
cat("- PK '", pk, "' unique...", sep = "")
if (pk.uniq(plot, pk) == TRUE) {
  cat("pass\n")
} else {
  # Save the duplicated plot codes
  dup.plot.codes <- aggregate(cnt ~ plot_code, data = plot, FUN = sum)
  dup.plot.codes <- dup.plot.codes[dup.plot.codes$cnt > 1, ]
  names(dup.plot.codes)[names(dup.plot.codes) == "cnt"] <- "cnt.dup"
  plot.dup <- merge(plot, dup.plot.codes, by = "plot_code")
  plot.dup <- plot.dup[, c(
    "OBJECTID", "plot_code",
    "PlotStatus", "focalOrBenchmark"
  )]
  n.plots.dup <- length(unique(plot.dup$plot_code))
  n.plots.dup.rows <- nrow(plot.dup) - n.plots.dup
  err.msg <- paste0(
    n.plots.dup.rows,
    " rows in plot table 'POm_VQA_PlotData' have repeated plot codes and may be duplicates!"
  )
  err.action <- paste0(
    "Saving rows with duplicated plot codes to file '",
    file.dupe.plot.codes, "'"
  )
  df.err <- rbind(df.err, c(err.msg, err.action))
  filename <- paste0(RESULTSDIR, file.dupe.plot.codes)
  write.csv(plot.dup, file = filename, row.names = FALSE)

  if (dupe.plots.abort == TRUE) {
    stop("FAIL: Aborting due to duplicated plot codes!")
  } else {
    cat("\n", "- WARNING: ", err.msg, "\n", sep = "")
    cat("- ", err.action, "\n", sep = "")
  }
  err.msg <- ""
  err.action <- ""
}
# Confirm PK still unique
pk <- "OBJECTID"
cat("- PK '", pk, "' unique...", sep = "")
if (pk.uniq(plot, pk) == TRUE) {
  cat("pass\n")
} else {
  if (dupe.plots.abort == TRUE) {
    stop("FAIL: Aborting due to duplicated OBJECTID in plot table!")
  } else {
    cat("\n", "- WARNING: duplicated OBJECTID in plot table!\n")
  }
}

#############################
# Seral stage
#############################

cat("Populating seral stage...")

# Get seral stage from AgeClass
plot$seral <- ""
plot$seral[plot$eg.type == "vegetated/forest"] <- plot$AgeClass[plot$eg.type == "vegetated/forest"]

# Summarize counts of plots per category
plot$cnt <- 1
plot.seral.summary <- aggregate(
  cnt ~ eg + eg.type + seral,
  data = plot,
  FUN = sum,
  na.rm = TRUE
)
plot.seral.summary <- plot.seral.summary[do.call(order, plot.seral.summary), ]
if (echo.objects == TRUE) print(plot.seral.summary, row.names = FALSE, quote = FALSE)

plots.seral.missing <- plot[plot$seral == "[unknown.seral]", ]
n.seral.missing <- nrow(plots.seral.missing)

if (n.seral.missing > 0) {
  err.msg <- paste0(n.seral.missing, " forested plots cannot be assigned a seral stage due to missing or unmatched values of StructuralStage")
  err.action <- paste0(
    "Saving IDs of forested plots with missing seral stage to file '",
    file.seral.missing, "'"
  )
  cat("\n- WARNING: ", err.msg, "\n", sep = "")
  cat("- ", err.action, "\n", sep = "")

  # Restructure the df of bad plots for saving
  plots.seral.missing <- plots.seral.missing[, c(
    "OBJECTID",
    "plot_code",
    "EcosystemGrouping",
    "StructuralStage"
  )]
  colnames(plots.seral.missing) <- c(
    "OBJECTID",
    "PlotID",
    "EcosystemGrouping",
    "StructuralStage"
  )
  df.err <- rbind(df.err, c(err.msg, err.action))
  err.msg <- ""
  err.action <- ""
  filename <- paste0(RESULTSDIR, file.seral.missing)
  write.csv(plots.seral.missing, file = filename, row.names = FALSE)

  if (bad.plots.delete == TRUE) {
    err.action <- paste0(
      "Deleting ", n.seral.missing,
      " forested plots due to missing seral stage ('seral')"
    )
    cat("\n- WARNING: ", err.action, "...done\n")
    p.before <- nrow(plot)
    bad.plots.seral.missing <- plot[
      plot$seral == "[unknown.seral]",
      veg.meta.cols
    ] # Save bad plots
    plot <- plot[!plot$seral == "[unknown.seral]", ] # Delete bad plots from main df
    p.after <- nrow(plot)
    p.deleted <- p.before - p.after
    err.action1 <- paste0("Plots before: ", p.before)
    err.action2 <- paste0("Plots deleted: ", p.deleted)
    err.action3 <- paste0("Plots remaining: ", p.after)
    err.action <- paste0(
      err.action0, "; ", err.action1, ", ", err.action2, ", ",
      err.action3
    )
    cat("- ", err.action1, "\n- ", err.action2, "\n- ", err.action3, "\n")
    cat("- NOTE: forested plots with missing seral stage saved to 'bad.plots.seral.missing'\n", sep = "")
  }

  df.err <- rbind(df.err, c(err.msg, err.action))
  err.msg <- ""
  err.action <- ""
} else {
  cat("done\n")
}

##########################
# Plot EG + seral stage
##########################

cat("Populating eg.seral (Ecosystem Grouping + Seral Stage)...")

plot$eg.seral <- "[unknown]"

# Forested vegetation
plot$eg.seral[plot$eg.type == "vegetated/forest"] <- paste(
  plot$eg[plot$eg.type == "vegetated/forest"],
  plot$seral[plot$eg.type == "vegetated/forest"],
  sep = ", "
)

# Non-forested vegetation (no seral stage)
plot$eg.seral[plot$eg.type == "vegetated/non-forest"] <-
  plot$eg[plot$eg.type == "vegetated/non-forest"]

# Non-vegetation/anthropogenic
plot$eg.seral[plot$eg.type %in% c("anthropogenic", "non-vegetated")] <- "anthropogenic/non-vegetated"

# # Set the whole thing to unknown if any part missing
# plot$eg.seral[ grep("unknown", plot$eg.seral ) ] <- "[unknown]"

# Summarize counts of plots per category
plot.eg.seral.summary <- aggregate(
  cnt ~ eg.type + eg + seral + eg.seral,
  data = plot,
  FUN = sum,
  na.rm = TRUE
)
plot.eg.seral.summary <- plot.eg.seral.summary[do.call(order, plot.eg.seral.summary), ]
if (echo.objects == TRUE) print(plot.eg.seral.summary, row.names = FALSE, quote = FALSE)

# Save bad plots to separate df
n.eg.seral.missing <- nrow(plot[plot$eg.seral == "[unknown]", ])

if (n.eg.seral.missing > 0) {
  err.msg <- paste0(n.eg.seral.missing, " vegetated plots missing Ecosystem Grouping + Seral Stage (eg.seral)")
  err.action <- "[No action]"
  cat("\n- WARNING: ", err.msg, "\n", sep = "")

  # Save the bad plots
  bad.plots.eg.seral.missing <- plot[
    plot$eg.seral == "[unknown]",
    c(veg.meta.cols, "eg.type", "eg", "eg.seral")
  ]

  if (bad.plots.delete == TRUE) {
    err.action0 <- paste0(
      n.eg.seral.missing,
      " forested plots due to missing EG + seral stage ('eg.seral')"
    )
    cat("\n- WARNING: ", err.action, "...done\n", sep = "")
    p.before <- nrow(plot)
    plot <- plot[, c("PlotID", "eg.seral")] %>% filter(!grepl("unknown", eg.seral))
    p.after <- nrow(plot)
    p.deleted <- p.before - p.after
    err.action1 <- paste0("Plots before: ", p.before)
    err.action2 <- paste0("Plots deleted: ", p.deleted)
    err.action3 <- paste0("Plots remaining: ", p.after)
    err.action <- paste0(
      err.action0, "; ", err.action1, ", ", err.action2, ", ",
      err.action3
    )
    cat("- ", err.action1, "\n- ", err.action2, "\n- ", err.action3, "\n")
    cat("NOTE: forested plots with missing seral stage saved to 'bad.plots.eg.seral.missing'\n", sep = "")
  }

  df.err <- rbind(df.err, c(err.msg, err.action))
  err.msg <- ""
  err.action <- ""
} else {
  cat("done\n")
}

#####################################
# Flag, save and delete non-vegetated plots
#####################################

cat("Deleting non-vegetation plots")

if (non.veg.plots.delete == TRUE) {
  # Substitute eg.type for non-vegetated EGs
  plot$eg.seral[plot$eg.type == "anthropogenic"] <- "[Anthropogenic]"
  plot$eg.seral[plot$eg.type == "non-vegetated"] <- "[Non-vegetated]"
  plots.non.veg <- plot[!plot$eg.type %in% c("vegetated/forest", "vegetated/non-forest"), ]
  n.plots.non.veg <- nrow(plots.non.veg)

  if (n.plots.non.veg == 0) {
    cat("...0 non-veg plots...nothing to do\n")
  } else {
    # Calculate and display counts of plots per eg before deleting non-veg
    cat(":\n")

    # Get count of plots per EG
    plot.eg.seral.count <- aggregate(cnt ~ eg.seral,
      data = plot, FUN = sum, na.rm = TRUE
    )

    if (echo.objects == TRUE) cat("Plots per EG before deleting non-vegetated: \n")
    if (echo.objects == TRUE) {
      print(plot.eg.seral.count)
      cat("\n")
    }

    cat("- Saving non-vegetated plots to df 'plots.non.veg'...")
    p.veg.deleted <- 0
    p.veg.deleted <- nrow(plots.non.veg)
    cat("done\n")

    if (p.veg.deleted > 0) {
      err.action <- paste0("Removed ", p.veg.deleted, " non-vegetation plots")
      cat(err.action, "\n")
      plot <- plot[plot$eg.type %in% c("vegetated/forest", "vegetated/non-forest"), ]
      df.err <- rbind(df.err, c(err.msg, err.action))
      err.msg <- ""
      err.action <- ""
    }
  }
} else {
  cat("...SKIPPING\n")
}

##########################################
# Check for disturbed benchmark plots
##########################################

cat("Checking for disturbed benchmark plots...")

# Check for bm plots marked disturbed
plot.bm.disturbed <- plot[
  plot$focalOrBenchmark == "b" & plot$PlotStatus == "Disturbed",
  c("plot_code", "focalOrBenchmark", "PlotStatus", "EcosystemGrouping")
]
plot.bm.disturbed.plot_codes <- plot.bm.disturbed[, c("plot_code")]
n.plot.bm.disturbed <- nrow(plot.bm.disturbed)

if (!n.plot.bm.disturbed == 0) {
  # Set error file name and error message
  f.err <- file.plots.bm.disturbed
  err.msg <- paste0(n.plot.bm.disturbed, " benchmark plots marked 'Disturbed'")
  cat("\n- WARNING: ", err.msg, "\n", sep = "")

  # Fix error using the requested method and set action message
  if (plot.bm.disturbed.action == "set focal") {
    # Adjust focalOrBenchmark to agree with disturbance
    plot$focalOrBenchmark[plot$disturbance == "Disturbed"] <- "f"
    err.action <- paste0("Changing focalOrBenchmark to 'f' and saving bad plot details to ", f.err, "'")
  } else if (plot.bm.disturbed.action == "set benchmark") {
    # Adjust bm disurbance to agree with focalOrBenchmark
    plot$disturbance[plot$focalOrBenchmark == "b"] <- "Undisturbed"
    err.action <- paste0("- Setting all bm plot disturbance codes to 'Undisturbed' and saving bad plot details to '", f.err, "'")
  } else if (plot.bm.disturbed.action == "delete") {
    # Delete bad plots from main df
    plot <- plot[!plot$plot_code %in% plot.bm.disturbed.plot_codes, ]
    err.action <- paste0(
      "- Deleting ", n.plot.bm.disturbed,
      " benchmark plots with disturbance code='Disturbed'\n",
      "- Saving bad plot details to '", f.err, "'"
    )
  } else {
    # plot.bm.disturbed.action=="none"
    err.action <- paste0("- No action taken; saving bad plot details to '", f.err, "'")
  }

  # Save the error message and action taken
  cat(err.action, "\n", sep = "")
  filename <- paste0(RESULTSDIR, f.err)
  write.csv(plot.bm.disturbed, file = filename, row.names = FALSE)
  df.err <- rbind(df.err, c(err.msg, err.action))
  err.msg <- ""
  err.action <- ""
} else {
  cat("done\n")
}

####################################
# Add disturbance & reclamation codes
####################################

cat("Adding disturbance & reclamation codes to land cover...")

# # Undisturbed (no disturbance qualifier)
# if ( dist.code.append.undisturbed==TRUE ) {
# 	plot$landCover[ plot$disturbance=="Undisturbed" ] <-
# 		paste0(
# 		plot$eg.seral[ plot$disturbance=="Undisturbed" ],
# 		" [Undisturbed]"
# 		)
# } else {
# 	plot$landCover[ plot$disturbance=="Undisturbed" ] <-
# 		plot$eg.seral[ plot$disturbance=="Undisturbed" ]
# }
#
# # Disturbed, not reclaimed
# plot$landCover[ !plot$disturbance=="Undisturbed" & plot$reclamation=="" ] <-
# 	paste0(
# 	plot$eg.seral[ !plot$disturbance=="Undisturbed" & plot$reclamation=="" ],
# 	" [",
# 	plot$disturbance[ !plot$disturbance=="Undisturbed" & plot$reclamation=="" ],
# 	"]"
# 	)
#
# # Disturbed, reclaimed
# plot$landCover[ !plot$disturbance=="Undisturbed" & !plot$reclamation=="" ] <-
# 	paste0(
# 	plot$eg.seral[ !plot$disturbance=="Undisturbed" & !plot$reclamation=="" ],
# 	" [",
# 	plot$disturbance[ !plot$disturbance=="Undisturbed" & !plot$reclamation=="" ],
# 	", ",
# 	plot$reclamation[ !plot$disturbance=="Undisturbed" & !plot$reclamation=="" ],
# 	"]"
# 	)

# New simple method
plot$landCover[plot$dist.rec == ""] <- plot$eg.seral[plot$dist.rec == ""]
plot$landCover[!plot$dist.rec == ""] <-
  paste0(plot$eg.seral[!plot$dist.rec == ""], " [", plot$dist.rec[!plot$dist.rec == ""], "]")
cat("done\n")

####################################
# Include seral stage codes in bm.veg if applicable
####################################

cat("Adding seral stage to forested benchmark vegetation...")

if (bm.forested.include.seral == TRUE) {
  plot$bm.veg <- plot$eg.seral
  cat("done\n")
} else {
  cat("SKIPPING\n")
}

####################################
# Append disturbance code '[Undisturbed]' to
# bm vegetation
####################################

if (dist.code.append.undisturbed == TRUE) {
  cat("Adding disturbance code '[Undisturbed]' to benchmark vegetation...")
  # Undisturbed (no disturbance qualifier)
  plot$bm.veg <-
    paste0(plot$bm.veg, " [Undisturbed]")
  cat("done\n")
}

##########################################
# Re-standardize landCover & bmveg codes
##########################################

if (use.lc.bm.short.codes == TRUE) {
  cat("Converting land cover and bm vegetation names to short codes:\n")

  cat("- bm.veg codes...")
  plot$bm.veg.orig <- plot$bm.veg
  plot <- plot %>%
    mutate(bm.veg = case_when(
      bm.veg == "Balsam Fir/Black Spruce Forest, Old" ~ "BF/BS Forest, Old",
      bm.veg == "Balsam Fir/Black Spruce Forest, Early-Mid" ~ "BF/BS Forest, Early-Mid",
      bm.veg == "Balsam Fir/Black Spruce Forest, Mature" ~ "BF/BS Forest, Mature",
      bm.veg == "Balsam Fir/Black Spruce Forest, Very Old" ~ "BF/BS Forest, Very Old",
      bm.veg == "Floodplain" ~ "Floodplain",
      bm.veg == "Scrub" ~ "Scrub",
      bm.veg == "Wetland" ~ "Wetland",
      bm.veg == "White Birch Forest, Mature" ~ "WB Forest, Mature",
      bm.veg == "White Birch Forest, Early-Mid" ~ "WB Forest, Early-Mid",
      bm.veg == "White Spruce Forest, Mature" ~ "WS Forest, Mature",
      TRUE ~ bm.veg
    ))
  cat("done\n")

  cat("- landCover codes...")
  plot$landCover.orig <- plot$landCover
  plot <- plot %>%
    mutate(landCover = case_when(
      landCover == "Balsam Fir/Black Spruce Forest, Early-Mid" ~ "BF/BS Forest, Early-Mid",
      landCover == "Balsam Fir/Black Spruce Forest, Early-Mid [Disturbed]" ~ "BF/BS Forest, Early-Mid [Disturbed/Logged]",
      landCover == "Balsam Fir/Black Spruce Forest, Early-Mid [Logging]" ~ "BF/BS Forest, Early-Mid [Disturbed/Logged]",
      landCover == "Balsam Fir/Black Spruce Forest, Early-Mid [Mining, Natural Regeneration]" ~ "BF/BS Forest, Early-Mid [Established Rec]",
      landCover == "Balsam Fir/Black Spruce Forest, Early-Mid [Established Rec]" ~ "BF/BS Forest, Early-Mid [Established Rec]",
      landCover == "Balsam Fir/Black Spruce Forest, Early-Mid [Mining, Reclaimed]" ~ "BF/BS Forest, Early-Mid [Reclaimed]",
      landCover == "Balsam Fir/Black Spruce Forest, Early-Mid [Reclaimed]" ~ "BF/BS Forest, Early-Mid [Reclaimed]",
      landCover == "Balsam Fir/Black Spruce Forest, Early-Mid [Reclaimed, Reclaimed]" ~ "BF/BS Forest, Early-Mid [Reclaimed]",
      landCover == "Balsam Fir/Black Spruce Forest, Mature" ~ "BF/BS Forest, Mature",
      landCover == "Balsam Fir/Black Spruce Forest, Mature [Logging]" ~ "BF/BS Forest, Mature [Disturbed/Logged]",
      landCover == "Balsam Fir/Black Spruce Forest, Mature [Disturbed]" ~ "BF/BS Forest, Mature [Disturbed/Logged]",
      landCover == "Balsam Fir/Black Spruce Forest, Old" ~ "BF/BS Forest, Old",
      landCover == "Balsam Fir/Black Spruce Forest, Old [Logging]" ~ "BF/BS Forest, Old [Disturbed/Logged]",
      landCover == "Balsam Fir/Black Spruce Forest, Old [Disturbed]" ~ "BF/BS Forest, Old [Disturbed/Logged]",
      landCover == "Balsam Fir/Black Spruce Forest, Very Old" ~ "BF/BS Forest, Very Old",
      landCover == "Floodplain" ~ "Floodplain - Active Channel",
      landCover == "Scrub [Disturbed]" ~ "Scrub [Disturbed/Logged]",
      landCover == "Scrub [Logging]" ~ "Scrub [Disturbed/Logged]",
      landCover == "Wetland" ~ "Wetland",
      landCover == "Wetland [Mining]" ~ "Wetland [Reclaimed]",
      landCover == "Wetland [Logging]" ~ "Wetland [Disturbed/Logged]",
      landCover == "Wetland [Disturbed]" ~ "Wetland [Disturbed/Logged]",
      landCover == "White Birch Forest, Early-Mid" ~ "WB Forest, Early-Mid",
      landCover == "White Birch Forest, Early-Mid [Disturbed]" ~ "WB Forest, Early-Mid [Disturbed/Logged]",
      landCover == "White Birch Forest, Early-Mid [Disturbed/Logged]" ~ "WB Forest, Early-Mid [Disturbed/Logged]",
      landCover == "White Birch Forest, Mature" ~ "WB Forest, Mature",
      landCover == "White Spruce Forest, Mature" ~ "WS Forest, Mature",
      TRUE ~ landCover
    ))
  cat("done\n")
}

####################################
# Create df lc.summary with landcover classes
# land plot sample sizes
####################################

cat("Creating land cover summary:\n")

cat("- Creating plot.meta...")
plot.meta <- plot[, meta.cols]
plot.meta$cnt <- 1
plot.meta[is.na(plot.meta)] <- "[unknown]" # allows grouping by NAs
cat("done\n")

cat("- Creating lc.summary...")
# Only use land cover classes with at least one focal plot
lc.summary <- aggregate(
  cnt ~ bm.veg + landCover + eg.type,
  data = plot.meta[plot.meta$focalOrBenchmark == "f", ],
  FUN = sum,
  na.rm = TRUE
)
names(lc.summary)[names(lc.summary) == "cnt"] <- "n.plots"
lc.summary <- lc.summary[, c("landCover", "bm.veg", "eg.type", "n.plots")]

# Count focal plots
lc.summary.f <- aggregate(
  cnt ~ landCover,
  data = plot.meta[plot.meta$focalOrBenchmark == "f", ],
  FUN = sum,
  na.rm = TRUE
)
names(lc.summary.f)[names(lc.summary.f) == "cnt"] <- "n.f.plots"
lc.summary.f <- lc.summary.f[, c("landCover", "n.f.plots")]

# Count bm plots
lc.summary.b <- aggregate(
  cnt ~ bm.veg,
  data = plot.meta[plot.meta$focalOrBenchmark == "b", ],
  FUN = sum,
  na.rm = TRUE
)
names(lc.summary.b)[names(lc.summary.b) == "cnt"] <- "n.b.plots"
lc.summary.b <- lc.summary.b[, c("bm.veg", "n.b.plots")]

# Add counts to main df
lc.summary <- merge(lc.summary, lc.summary.f, by = "landCover", all.x = TRUE)
lc.summary <- merge(lc.summary, lc.summary.b, by = "bm.veg", all.x = TRUE)
lc.summary$n.f.plots[is.na(lc.summary$n.f.plots)] <- 0
lc.summary$n.b.plots[is.na(lc.summary$n.b.plots)] <- 0

# Get rid of total plots, irrelvant and confusing
lc.summary <- subset(lc.summary, select = -c(n.plots))

lc.summary <- lc.summary[, c("landCover", "bm.veg", "eg.type", "n.f.plots", "n.b.plots")]
cat("done\n")

############################
# Add veg plot sample size stats
# to land cover summary
############################

cat("- Calculating plot sample sizes...")
# Make summary counts of plots in all land cover classes, including
# classes with either zero focal or zero benchmark plots
lc.summary.all <- sqldf("
SELECT f.`landCover`, b.`bm.veg`, `n.focal`, `n.bm`
FROM
(
SELECT `bm.veg`, COUNT(*) AS `n.bm`
FROM plot
WHERE focalOrBenchmark='b'
GROUP BY `bm.veg`
) AS b
LEFT JOIN
(
SELECT `landCover`, `bm.veg`, COUNT(*) AS `n.focal`
FROM plot
WHERE focalOrBenchmark='f'
GROUP BY `landCover`, `bm.veg`
) AS f
ON f.`bm.veg`=b.`bm.veg`
ORDER BY b.`bm.veg`, f.`landCover`
")

# Set NAs to 0
lc.summary.all$n.focal[is.na(lc.summary.all$n.focal)] <- 0
lc.summary.all$n.bm[is.na(lc.summary.all$n.bm)] <- 0
cat("done\n")

cat("- Adding column N.MIN.ABS...")
lc.summary.all$N.MIN.ABS <- N.MIN.ABS
cat("done\n")

cat("- Calculating `all.n.OK`...")
# Calculate all.n.OK
lc.summary.all$all.n.OK <- lc.summary.all$all.n.OK <- TRUE
lc.summary.all$all.n.OK[lc.summary.all$n.focal < N.MIN.ABS |
  lc.summary.all$n.bm < N.MIN.ABS] <- FALSE
cat("done\n")

cat("- Calculating veg plot sample sizes...")
veg.plot.meta <- plot.meta[, c(
  "plot_code", "focalOrBenchmark", "disturbance", "eg.type",
  "landCover", "bm.veg"
)]
veg <- merge(veg, veg.plot.meta, by = "plot_code", all.x = TRUE)

# Count plots with veg data
vegp <- as.data.frame(unique(veg[, c("plot_code", "focalOrBenchmark", "bm.veg", "landCover")]))
vegp[is.na(vegp)] <- "[unknown]"
vegp$cnt <- 1

# Focal veg plot sample sizes
vegp.lc.f <- aggregate(
  cnt ~ landCover,
  data = vegp[vegp$focalOrBenchmark == "f", ],
  FUN = sum,
  na.rm = TRUE
)
colnames(vegp.lc.f) <- c("landCover", "n.f.plots.veg")

# Benchmark veg plot sample sizes
vegp.lc.b <- aggregate(
  cnt ~ bm.veg,
  data = vegp[vegp$focalOrBenchmark == "b", ],
  FUN = sum,
  na.rm = TRUE
)
colnames(vegp.lc.b) <- c("bm.veg", "n.b.plots.veg")

# Merge counts into the main summary file
lc.summary <- merge(lc.summary, vegp.lc.f, by = "landCover", all.x = TRUE)
lc.summary <- merge(lc.summary, vegp.lc.b, by = "bm.veg", all.x = TRUE)
lc.summary$n.f.plots.veg[is.na(lc.summary$n.f.plots.veg)] <- 0
lc.summary$n.b.plots.veg[is.na(lc.summary$n.b.plots.veg)] <- 0
cat("done\n")

# Flag plots with f and/or b sample sizes less than N.MIN.ABS
cat("- Flagging land cover classes with f or b sample sizes<N.MIN.ABS...")
lc.summary$all.n.OK <- FALSE
lc.summary$all.n.OK[lc.summary$n.f.plots >= N.MIN.ABS & lc.summary$n.b.plots >= N.MIN.ABS] <- TRUE
lc.summary <- df.reorder(lc.summary, col.move = "bm.veg", col.before = "eg.type")
cat("done\n")

##########################################
##########################################
# Prepare input file data frames
##########################################
##########################################

cat("Preparing input file data frames:\n")

#######################################
# Land cover
# df: landCover
# For input file: landCover.csv
#######################################

cat("- Land cover (df landCover)...")
landCover <- lc.summary[, c("landCover", "eg.type", "bm.veg", "n.f.plots", "n.b.plots", "all.n.OK")]
names(landCover)[names(landCover) == "landCover"] <- "landCover"

# This application doesn't include area, so assign dummy values of 1 ha to each land cover class
landCover$area_ha <- 1

# Correct bm plot counts for land cover classes with zero plots
n.plots.bm.veg <- unique(landCover[landCover$n.b.plots > 0, c("bm.veg", "n.b.plots")])
colnames(n.plots.bm.veg) <- c("bm.veg", "n.b.plots.corrected")
landCover <- merge(landCover, n.plots.bm.veg, by = "bm.veg", all.x = TRUE)
landCover$n.b.plots <- landCover$n.b.plots.corrected
landCover <- landCover[, !names(landCover) %in% c("n.b.plots.corrected")]
landCover$n.b.plots[is.na(landCover$n.b.plots)] <- 0
landCover$n.f.plots[is.na(landCover$n.f.plots)] <- 0
landCover$all.n.OK[landCover$n.f.plots < N.MIN.ABS | landCover$n.b.plots < N.MIN.ABS] <- FALSE

# Rename and reorder
names(landCover)[names(landCover) == "bm.veg"] <- "vegClass"
names(landCover)[names(landCover) == "eg.type"] <- "lc.type"
names(landCover)[names(landCover) == "n.f.plots"] <- "focal_plots"
names(landCover)[names(landCover) == "n.b.plots"] <- "bm_plots"
landCover <- landCover[, c("landCover", "lc.type", "vegClass", "area_ha", "focal_plots", "bm_plots", "all.n.OK")]

cat("done\n")

#################
# Plot metadata
# df: plotMetadata
#################

cat("- Plot metadata (df plotMetadata)...")

plotMetadata <- unique(plot[, c("plot_code", "focalOrBenchmark", "bm.veg", "landCover")])

# Rename field
names(plotMetadata)[names(plotMetadata) == "plot_code"] <- "plotCode"
names(plotMetadata)[names(plotMetadata) == "bm.veg"] <- "vegClass"
names(plotMetadata)[names(plotMetadata) == "landCover"] <- "landCover"

cat("done\n")

#######################################
# Species attributes
# Creates df: species
# For input file: species.csv
#######################################

cat("- Species attributes (df species)...")

# Accepted species only
species <- df.spp.lookup[, c("ScientificName", "Classification", "Layer", "NativeStatus", "Invasive")]
names(species)[names(species) == "Classification"] <- "Habit"

species$is_exotic <- ""
species$growthForm <- ""
species$genus <- ""

# Populate is_exotic
species <- species %>%
  mutate(is_exotic = case_when(
    NativeStatus == "Native" ~ 0,
    NativeStatus == "Introduced" ~ 1,
    TRUE ~ NA
  ))
species$is_exotic[is.na(species$is_exotic)] <- 0

# Populate growthForm
species <- species %>%
  mutate(growthForm = case_when(
    Habit %in% c("Tree", "Tree/Shrubs") ~ "Tree",
    Habit %in% c("Herb", "Herbaceous") ~ "Herb",
    Habit %in% c("Liverwort", "Moss", "Lichen", "Moss/Lichen") ~ "Bryophytes",
    Habit %in% c("Shrub", "Dwarf Shrubs") ~ "Shrub",
    TRUE ~ NA
  ))

# Add id
species$id <- seq.int(nrow(species))
species$genus <- str_split_fixed(species$ScientificName, " ", 2)[, 1]
species <- species[, c("id", "genus", "ScientificName", "is_exotic", "growthForm")]
colnames(species) <- c("speciesID", "genus", "species", "is_exotic", "growthForm")

cat("done\n")


##########################################
# Cover per species per stratum
#
# Method: MAX species cover per stratum
# Handles edge case where multiple entries per species in a stratum
#
# Includes simplified stratum field and keys for aggregating by
# plot+stratum or plot+species
# df: speciesCoverByStratumAll
##########################################

cat("- Species cover by stratum (df speciesCoverByStratumAll):\n")

cat("-- Creating df speciesCoverByStratumAll...")
# Add composite key plot.species.layer
veg <- veg %>% select_if(!names(.) %in% c("plot.species.layer"))
veg$plot.species.layer <- paste0(veg$plot_code, "-", veg$Species, "-", veg$Layer)

# Aggregation in theory isn't necessary, but will remove
# erroneous duplicate plot x species x layer entries, if any
speciesCoverByStratumAll <- aggregate(
  CoverPercent ~ plot.species.layer,
  data = veg,
  FUN = max,
  na.rm = TRUE,
  na.pass = NULL
)

# Add plot code & stratum
speciesCoverByStratumAll <- merge(speciesCoverByStratumAll, veg[, c("plot_code", "plot.species.layer", "Species", "Layer")],
  by = "plot.species.layer", all.x = TRUE
)

# Add focalOrBenchmark
speciesCoverByStratumAll <- merge(speciesCoverByStratumAll, plot[, c("plot_code", "focalOrBenchmark")],
  by = "plot_code", all.x = TRUE
)

# Add species native status
if (spp.merge.by.latin == TRUE) {
  speciesCoverByStratumAll <- merge(speciesCoverByStratumAll, df.spp.lookup[, c("ScientificName", "CommonName", "NativeStatus")],
    by.x = "Species", by.y = "ScientificName", all.x = TRUE
  )
} else {
  # Merge by common name
  speciesCoverByStratumAll <- merge(speciesCoverByStratumAll, df.spp.lookup[, c("ScientificName", "CommonName", "NativeStatus")],
    by.x = "Species", by.y = "CommonName", all.x = TRUE
  )
}

# Populate is_exotic
speciesCoverByStratumAll$is_exotic <- 0
speciesCoverByStratumAll$is_exotic[speciesCoverByStratumAll$NativeStatus %in% c("Non-native", "Introduced")] <- 1
speciesCoverByStratumAll$is_exotic[is.na(speciesCoverByStratumAll$is_exotic)] <- 1

# Add landcover
speciesCoverByStratumAll <- merge(speciesCoverByStratumAll, plot[, c("plot_code", "landCover", "bm.veg")],
  by = "plot_code", all.x = TRUE
)

# Now make the df
# Use scientific name for species, regardless of how df was merged!
speciesCoverByStratumAll <- speciesCoverByStratumAll[
  ,
  c(
    "plot_code", "focalOrBenchmark", "landCover", "bm.veg", "Layer",
    "ScientificName", "is_exotic", "CoverPercent"
  )
]
colnames(speciesCoverByStratumAll) <- c(
  "plotCode", "focalOrBenchmark",
  "landCover", "vegClass", "stratum", "species", "is_exotic", "cover"
)

# Add key field plot.stratum
speciesCoverByStratumAll$plot.stratum <- paste0(
  speciesCoverByStratumAll$plotCode, "-", speciesCoverByStratumAll$stratum
)

# Add key field plot.species
speciesCoverByStratumAll$plot.species <- paste0(
  speciesCoverByStratumAll$plotCode, "-", speciesCoverByStratumAll$species
)

# Add simplified strata used for exotic species indicators: Bryophyte, herb, shrub, tree
# Current 9 layers is too many for exotic species, generates tons of zeroes
speciesCoverByStratumAll$stratum.simple <- NA
speciesCoverByStratumAll <- speciesCoverByStratumAll %>%
  mutate(stratum.simple = case_when(
    stratum %in% c("Herb_Dwarf", "Epiphyte", "Epiphytes", "Herb", "Herb/Dwarf Shrubs") ~ "Herb",
    stratum %in% c(
      "D_Soil", "Dw_Wood", "Moss/Lichens on Soil/Hummus", "Moss/Lichens on Soil/Humus",
      "Moss/Lichens on non-soil (wood/rock)", "Moss/Lichen"
    ) ~ "Bryophyte",
    stratum %in% c("Low_Shrub", "Tall_Shrub", "Low shrub (to 2 m)") ~ "Shrub",
    stratum %in% c(
      "Subcanopy_Trees", "Main_Canopy", "Main Canopy", "Tall shrub (2-10 m)",
      "Dominant_Trees", "Tree/Shrubs", "Tree (> 10 m)"
    ) ~ "Tree",
    TRUE ~ ""
  ))
speciesCoverByStratumAll$stratum.simple[
  is.na(speciesCoverByStratumAll$stratum.simple) |
    speciesCoverByStratumAll$stratum.simple == ""
] <- "[unknown]"

# Add key field plot.stratum.simple
speciesCoverByStratumAll$plot.stratum.simple <- paste0(
  speciesCoverByStratumAll$plotCode, "-", speciesCoverByStratumAll$stratum.simple
)

# Backup original plot.stratum column
speciesCoverByStratumAll$stratum.orig <- speciesCoverByStratumAll$stratum
speciesCoverByStratumAll$plot.stratum.orig <- speciesCoverByStratumAll$plot.stratum

cat("done\n")

cat("-- Adjusting stratum codes...")
if (use.stratum.simple) {
  cat("using stratum.simple...")
  speciesCoverByStratumAll$stratum <- speciesCoverByStratumAll$stratum.simple
  speciesCoverByStratumAll$plot.stratum <- speciesCoverByStratumAll$plot.stratum.simple
} else {
  cat("keeping verbatim codes...")
}
cat("done\n")

##########################################
# Cover per species per plot
#
# Species cover aggregated by plot.
# Cover is MAX cover across all strata in
# which species present.
# Includes is_exotic flag.
# df: speciesCover
##########################################

cat("- Species cover per plot (df speciesCover)...")

speciesCoverAll <- unique(speciesCoverByStratumAll[
  ,
  c(
    "plotCode", "plot.species", "focalOrBenchmark", "landCover", "vegClass",
    "species", "is_exotic"
  )
])
plot.species.cover <- aggregate(
  cover ~ plot.species,
  data = speciesCoverByStratumAll,
  FUN = max,
  na.rm = TRUE,
  na.pass = NULL
)
speciesCoverAll <- merge(speciesCoverAll, plot.species.cover, by = "plot.species", all.x = TRUE)
speciesCoverAll$cover[is.na(speciesCoverAll$cover)] <- 0

# Make the final dfs
speciesCoverAll <- speciesCoverAll[
  ,
  c(
    "plotCode", "focalOrBenchmark", "landCover", "vegClass",
    "species", "is_exotic", "cover"
  )
]

speciesCover <- speciesCoverAll

# Just in case
speciesCover <- speciesCover[!is.na(speciesCover$focalOrBenchmark), ]

# Make species cover df
names(speciesCover)[names(speciesCover) == "plot_code"] <- "plotCode"
names(speciesCover)[names(speciesCover) == "maxcov"] <- "cover"
names(speciesCover)[names(speciesCover) == "landCover"] <- "landCover"
names(speciesCover)[names(speciesCover) == "bm.veg"] <- "vegClass"

# Convert percent to proportions if values detected outside range [0:1]
cover.vals <- unique(speciesCover$cover)

if (min(cover.vals) < 0 || max(cover.vals > 1)) {
  cat("WARNING: converting 'cover' from percent to proportions...")
  speciesCover$cover <- speciesCover$cover / 100
}

# Remove unwanted columns
speciesCover <- speciesCover[
  ,
  c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "species", "is_exotic", "cover")
]

cat("done\n")

################################################
# Percent cover exotic species by stratum
#
# Cover of exotic species in *simplified* strata
# All individuals of exotic species aggregated
# by stratum, regardless of species
# df: exoticCoverByStratum
#
# NOTE 1:
# Cover is "Total cover" of all species
# SUMMED in each layer.
# Total cover >100% is truncated at 100%
#
# NOTE 2:
# Should be modified to calculate cover in two steps:
# (1) stratum.cover=sum(cover),
# 		aggregating by plot.stratum
# (2) stratum.simple.cover=max(stratum.cover),
# 		aggregating by plot.stratum.simple
################################################

cat("- Total cover exotic species by stratum (df exoticCoverByStratum):\n")

cat("-- Creating df speciesCoverByStratumAllExotic...")

# Cover: Exclude native species and all bryophytes
speciesCoverByStratumAllExotic <- speciesCoverByStratumAll[
  speciesCoverByStratumAll$is_exotic == 1,
]
speciesCoverByStratumAllExotic <- speciesCoverByStratumAllExotic[
  !speciesCoverByStratumAllExotic$plot.stratum.simple == "Bryophyte",
]
cat("done\n")

cat("-- Aggregating to simplified strata...")
# Plot+strata: exclude bryophyte stratum
plot.stratum.simple.cover.exotic <- aggregate(
  cover ~ plot.stratum.simple,
  data = speciesCoverByStratumAllExotic,
  FUN = sum,
  na.rm = TRUE,
  na.pass = NULL
)
cat("done\n")

cat("-- Creating exoticCoverByStratum...")
exoticCoverByStratum <- unique(speciesCoverByStratumAll[
  !speciesCoverByStratumAll$plot.stratum.simple == "Bryophyte",
  c("plotCode", "stratum.simple", "plot.stratum.simple", "focalOrBenchmark", "landCover", "vegClass")
])
exoticCoverByStratum <- merge(
  exoticCoverByStratum, plot.stratum.simple.cover.exotic,
  by = "plot.stratum.simple", all.x = TRUE
)
exoticCoverByStratum$cover[is.na(exoticCoverByStratum$cover)] <- 0

# Remove any NA rows
exoticCoverByStratum <- exoticCoverByStratum[
  !is.na(exoticCoverByStratum$focalOrBenchmark),
]

# Remove any bryophytes rows
# Shouldn't be there, but just in case
exoticCoverByStratum <- exoticCoverByStratum[
  !exoticCoverByStratum$stratum.simple == "Bryophyte",
]

# Truncate values>100 to 100
exoticCoverByStratum$cover[exoticCoverByStratum$cover > 100] <- 100

# Set final column names
names(exoticCoverByStratum)[names(exoticCoverByStratum) == "stratum.simple"] <- "stratum"

# Convert percent to proportions if values detected outside range [0:1]
cover.vals <- unique(exoticCoverByStratum$cover)
if (min(cover.vals) < 0 || max(cover.vals > 1)) {
  cat("WARNING: percent detected...converting 'cover' from percent to proportions...")
  exoticCoverByStratum$cover <- exoticCoverByStratum$cover / 100
}

exoticCoverByStratum <- exoticCoverByStratum[
  ,
  c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "cover")
]

# Tidy up
rm(list = c("speciesCoverByStratumAllExotic", "plot.stratum.simple.cover.exotic"))
cat("done\n")

################################################
# Percent cover by stratum (=growth form or layer)
#
# Cover aggregated by stratum ("Layer" in original data)
# df: coverByStratum
#
# NOTE:
# Cover is "Total cover" of all species SUMMED
# for each layer.
# Total cover >100% is truncated to 100%
################################################

cat("- Cover per stratum / growth form (df coverByStratum)...")

# Summed cover by plot+strata
plot.stratum.cover <- aggregate(
  cover ~ plot.stratum,
  data = speciesCoverByStratumAll,
  FUN = sum,
  na.rm = TRUE,
  na.pass = NULL
)
coverByStratum <- unique(speciesCoverByStratumAll[
  ,
  c("plotCode", "stratum", "plot.stratum", "focalOrBenchmark", "landCover", "vegClass")
])
coverByStratum <- merge(
  coverByStratum, plot.stratum.cover,
  by = "plot.stratum", all.x = TRUE
)
coverByStratum$cover[is.na(coverByStratum$cover)] <- 0
coverByStratum <- coverByStratum[!is.na(coverByStratum$focalOrBenchmark), ]

# Truncate values>100 to 100
coverByStratum$cover[coverByStratum$cover > 100] <- 100

# Set final names
names(coverByStratum)[names(coverByStratum) == "cover"] <- "cover"

# Convert percent to proportions if values detected outside range [0:1]
cover.vals <- unique(coverByStratum$cover)
if (min(cover.vals) < 0 || max(cover.vals > 1)) {
  cat("WARNING: percent detected...converting 'cover' from percent to proportions...")
  coverByStratum$cover <- coverByStratum$cover / 100
}

# Remove unneeded columns
coverByStratum <- coverByStratum[
  ,
  c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "cover")
]

# Copy to the data frame named as expected by import.R
coverByGrowthForm <- coverByStratum

# Tidy up
rm(list = c("plot.stratum.cover"))

cat("done\n")

################################################
# Ground cover
# Used by indicators: GC
# Input file: groundCover.csv
# Fields:
# 		plotCode, focalOrBenchmark, landCover,
# 	vegClass, stratum, cover
################################################

cat("- Ground cover (df groundCover):\n")

gc.raw <- plot[, c(
  "plot_code",
  "SubstrateDecWood",
  "SubstrateBedrock",
  "SubstrateRocks",
  "SubstrateMineralSoil",
  "SubstrateOrganicMatter",
  "SubstrateWater"
)]

# Check for -999 in one or more ground cover indicators
cat("-- Checking for missing values (-999) in raw data...")

gc.raw$bad <- FALSE
bad.row <- apply(gc.raw, 1, function(x) any(x == -999))
gc.raw$bad[bad.row] <- TRUE
n.bad.rows <- length(gc.raw$bad[bad.row == TRUE])

if (n.bad.rows > 0) {
  err.msg <- paste0(n.bad.rows, " rows contain -999 for ground cover indicators")
  cat("\n-- WARNING: ", err.msg, "\n", sep = "")

  if (gc.missing.val.delete) {
    # Delete rows with one or more values of -999
    gc.raw <- gc.raw[!bad.row, ]
    err.action <- paste0("ACTION: deleted ", n.bad.rows, " rows with -999 for ground cover indicators")
    cat("-- ", err.action, "\n", sep = "")
  } else {
    # Set -999 values to 0
    gc.raw[gc.raw == -999] <- 0
    err.action <- paste0("ACTION: set -999 to 0 in ", n.bad.rows, " rows with percent cover")
    cat("-- ", err.action, "\n", sep = "")
  }

  df.err <- rbind(df.err, c(err.msg, err.action))
  err.msg <- ""
  err.action <- ""
} else {
  cat("done\n")
}

cat("-- Extracting ground cover columns to separate rows...")
gc.decwood <- gc.raw[, c("plot_code", "SubstrateDecWood")]
gc.decwood$stratum <- "SubstrateDecWood"
names(gc.decwood)[names(gc.decwood) == "SubstrateDecWood"] <- "perc.cover"

gc.bedrock <- gc.raw[, c("plot_code", "SubstrateBedrock")]
gc.bedrock$stratum <- "SubstrateBedrock"
names(gc.bedrock)[names(gc.bedrock) == "SubstrateBedrock"] <- "perc.cover"

gc.rocks <- gc.raw[, c("plot_code", "SubstrateRocks")]
gc.rocks$stratum <- "SubstrateRocks"
names(gc.rocks)[names(gc.rocks) == "SubstrateRocks"] <- "perc.cover"

gc.mineralsoil <- gc.raw[, c("plot_code", "SubstrateMineralSoil")]
gc.mineralsoil$stratum <- "SubstrateMineralSoil"
names(gc.mineralsoil)[names(gc.mineralsoil) == "SubstrateMineralSoil"] <- "perc.cover"

gc.organicmatter <- gc.raw[, c("plot_code", "SubstrateOrganicMatter")]
gc.organicmatter$stratum <- "SubstrateOrganicMatter"
names(gc.organicmatter)[names(gc.organicmatter) == "SubstrateOrganicMatter"] <- "perc.cover"

gc.water <- gc.raw[, c("plot_code", "SubstrateWater")]
gc.water$stratum <- "SubstrateWater"
names(gc.water)[names(gc.water) == "SubstrateWater"] <- "perc.cover"

gc <- rbind(gc.decwood, gc.bedrock, gc.rocks, gc.mineralsoil, gc.organicmatter, gc.water)
cat("done\n")

cat("-- Merging with plot metadata...")
gc.rows.before <- nrow(gc)

# Merge with inner join (loss of rows is an error)
gc <- merge(gc, plotMetadata, by.x = "plot_code", by.y = "plotCode")
gc.rows.after <- nrow(gc)
gc.rows.diff <- gc.rows.before - gc.rows.after
if (gc.rows.diff > 0) {
  err.msg <- paste0("\n-- WARNING: ", gc.rows.diff, " rows lost from gc during merge with polot.metadata.")
  err.action <- "[No action]"
  cat(err.msg, "\n")
  df.err <- rbind(df.err, c(err.msg, err.action))
  err.msg <- ""
  err.action <- ""
} else {
  cat("done\n")
}

# Converting NA cover to 0% cover
# 0 OK for this EI
cat("-- Checking for 0 or NA cover...")
gc.na <- which(
  is.na(gc$perc.cover)
)
rows.gc.na <- nrow(gc[gc.na, ])

if (rows.gc.na > 0) {
  err.msg <- paste0(rows.gc.na, " out of ", gc.rows.after, " rows are NA or zero in data from ground cover indicators")
  cat("\n-- WARNING: ", err.msg, "\n", sep = "")

  if (gc.na.delete == TRUE) {
    gc <- gc[-gc.na, ]
    err.action <- paste0("ACTION: deleted ", rows.gc.na, " rows with missing cover")
    cat("-- ", err.action, "\n", sep = "")
  } else {
    gc$perc.cover[gc.na] <- 0
    err.action <- paste0("ACTION: ", rows.gc.na, " rows with missing cover set to zero cover")
    cat("-- ", err.action, "\n", sep = "")
  }
  df.err <- rbind(df.err, c(err.msg, err.action))
  err.msg <- ""
  err.action <- ""
} else {
  cat("done: no all-NA values of perc.cover\n")
}

# Just in case
gc <- gc[!is.na(gc$focalOrBenchmark), ]

# Make final data frame
cat("-- Saving final data frame...")
groundCover <- gc
names(groundCover)[names(groundCover) == "plot_code"] <- "plotCode"
names(groundCover)[names(groundCover) == "landCover"] <- "landCover"
names(groundCover)[names(groundCover) == "bm.veg"] <- "vegClass"
names(groundCover)[names(groundCover) == "perc.cover"] <- "cover"

# Convert percent to proportions if values detected outside range [0:1]
cover.vals <- unique(groundCover$cover)
if (min(cover.vals) < 0 || max(cover.vals > 1)) {
  cat("WARNING: percent detected...converting 'cover' from percent to proportions...")
  groundCover$cover <- groundCover$cover / 100
}

# Save final data frame, relevant fields only
groundCover <- groundCover[
  ,
  c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "cover")
]

# Tidy up
rm(list = c(
  "gc", "gc.decwood", "gc.bedrock", "gc.rocks", "gc.mineralsoil", "gc.organicmatter"
))

cat("done\n")

############################################
# Prepare detailed sample size summary
#
# Make detailed summary by land cover class of actual, final
# sample sizes for each indicator, plus new determination of
# land  cover classes with n>N.MIN.ABS for all indicators
############################################

cat("Preparing detailed land cover summary table (lc.summary.detailed):\n")

cat("-- Preparing table based on lc.summary...")

lc.summary.detailed <- lc.summary[
  ,
  c("bm.veg", "landCover", "eg.type", "n.f.plots", "n.b.plots")
]
plot.meta.fb <- plotMetadata[, c("plotCode", "focalOrBenchmark", "vegClass", "landCover")]
colnames(plot.meta.fb) <- c("plotCode", "focalOrBenchmark", "bm.veg", "landCover")

lc.detailed.update <- function(lc.summary.detailed, plot.meta.fb, ind.plots, col.n.f, col.n.b) {
  colnames(ind.plots) <- "plotCode"
  ind.plots <- merge(ind.plots, plot.meta.fb, by = "plotCode")
  ind.plots$plots <- 1
  lc.n.f <- aggregate(
    plots ~ landCover,
    data = ind.plots[ind.plots$focalOrBenchmark == "f", ],
    FUN = sum
  )
  lc.n.b <- aggregate(
    plots ~ bm.veg,
    data = ind.plots[ind.plots$focalOrBenchmark == "b", ],
    FUN = sum
  )
  colnames(lc.n.f) <- c("landCover", col.n.f)
  colnames(lc.n.b) <- c("bm.veg", col.n.b)
  lc.summary.detailed <- merge(lc.summary.detailed, lc.n.f, by = "landCover", all.x = TRUE)
  lc.summary.detailed <- merge(lc.summary.detailed, lc.n.b, by = "bm.veg", all.x = TRUE)
  cmd <- paste0("lc.summary.detailed[ is.na(lc.summary.detailed$", col.n.f, "), c('", col.n.f, "') ] <- 0")
  eval(parse(text = cmd))
  cmd <- paste0("lc.summary.detailed[ is.na(lc.summary.detailed$", col.n.b, "), c('", col.n.b, "') ] <- 0")
  eval(parse(text = cmd))
  return(lc.summary.detailed)
}

cat("done\n")

#
# Add indicator-specific sample sizes
#

cat("-- Adding indicator-specific sample sizes...")

# Species Richness
col.n.f <- "n.f.SR"
col.n.b <- "n.b.SR"
ind.plots <- as.data.frame(unique(speciesCover$plotCode))
lc.summary.detailed <- lc.detailed.update(
  lc.summary.detailed,
  plot.meta.fb, ind.plots, col.n.f, col.n.b
)

# TD
col.n.f <- "n.f.TD"
col.n.b <- "n.b.TD"
ind.plots <- as.data.frame(unique(speciesCover$plotCode))
lc.summary.detailed <- lc.detailed.update(
  lc.summary.detailed,
  plot.meta.fb, ind.plots, col.n.f, col.n.b
)

# PCESS
col.n.f <- "n.f.PCESS"
col.n.b <- "n.b.PCESS"
ind.plots <- as.data.frame(unique(exoticCoverByStratum$plotCode))
lc.summary.detailed <- lc.detailed.update(
  lc.summary.detailed,
  plot.meta.fb, ind.plots, col.n.f, col.n.b
)

# PCGF
col.n.f <- "n.f.PCGF"
col.n.b <- "n.b.PCGF"
ind.plots <- as.data.frame(unique(coverByStratum$plotCode))
lc.summary.detailed <- lc.detailed.update(
  lc.summary.detailed,
  plot.meta.fb, ind.plots, col.n.f, col.n.b
)

# GC
col.n.f <- "n.f.GC"
col.n.b <- "n.b.GC"
ind.plots <- as.data.frame(unique(groundCover$plotCode))
lc.summary.detailed <- lc.detailed.update(
  lc.summary.detailed,
  plot.meta.fb, ind.plots, col.n.f, col.n.b
)

cat("done\n")

############################
# Summarize all sample sizes relative to N.MIN.ABS
############################

cat("-- Evaluating sample sizes relative to N.MIN.ABS...")

lc.summary.detailed$all.n.OK <- TRUE
lc.summary.detailed$all.n.OK[
  lc.summary.detailed$n.f.SR < N.MIN.ABS |
    lc.summary.detailed$n.b.SR < N.MIN.ABS |
    lc.summary.detailed$n.f.TD < N.MIN.ABS |
    lc.summary.detailed$n.b.TD < N.MIN.ABS |
    lc.summary.detailed$n.f.PCESS < N.MIN.ABS |
    lc.summary.detailed$n.b.PCESS < N.MIN.ABS |
    lc.summary.detailed$n.f.PCGF < N.MIN.ABS |
    lc.summary.detailed$n.b.PCGF < N.MIN.ABS |
    lc.summary.detailed$n.f.GC < N.MIN.ABS |
    lc.summary.detailed$n.b.GC < N.MIN.ABS
] <- FALSE
cat("done\n")

######################################
# Reorder and rename land cover summary tables
######################################

cat("-- Reordering and renaming land cover summary tables...")

# lc.summary for internal use, does not require fancy column names
# Update all.n.OK to more accurate version in lc.summary.detailed
names(lc.summary)[names(lc.summary) == "all.n.OK"] <- "all.n.OK.orig"
lc.summary.detailed.all.n.OK <- lc.summary.detailed[, c("landCover", "all.n.OK")]
lc.summary <- merge(lc.summary, lc.summary.detailed.all.n.OK,
  by = "landCover", all.x = TRUE
)
lc.summary <- df.reorder(lc.summary, col.move = "landCover", move.first = TRUE)
# Adding dummy column "ha" for compatibility with existing scripts
lc.summary$ha <- 100
lc.summary <- within(lc.summary, rm(all.n.OK.orig))

# lc.summary gets fancy column names
lc.summary.detailed <- subset(lc.summary.detailed, select = -c(eg.type))
lc.summary.detailed <- df.reorder(lc.summary.detailed, col.move = "landCover", move.first = TRUE)
lc.summary.detailed <- df.reorder(lc.summary.detailed,
  col.move = "all.n.OK",
  col.before = "bm.veg"
)
colnames(lc.summary.detailed) <- c(
  "Land cover class", "Benchmark vegetation",
  "all.n.OK", "n.f.plots", "n.b.plots", "n.f.SR", "n.b.SR",
  "n.f.TD", "n.b.TD", "n.f.PCESS", "n.b.PCESS", "n.f.PCGF", "n.b.PCGF", "n.f.GC", "n.b.GC"
)

cat("done\n")

# ######################################
# # Prepare & save final error report file
# ######################################
#
# cat("Preparing final error report file...")
# df.err[is.na(df.err)] <- ""
# df.err <- df.err[ !(df.err$message=="" & df.err$action==""), ]
# df.err$error.id <- seq(1:nrow(df.err))
# df.err <- df.reorder( df.err, col.move="error.id", move.first=TRUE)
# cat("done\n")
#
# filename <- paste0(RESULTSDIR, "import.errors.csv")
# cat("Saving df.err as error summary file '", filename, "'...", sep="")
# write.table( df.err, file=filename, sep=",",  row.names=FALSE )
# cat("done\n")

######################################
# Some last standardizations
######################################

# Remove is_exotic from speciesCover
speciesCover <- speciesCover[
  ,
  c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "species", "cover")
]

############################################
############################################
# End of script
############################################
############################################

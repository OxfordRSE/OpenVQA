##################################################################
# Convert vqa summary files to Excel workbook with three sheets
#
# Sourced by vqa.summary.R
# Requires package "xlsx"
##################################################################

##########################################
# Check required dependencies loaded.
# Assumes params file loaded.
##########################################

require(openxlsx2)
if (!exists("functions.loaded", mode = "function")) source(paste0(SRCDIR, "includes/functions.R"))

##########################################
# Set local parameters
##########################################

# Input files
file.q.overall.bm <- paste0(RESULTSDIR, SUMMARY.FOCAL.BM.FILE)
file.q.overall <- paste0(RESULTSDIR, SUMMARY.FOCAL.FILE)
file.q.fg <- paste0(RESULTSDIR, SUMMARY.FOCAL.FG.FILE)
file.q.ei <- paste0(RESULTSDIR, SUMMARY.FOCAL.EI.FILE)
file.ei.stratum.include <- paste0(INPUTDIR, BLACKLIST.FILE)
file.ei.stratum.veg.include <- paste0(INPUTDIR, WHITELIST.FILE)
file.lc <- paste0(INPUTDIR, LANDCOVER.FILE)
file.veg <- paste0(INPUTDIR, VEG.FILE)

# Output file
file.q.xlsx.out <- paste0(RESULTSDIR, FNAME.Q.XLSX.OUT)

# Re-declaring here for backwards compatibility
if (!exists("VQA.SUMMARY.PLURALIZE.STRATA")) VQA.SUMMARY.PLURALIZE.STRATA <- FALSE

##########################################
# Begin
##########################################

cat("******************************************\n")
cat("Exporting VQA results to Excel\n\n")
cat("Importing and restructuring CSV summary files:\n")

##########################################
# Overall quality results
##########################################

cat("  ", basename(file.q.overall), "...", sep = "")
q.overall <- read.csv(file.q.overall, header = T)

if (QH.OMIT == TRUE) {
  # Drop area and quality hectares columns
  drop.cols <- c("area_ha", "qh", "qh.lcl", "qh.ucl")
  q.overall <- q.overall[, !names(q.overall) %in% c(drop.cols)]
}

if (VQA.XLS.DROP.NOTES == TRUE) {
  q.overall <- q.overall[, !names(q.overall) == "notes"]
}


if (VQA.XLS.REMOVE.Q.NA == TRUE) {
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
for (i in 1:nrow(q.overall)) {
  q.overall$q[i] <- specify_decimal(as.numeric(q.overall$q[i]), 2)
  q.overall$q.lcl[i] <- specify_decimal(as.numeric(q.overall$q.lcl[i]), 2)
  q.overall$q.ucl[i] <- specify_decimal(as.numeric(q.overall$q.ucl[i]), 2)
  q.overall$mdes[i] <- specify_decimal(as.numeric(q.overall$mdes[i]), 2)
}

# Set precision for area and quality hectares if applicable
if (QH.OMIT == FALSE) {
  for (i in 1:nrow(q.overall)) {
    q.overall$area_ha[i] <- specify_decimal(as.numeric(q.overall$area_ha[i]), 1)
    q.overall$qh[i] <- specify_decimal(as.numeric(q.overall$qh[i]), 1)
    q.overall$qh.lcl[i] <- specify_decimal(as.numeric(q.overall$qh.lcl[i]), 1)
    q.overall$qh.ucl[i] <- specify_decimal(as.numeric(q.overall$qh.ucl[i]), 1)
  }
}

cat("done\n")

##########################################
# Benchmark vegetation quality results
##########################################

if (QH.OMIT == FALSE) {
  cat("  ", basename(file.qh.bm), "...", sep = "")
  qh.bm <- read.csv(file.qh.bm, header = T)

  if (VQA.XLS.DROP.NOTES == TRUE) {
    qh.bm <- qh.bm[, !names(qh.bm) == "notes"]
  }

  # Set precision for area and quality hectares if applicable
  for (i in 1:nrow(qh.bm)) {
    qh.bm$area_ha[i] <- specify_decimal(as.numeric(qh.bm$area_ha[i]), 1)
    qh.bm$qh[i] <- specify_decimal(as.numeric(qh.bm$qh[i]), 1)
    qh.bm$qh.lcl[i] <- specify_decimal(as.numeric(qh.bm$qh.lcl[i]), 1)
    qh.bm$qh.ucl[i] <- specify_decimal(as.numeric(qh.bm$qh.ucl[i]), 1)
  }

  cat("done\n")
}

##########################################
# Fuctional group quality results
##########################################

cat("  ", basename(file.q.fg), "...", sep = "")
q.fg <- read.csv(file.q.fg, header = T)

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
for (i in 1:nrow(q.fg)) {
  q.fg$q[i] <- specify_decimal(as.numeric(q.fg$q[i]), 2)
  q.fg$q.lcl[i] <- specify_decimal(as.numeric(q.fg$q.lcl[i]), 2)
  q.fg$q.ucl[i] <- specify_decimal(as.numeric(q.fg$q.ucl[i]), 2)
  q.fg$mdes[i] <- specify_decimal(as.numeric(q.fg$mdes[i]), 2)
}

cat("done\n")

##########################################
# Indicator quality results
##########################################

cat("  ", basename(file.q.ei), "...", sep = "")
q.ei <- read.csv(file.q.ei, header = T)

# Drop unwanted columns
drop.cols <- c("lc.ig", "lc.fg")
q.ei <- q.ei[, !(names(q.ei) %in% drop.cols)]

# Drop excluded indicator + column "include", if requested
if (VQA.XLS.DROP.INCLUDE.FALSE == TRUE) {
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
for (i in 1:nrow(q.ei)) {
  q.ei$q[i] <- specify_decimal(as.numeric(q.ei$q[i]), 2)
  q.ei$q.lcl[i] <- specify_decimal(as.numeric(q.ei$q.lcl[i]), 2)
  q.ei$q.ucl[i] <- specify_decimal(as.numeric(q.ei$q.ucl[i]), 2)
  q.ei$mdes[i] <- specify_decimal(as.numeric(q.ei$mdes[i]), 2)
}

cat("done\n")

##########################################
# Metadata
##########################################

cat("Preparing metadata...")

# Get latest analysis date/time from one of the CSVs
latest.run <- file.info(file.q.overall)$mtime
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
    value = c(PROJ, ASSESS, latest.run),
    stringsAsFactors = FALSE
  )
)
cat("done\n")

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
  paste0("RAW_PLOTDATA_FILENAME|Raw data|", RAW_PLOTDATA_FILENAME, "|Raw plot data file"),
  paste0("RAW_SPP_FILENAME|Raw data|", RAW_PLOTDATA_FILENAME, "|Raw species attribute file "),
  paste0("N.MIN.ABS|Data validation|", N.MIN.ABS, "|Minimum plots to run VQA for land cover class"),
  paste0("REMOVE.TD.OUTLIERS|Indicator options|", REMOVE.TD.OUTLIERS, "|Remove TD outliers?"),
  paste0("IS.EXOTIC.MISSING.ASSUME.NATIVE|Indicator options|", IS.EXOTIC.MISSING.ASSUME.NATIVE, "| "),
  paste0("Include no-data plots|Model settings|", INCLUDE.PLOTS.NODATA, "|Include plots with no data, assuming indicator value=0"),
  paste0("Q.FG.GROUP.BY|Model settings|", Q.FG.GROUP.BY, "|Calculate Q.fg as mean of indicator qualities or indicator group qualities? "),
  paste0("Q.FG.METHOD|Model settings|", Q.FG.METHOD, "|Type of mean used to calculate Q.fg"),
  paste0("Q.OVERALL.METHOD|Model settings|", Q.OVERALL.METHOD, "|Type of mean used to calculate Q.overall"),
  paste0("QH.METHOD|Model settings|", qh.method.disp, "|Quality Hectares method"),
  paste0("boot.reps|Model settings|", boot.reps, "|Number of bootstrap replicates")
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

##########################################
# Indicators
##########################################

cat("Preparing indicator summary:\n")

cat("  Loading parameters for indicator groups...")
df.ind <- df.EI.FG # Original indicator df, from params file

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
  curr.ei.params <- ei.params(curr.ei)

  # Load indicator parameters
  df.ind$ind.name[df.ind$EI == curr.ei] <- curr.ei.params$ei.name # Temporary
  df.ind$ind.group[df.ind$EI == curr.ei] <- curr.ei.params$ei.name
  df.ind$has.strata[df.ind$EI == curr.ei] <- curr.ei.params$has.stratum
  df.ind$distn[df.ind$EI == curr.ei] <- curr.ei.params$distn
  df.ind$q.method[df.ind$EI == curr.ei] <- curr.ei.params$q.method
  df.ind$test.tail[df.ind$EI == curr.ei] <- curr.ei.params$test.tail

  if (!is.na(curr.ei.params$bm.val)) {
    df.ind$bm.val[df.ind$EI == curr.ei] <- curr.ei.params$bm.val
  }
}

# Set column order
df.ind <- df.ind[, c("EI", "ind.group", "ind.name", "f.group", "has.strata", "stratum", "distn", "q.method", "test.tail", "bm.val")]
cat("done\n")

# Load ei.stratum.include file (="blacklist file")
cat("  Loading ei.stratum.include file...")
df.strata <- read.csv(file.ei.stratum.include, header = T)
# df.strata <- df.strata[ df.strata$include==TRUE & !df.strata$stratum=='nostrata', ]
df.strata <- df.strata[df.strata$include == TRUE, ]
if (pcess.herbs.only == TRUE) {
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
df.strata.wl <- read.csv(file.ei.stratum.veg.include, header = T)
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

if (VQA.SUMMARY.PLURALIZE.STRATA == TRUE) {
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

  if (VQA.SUMMARY.PLURALIZE.STRATA == TRUE) {
    # Make selected indicators plural
    if (curr.ei %in% c("PCGF", "PCESS")) {
      for (j in 1:nrow(df.ind.new.rows)) {
        df.ind.new.rows$stratum[j] <- capFirst(pluralize(tolower(df.ind.new.rows$stratum[j])))
      }
    }
  }
  df.ind.new.rows$ind.name <- paste0(df.ind.new.rows$ind.name, df.ind.new.rows$stratum)

  # Set column order of new rows, append to df.ind and drop old row
  df.ind.new.rows <- df.ind.new.rows[, c("EI", "ind.group", "ind.name", "f.group", "has.strata", "stratum", "distn", "q.method", "test.tail", "bm.val")]

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
df.ind <- df.ind[, c("ind.name", "ind.group", "f.group", "distn", "q.method", "test.tail", "bm.val")]

cat("done\n")

##########################################
# Land cover
##########################################

cat("Preparing land cover summary:\n")

cat("  Loading input file '", file.lc, "'...")
df.lc <- read.csv(file.lc, header = T)
if (file.exists(file.veg)) df.veg <- read.csv(file.veg, header = T)
cat("done\n")

if (exists("df.veg")) {
  cat("  Adding bm vegetation full names...")
  df.lc <- merge(df.lc, df.veg[, c("vegClass", "vegName")], by = "vegClass", all.x = TRUE)
  df.lc <- df.reorder(df.lc, col.move = "vegName", col.before = "vegClass")
  cat("done\n")
}
df.lc <- df.reorder(df.lc, col.move = "landCover", move.first = TRUE)

if (QH.OMIT == TRUE && "area_ha" %in% colnames(df.lc)) {
  df.lc <- df.lc[, !(names(df.lc) == "area_ha")]
}

cat("  Setting NAs to zero...")
# Set NAs to zero
if ("area_ha" %in% colnames(df.lc) && any(is.na(df.lc$area_ha))) df.lc$area_ha[is.na(df.lc$area_ha)] <- 0
if (any(is.na(df.lc$focal_plots))) df.lc$focal_plots[is.na(df.lc$focal_plots)] <- 0
if (any(is.na(df.lc$bm_plots))) df.lc$bm_plots[is.na(df.lc$bm_plots)] <- 0
cat("done\n")

# Set precision for area if applicable
if ("area_ha" %in% colnames(df.lc)) {
  cat("  Setting area_ha precision...")
  for (i in 1:nrow(df.lc)) {
    df.lc$area_ha[i] <- specify_decimal(as.numeric(df.lc$area_ha[i]), 1)
  }
  cat("done\n")
}

cat("  Setting final sort order...")
df.lc <- df.lc[order(df.lc$vegClass, df.lc$landCover), ]
cat("done\n")

#####################################################
# Make data frames more Excel-friendly
#####################################################

cat("Making data frames Excel-friendly:\n")

cat("  Renaming data frames...")
# Prefix dfs to distinguish them from sheet objects
df.q.ei <- q.ei
df.q.fg <- q.fg
df.q.overall <- q.overall
df.qh.bm <- qh.bm
rm(q.ei, q.fg, q.overall, qh.bm)
cat("done\n")

cat("  Combining quality scores and confidences limis in single column...")
df.q.ei$q.full <- paste0(df.q.ei$q, " [", df.q.ei$q.lcl, ", ", df.q.ei$q.ucl, "]")
df.q.fg$q.full <- paste0(df.q.fg$q, " [", df.q.fg$q.lcl, ", ", df.q.fg$q.ucl, "]")
df.q.overall$q.full <- paste0(df.q.overall$q, " [", df.q.overall$q.lcl, ", ", df.q.overall$q.ucl, "]")
if (("qh" %in% colnames(df.q.overall)) && ("qh.lcl" %in% colnames(df.q.overall)) && ("qh.ucl" %in% colnames(df.q.overall))) {
  df.q.overall$qh.full <- paste0(df.q.overall$qh, " [", df.q.overall$qh.lcl, ", ", df.q.overall$qh.ucl, "]")
}
if (QH.OMIT == FALSE) {
  df.qh.bm$qh.full <- paste0(df.qh.bm$qh, " [", df.qh.bm$qh.lcl, ", ", df.qh.bm$qh.ucl, "]")
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
if (QH.OMIT == FALSE) {
  if (("qh.full" %in% colnames(df.qh.bm)) && ("area_ha" %in% colnames(df.qh.bm))) {
    df.qh.bm <- df.reorder(df.qh.bm, col.move = "qh.full", col.before = "area_ha")
  }
}
cat("done\n")

cat("  Sorting data frames...")
df.q.ei <- df.q.ei[order(df.q.ei$indicator.group, df.q.ei$indicator, df.q.ei$bm.veg, df.q.ei$landcover), ]
cat("done\n")

cat("  Dropping superfluous columns...")
drop.cols <- c("q", "q.lcl", "q.ucl", "qh", "qh.lcl", "qh.ucl")
df.q.ei <- df.q.ei[, !(names(df.q.ei) %in% drop.cols)]
df.q.fg <- df.q.fg[, !(names(df.q.fg) %in% drop.cols)]
df.q.overall <- df.q.overall[, !(names(df.q.overall) %in% drop.cols)]
if (QH.OMIT == FALSE) df.qh.bm <- df.qh.bm[, !(names(df.qh.bm) %in% drop.cols)]
cat("done\n")

cat("  Renaming headers...")
# Keep all to support the variable content of
# these dfs, especially q.overall

# df.q.fg
names(df.q.ei)[names(df.q.ei) == "landcover"] <- "Land Cover Class"
names(df.q.ei)[names(df.q.ei) == "bm.veg"] <- "Benchmark Vegetation"
names(df.q.ei)[names(df.q.ei) == "indicator.group"] <- "Indicator Group"
names(df.q.ei)[names(df.q.ei) == "indicator"] <- "Indicator"
names(df.q.ei)[names(df.q.ei) == "f.group"] <- "Functional Group"
names(df.q.ei)[names(df.q.ei) == "dist"] <- "Distribution"
names(df.q.ei)[names(df.q.ei) == "test.tail"] <- "Test Tail"
names(df.q.ei)[names(df.q.ei) == "q.method"] <- "Quality Method"
names(df.q.ei)[names(df.q.ei) == "f.n"] <- "Focal N"
names(df.q.ei)[names(df.q.ei) == "f.mean"] <- "Focal Mean"
names(df.q.ei)[names(df.q.ei) == "f.sd"] <- "Focal SD"
names(df.q.ei)[names(df.q.ei) == "b.n"] <- "Benchmark N"
names(df.q.ei)[names(df.q.ei) == "b.mean"] <- "Benchmark Mean"
names(df.q.ei)[names(df.q.ei) == "b.sd"] <- "Benchmark SD"
names(df.q.ei)[names(df.q.ei) == "q"] <- "Q"
names(df.q.ei)[names(df.q.ei) == "q.lcl"] <- "Q.lcl"
names(df.q.ei)[names(df.q.ei) == "q.ucl"] <- "Q.ucl"
names(df.q.ei)[names(df.q.ei) == "mdes"] <- "MDES"
names(df.q.ei)[names(df.q.ei) == "q.full"] <- "Q [Lower, Upper 95% CLs]"

# df.q.fg
names(df.q.fg)[names(df.q.fg) == "landcover"] <- "Land Cover Class"
names(df.q.fg)[names(df.q.fg) == "f.group"] <- "Functional Group"
names(df.q.fg)[names(df.q.fg) == "q"] <- "Q"
names(df.q.fg)[names(df.q.fg) == "q.lcl"] <- "Q.lcl"
names(df.q.fg)[names(df.q.fg) == "q.ucl"] <- "Q.ucl"
names(df.q.fg)[names(df.q.fg) == "mdes"] <- "MDES"
names(df.q.fg)[names(df.q.fg) == "q.full"] <- "Q [Lower, Upper 95% CLs]"

# df.q.overall
names(df.q.overall)[names(df.q.overall) == "landcover"] <- "Land Cover Class"
names(df.q.overall)[names(df.q.overall) == "bm.veg"] <- "Benchmark Vegetation"
names(df.q.overall)[names(df.q.overall) == "q"] <- "Q"
names(df.q.overall)[names(df.q.overall) == "q.lcl"] <- "Q.lcl"
names(df.q.overall)[names(df.q.overall) == "q.ucl"] <- "Q.ucl"
names(df.q.overall)[names(df.q.overall) == "mdes"] <- "MDES"
names(df.q.overall)[names(df.q.overall) == "area_ha"] <- "Area (ha)"
names(df.q.overall)[names(df.q.overall) == "qh"] <- "QH"
names(df.q.overall)[names(df.q.overall) == "qh.lcl"] <- "QH.lcl"
names(df.q.overall)[names(df.q.overall) == "qh.ucl"] <- "QH.ucl"
names(df.q.overall)[names(df.q.overall) == "q.full"] <- "Q [Lower, Upper 95% CLs]"
names(df.q.overall)[names(df.q.overall) == "qh.full"] <- "QH [Lower, Upper 95% CLs]"

# df.qh.bm
if (QH.OMIT == FALSE) {
  names(df.qh.bm)[names(df.qh.bm) == "bm.veg"] <- "Benchmark Vegetation"
  names(df.qh.bm)[names(df.qh.bm) == "mdes"] <- "MDES"
  names(df.qh.bm)[names(df.qh.bm) == "area_ha"] <- "Area (ha)"
  names(df.qh.bm)[names(df.qh.bm) == "qh"] <- "QH"
  names(df.qh.bm)[names(df.qh.bm) == "qh.lcl"] <- "QH.lcl"
  names(df.qh.bm)[names(df.qh.bm) == "qh.ucl"] <- "QH.ucl"
  names(df.qh.bm)[names(df.qh.bm) == "qh.full"] <- "QH [Lower, Upper 95% CLs]"
}

# df.ind
names(df.ind)[names(df.ind) == "ind.name"] <- "Indicator"
names(df.ind)[names(df.ind) == "ind.group"] <- "Indicator Group"
names(df.ind)[names(df.ind) == "f.group"] <- "Functional Group"
names(df.ind)[names(df.ind) == "distn"] <- "Distribution"
names(df.ind)[names(df.ind) == "q.method"] <- "Quality Method"
names(df.ind)[names(df.ind) == "test.tail"] <- "Test Tail"
names(df.ind)[names(df.ind) == "bm.val"] <- "Fixed Benchmark Value"

# df.lc
names(df.lc)[names(df.lc) == "landCover"] <- "Land Cover Class"
names(df.lc)[names(df.lc) == "type"] <- "Land Cover Type"
names(df.lc)[names(df.lc) == "vegClass"] <- "Benchmark Vegetation"
names(df.lc)[names(df.lc) == "vegName"] <- "Vegetation Name"
names(df.lc)[names(df.lc) == "lc.type"] <- "Structural Class"
if ("area_ha" %in% colnames(df.lc)) {
  names(df.lc)[names(df.lc) == "area_ha"] <- "Area (ha)"
}
names(df.lc)[names(df.lc) == "focal_plots"] <- "Focal Plots (N.f)"
names(df.lc)[names(df.lc) == "bm_plots"] <- "Benchmark Plots (N.b)"
names(df.lc)[names(df.lc) == "all.n.OK"] <- "Run VQA?"

cat("done\n")

#####################################################
# Convert to sheets of single Excel workbook & save
#####################################################

cat("Preparing Excel file:\n", sep = "")

cat("  Creating workbook...")
wb <- wb_workbook()
cat("done\n")

####################
# Add sheet meta
####################

ws <- "Meta"
cat("  Preparing sheet '", ws, "':\n", sep = "")

cat("    Adding sheet...")
wb <- wb_add_worksheet(wb, sheet = ws)
wb <- wb_add_data(wb, ws, df.meta, col_names = FALSE)
cat("done\n")

cat("    Setting fonts...")
# Set first column bold and second column plain
wb$add_font(ws, "A1:A100", name = "Arial", bold = TRUE)
wb$add_font(ws, "B1:B100", name = "Arial")
cat("done\n")

cat("    Setting column width and justification...")
wb$set_col_widths(sheet = "meta", cols = c(1), widths = max(nchar(df.meta$head)) + 2)
wb$set_col_widths(sheet = "meta", cols = c(2), widths = max(nchar(df.meta$value)) + 2)
cat("done\n")

####################
# Add sheet Parameters
####################

ws <- "Parameters"
cat("  Preparing sheet '", ws, "':\n", sep = "")

cat("    Adding sheet...")
wb <- wb_add_worksheet(wb, sheet = ws)
wb <- wb_add_data(wb, ws, df.settings)
cat("done\n")

cat("    Setting fonts...")
# Set first column bold and second column plain
wb$add_font(ws, "A1:Z500", name = "Arial") # Whole sheet
wb$add_font(ws, "A1:Z1", name = "Arial", bold = TRUE) # Header
cat("done\n")

cat("    Setting column width and justification...")
# Set col width to widest string, justify text left, numbers right
xl.col.wj(curr.wb = wb, curr.ws = ws, curr.df = df.settings)
cat("done\n")

####################
# Add sheet Indicators
####################

ws <- "Indicators"
cat("  Preparing sheet '", ws, "':\n", sep = "")

cat("    Adding sheet...")
wb <- wb_add_worksheet(wb, sheet = ws)
wb <- wb_add_data(wb, ws, df.ind)
cat("done\n")

cat("    Setting fonts...")
# Set first column bold and second column plain
wb$add_font(ws, "A1:Z500", name = "Arial") # Whole sheet
wb$add_font(ws, "A1:Z1", name = "Arial", bold = TRUE) # Header
cat("done\n")

cat("    Setting column width and justification...")
# Set col width to widest string, justify text left, numbers right
xl.col.wj(curr.wb = wb, curr.ws = ws, curr.df = df.ind)
cat("done\n")

####################
# Add sheet LandCover
####################

ws <- "LandCover"
cat("  Preparing sheet '", ws, "':\n", sep = "")

cat("    Adding sheet...")
wb <- wb_add_worksheet(wb, sheet = ws)
wb <- wb_add_data(wb, ws, df.lc)
cat("done\n")

cat("    Setting fonts...")
# Set first column bold and second column plain
wb$add_font(ws, "A1:Z500", name = "Arial") # Whole sheet
wb$add_font(ws, "A1:Z1", name = "Arial", bold = TRUE) # Header
cat("done\n")

cat("    Setting column width and justification...")
# Set col width to widest string, justify text left, numbers right
xl.col.wj(curr.wb = wb, curr.ws = ws, curr.df = df.lc)
cat("done\n")

####################
# Add sheet q.ei
####################

ws <- "Q.i"
cat("  Preparing sheet '", ws, "':\n", sep = "")

cat("    Adding sheet...")
wb <- wb_add_worksheet(wb, sheet = ws)
wb <- wb_add_data(wb, ws, df.q.ei)
cat("done\n")

cat("    Setting fonts...")
wb$add_font(ws, "A1:Z500", name = "Arial") # Whole sheet
wb$add_font(ws, "A1:Z1", name = "Arial", bold = TRUE) # Header
cat("done\n")

cat("    Setting column width and justification...")
# Set col width to widest string, justify text left, numbers right
xl.col.wj(curr.wb = wb, curr.ws = ws, curr.df = df.q.ei)

# Special handling for formatted column q.full
col.q.full <- "Q [Lower, Upper 95% CLs]"
col.idx <- which(names(df.q.ei) == col.q.full)
col.idx.xl <- xlcolconv(col.idx)
cmd <- paste0("wb$add_cell_style( sheet=ws, dims='", col.idx.xl, "1:", col.idx.xl, "500', horizontal='center' )")
eval(parse(text = cmd))

cat("done\n")

####################
# Add sheet q.fg
####################

ws <- "Q.fg"
cat("  Preparing sheet '", ws, "':\n", sep = "")

cat("    Adding sheet...")
wb <- wb_add_worksheet(wb, sheet = ws)
wb <- wb_add_data(wb, ws, df.q.fg)
cat("done\n")

cat("    Setting fonts...")
wb$add_font(ws, "A1:Z500", name = "Arial") # Whole sheet
wb$add_font(ws, "A1:Z1", name = "Arial", bold = TRUE) # Header
cat("done\n")

# Set col width to widest string, justify text left, numbers right
cat("    Setting column width and justification...")
xl.col.wj(curr.wb = wb, curr.ws = ws, curr.df = df.q.fg)

# Special handling for formatted column q.full
col.idx <- which(names(df.q.fg) == col.q.full) # Reusing col.q.full
col.idx.xl <- xlcolconv(col.idx)
cmd <- paste0("wb$add_cell_style( sheet=ws, dims='", col.idx.xl, "1:", col.idx.xl, "500', horizontal='center' )")
eval(parse(text = cmd))

cat("done\n")

####################
# Add sheet q.overall
####################

ws <- "Q.overall"
cat("  Preparing sheet '", ws, "':\n", sep = "")

cat("    Adding sheet...")
wb <- wb_add_worksheet(wb, sheet = ws)
wb <- wb_add_data(wb, ws, df.q.overall)
cat("done\n")

cat("    Setting fonts...")
wb$add_font(ws, "A1:Z500", name = "Arial") # Whole sheet
wb$add_font(ws, "A1:Z1", name = "Arial", bold = TRUE) # Header
cat("done\n")

# Set col width to widest string, justify text left, numbers right
cat("    Setting column width and justification...")
xl.col.wj(curr.wb = wb, curr.ws = ws, curr.df = df.q.overall)

# Special handling for formatted column q.full
col.idx <- which(names(df.q.overall) == col.q.full) # Reusing col.q.full
col.idx.xl <- xlcolconv(col.idx)
cmd <- paste0("wb$add_cell_style( sheet=ws, dims='", col.idx.xl, "1:", col.idx.xl, "500', horizontal='center' )")
eval(parse(text = cmd))

# Special handling for formatted column qh.full
col.qh.full <- "QH [Lower, Upper 95% CLs]"
if ((col.qh.full %in% colnames(df.q.overall))) {
  col.idx <- which(names(df.q.overall) == col.qh.full)
  col.idx.xl <- xlcolconv(col.idx)
  cmd <- paste0("wb$add_cell_style( sheet=ws, dims='", col.idx.xl, "1:", col.idx.xl, "500', horizontal='center' )")
  eval(parse(text = cmd))
}

cat("done\n")

####################
# Add sheet QH.bm
####################

if (!QH.OMIT) {
  ws <- "QH.bm"
  cat("  Preparing sheet '", ws, "':\n", sep = "")

  cat("    Adding sheet...")
  wb <- wb_add_worksheet(wb, sheet = ws)
  wb <- wb_add_data(wb, ws, df.qh.bm)
  cat("done\n")

  cat("    Setting fonts...")
  wb$add_font(ws, "A1:Z500", name = "Arial") # Whole sheet
  wb$add_font(ws, "A1:Z1", name = "Arial", bold = TRUE) # Header
  cat("done\n")

  # Set col width to widest string, justify text left, numbers right
  cat("    Setting column width and justification...")
  xl.col.wj(curr.wb = wb, curr.ws = ws, curr.df = df.qh.bm)
  #
  # # Special handling for formatted column q.full
  # col.idx <- which(names(df.qh.bm)==col.q.full)  # Reusing col.q.full
  # col.idx.xl <- xlcolconv(col.idx)
  # cmd <- paste0("wb$add_cell_style( sheet=ws, dims='", col.idx.xl, "1:", col.idx.xl, "500', horizontal='center' )")
  # eval(parse( text=cmd ))

  # Special handling for formatted column qh.full
  col.qh.full <- "QH [Lower, Upper 95% CLs]"
  if ((col.qh.full %in% colnames(df.qh.bm))) {
    col.idx <- which(names(df.qh.bm) == col.qh.full)
    col.idx.xl <- xlcolconv(col.idx)
    cmd <- paste0("wb$add_cell_style( sheet=ws, dims='", col.idx.xl, "1:", col.idx.xl, "500', horizontal='center' )")
    eval(parse(text = cmd))
  }
  cat("done\n")
}

##########################
# Save the final workbook
##########################

file.q.xlsx.out <- paste0(RESULTSDIR, FNAME.Q.XLSX.OUT)
cat("Saving completed Excel file '", FNAME.Q.XLSX.OUT, "' to results directory...", sep = "")
wb$save(file.q.xlsx.out)
cat("done\n")
cat("\n")

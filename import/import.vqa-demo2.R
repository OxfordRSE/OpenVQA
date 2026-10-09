################################################
# Import raw data and normalize to standard VQA
# input files
#
# Imports raw data and performs source-specific
# validations & corrections.
# Called by generic script import.R
#
# Requirements:
# 1. Raw data present in directory data/<PROJ>/<ASSESS>/raw/
# 2. Generic parameters in file params.R
# 2. Project-specific parameters in file params.<PROJ>.R
################################################

##########################################
#  Functions
##########################################

#################################################
#################################################
# Main
#################################################
#################################################

# Set up error message df
curr.date <- Sys.Date()
df.err <- as.data.frame(paste0("Import errors for project ", PROJ, " on ", curr.date, "."))
colnames(df.err) <- "message"
df.err$message <- as.character(df.err$message)
df.err$action <- as.character("")
df.err.bak <- df.err

###############################
###############################
# Import raw data
###############################
###############################

cat("\n")
cat("******************************************\n")
cat("Importing raw data\n")
cat("******************************************\n")
cat("\n")

cat("Importing database from file '", RAW_PLOTDATA_FILENAME, "'...", sep = "")
# Extract the worksheets, specifying data types as needed
f.data.raw <- paste0(RAWDATADIR, RAW_PLOTDATA_FILENAME)
cat("done\n")

cat("Extracting sheets:\n")
# Note: carriage return MUST be included or read_excel
# will suppress entire preceding message!
cat("- '", SHEET.VEG, "'...done\n", sep = "")
df.vegetation.raw <- as.data.frame(
  read_excel(f.data.raw,
    sheet = SHEET.VEG
  )
)
cat("- '", SHEET.SITES, "'...done\n", sep = "")
df.site.raw <- as.data.frame(
  read_excel(f.data.raw,
    sheet = SHEET.SITES
  )
)
cat("- '", SHEET.STRATA, "'...done\n", sep = "")
df.stratum.raw <- as.data.frame(
  read_excel(f.data.raw,
    sheet = SHEET.STRATA,
    .name_repair = "unique_quiet" # Turn off annoying column rename messages
  )
)
cat("- '", SHEET.PLOTS, "'...done\n", sep = "")
df.plot.raw <- as.data.frame(
  read_excel(f.data.raw,
    sheet = SHEET.PLOTS,
    .name_repair = "unique_quiet"
  )
)
cat("- '", SHEET.SPECIES, "'...done\n", sep = "")
df.species.raw <- as.data.frame(
  read_excel(f.data.raw,
    sheet = SHEET.SPECIES
  )
)
cat("- '", SHEET.COVER, "'...done\n", sep = "")
df.cover.raw <- as.data.frame(
  read_excel(f.data.raw,
    col_types = c("text"),
    sheet = SHEET.COVER
  )
)
cat("- '", SHEET.STRUCTURE, "'...done\n", sep = "")
df.structure.raw <- as.data.frame(
  read_excel(f.data.raw,
    col_types = c(
      "text", "text", "text", "text", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric"
    ),
    sheet = SHEET.STRUCTURE
  )
)

# Greece exotic species list
cat("Importing exotic species list...")
f.exotic.spp.raw <- paste0(RAWDATADIR, RAW_SPP_FILENAME)
df.exotic.spp.raw <- wb_to_df(f.exotic.spp.raw, sheet = RAW_SPP_SHEET)
cat("done\n")

# Save the raw data unchanged and work with the copies
cat("Backing up raw data data frames...")
df.vegetation <- df.vegetation.raw
df.site <- df.site.raw
df.stratum <- df.stratum.raw
df.plot <- df.plot.raw
df.species <- df.species.raw
df.cover <- df.cover.raw
df.structure <- df.structure.raw
df.exotic.spp <- df.exotic.spp.raw
cat("done\n")

#####################################
#####################################
# Import prepared benchmark files
# from main assessment if running
# offset assessment\
#####################################
#####################################

if (scope == "offset") {
  cat("\n")
  cat("*********************************************\n")
  cat("Importing previously-prepared benchmark data\n")
  cat("*********************************************\n")
  cat("\n")

  ######################################
  # Check required data frames present
  #####################################

  # Convert data frame list to file list & set data directory name
  f.input.list <- paste0(df.input.list, ".csv")
  f.input.dir <- paste0(DATA_BASEDIR_PROJ, bm.data.source.assess, "/inputs/")
  cat("Using source directory '", f.input.dir, "'\n", sep = "")

  cat("Checking that all required input files exist:\n")
  f.missing <- FALSE

  for (f.input.name in f.input.list) {
    # Form the file path and name
    f.input <- paste0(f.input.dir, f.input.name)

    cat("  ", f.input.name, "...", sep = "")
    if (file.exists(f.input)) {
      cat("found\n")
    } else {
      cat("MISSING!\n")
      f.missing <- TRUE
    }
  }

  if (f.missing == TRUE) {
    msg <- paste0("One or more input files are missing from directory '", f.input.dir, "'!\n")
    msg <- paste0(msg, "Please ensure that all of the above files\n")
    msg <- paste0(msg, "exist before running this import.\n")
    stop(msg)
  }

  cat("Importing VQA input files:\n")

  for (df.input in df.input.list) {
    f.input.name <- paste0(df.input, ".csv")
    f.input <- paste0(f.input.dir, f.input.name)

    # Form the import command
    df.input.bm.name <- paste0("df.", df.input, ".bm")
    cmd <- paste0(df.input.bm.name, " <- read.csv('", f.input, "', header=TRUE)")
    # cat(cmd, "\n")
    cat(paste0("  ", df.input.bm.name, "..."))
    eval(parse(text = cmd))
    cat("done\n")
  }

  cat("Dropping unneeded dfs...")
  rm(df.landCover.bm, df.species.bm)
  cat("done\n")

  cat("Performing initial filtering of input dfs to bm plots and metadata only...")
  df.plotMetadata.bm <- df.plotMetadata.bm[df.plotMetadata.bm$focalOrBenchmark == "b", ]
  df.speciesCover.bm <- df.speciesCover.bm[df.speciesCover.bm$focalOrBenchmark == "b", ]
  df.exoticCoverByStratum.bm <- df.exoticCoverByStratum.bm[df.exoticCoverByStratum.bm$focalOrBenchmark == "b", ]
  df.coverByGrowthForm.bm <- df.coverByGrowthForm.bm[df.coverByGrowthForm.bm$focalOrBenchmark == "b", ]
  cat("done\n")
}

#####################################
#####################################
# Standardize & validate data
#####################################
#####################################

cat("\n")
cat("******************************************\n")
cat("Standardizing & validating data\n")
cat("******************************************\n")
cat("\n")

cat("Performing general standardizations:\n")

##########################################
# Rename fields
##########################################

cat("- Renaming fields...")

# vegetation
names(df.vegetation)[names(df.vegetation) %in% c("Vegetation Code", "VegetationCode", "Vegetation code")] <- "vegCode"
names(df.vegetation)[names(df.vegetation) %in% c("Vegetation name", "VegetationName")] <- "vegName"

# strata
names(df.stratum)[names(df.stratum) == "Stratum code"] <- "stratumCode"
names(df.stratum)[names(df.stratum) %in%
  c("Vegetation code", "VegetationCode", "vegetationCode")] <- "vegCode"
names(df.stratum)[names(df.stratum) == "Notes"] <- "notes"

# sites
names(df.site)[names(df.site) %in% c("Site code", "SiteCode")] <- "siteCode"
names(df.site)[names(df.site) %in% c("Stratum code", "StratumCode")] <- "stratumCode"
names(df.site)[names(df.site) %in%
  c("Vegetation code", "VegetationCode", "vegetationCode")] <- "vegCode"
names(df.site)[names(df.site) == "Ha"] <- "ha"
names(df.site)[names(df.site) == "Notes"] <- "notes"

# df.plot
names(df.plot)[names(df.plot) == "PlotCode"] <- "plotCode"
names(df.plot)[names(df.plot) == "Date"] <- "date"
names(df.plot)[names(df.plot) == "Assessment"] <- "assessCode"
names(df.plot)[names(df.plot) == "SiteCode"] <- "siteCode"
names(df.plot)[names(df.plot) %in% c("Vegetation", "Vegetation code", "VegetationCode", "vegetationCode")] <- "vegCode"
names(df.plot)[names(df.plot) == "Notes"] <- "notes"

# df.species
names(df.species)[names(df.species) == "Species"] <- "species"

# cover
names(df.cover)[names(df.cover) == "PlotCode"] <- "plotCode"
names(df.cover)[names(df.cover) == "Species"] <- "species"

# structure: throw in extra dbh fields in case of >10 dbh measurements
names(df.structure)[names(df.structure) == "Plot"] <- "plotCode"
names(df.structure)[names(df.structure) == "Subplot"] <- "subplot"
names(df.structure)[names(df.structure) == "Zone"] <- "zone"
names(df.structure)[names(df.structure) == "Species"] <- "species"
names(df.structure)[names(df.structure) == "DBH1"] <- "dbh1"
names(df.structure)[names(df.structure) == "DBH2"] <- "dbh2"
names(df.structure)[names(df.structure) == "DBH3"] <- "dbh3"
names(df.structure)[names(df.structure) == "DBH4"] <- "dbh4"
names(df.structure)[names(df.structure) == "DBH5"] <- "dbh5"
names(df.structure)[names(df.structure) == "DBH6"] <- "dbh6"
names(df.structure)[names(df.structure) == "DBH7"] <- "dbh7"
names(df.structure)[names(df.structure) == "DBH8"] <- "dbh8"
names(df.structure)[names(df.structure) == "DBH9"] <- "dbh9"
names(df.structure)[names(df.structure) == "DBH10"] <- "dbh10"
names(df.structure)[names(df.structure) == "DBH11"] <- "dbh11"
names(df.structure)[names(df.structure) == "DBH12"] <- "dbh12"
names(df.structure)[names(df.structure) == "DBH13"] <- "dbh13"
names(df.structure)[names(df.structure) == "DBH14"] <- "dbh14"
names(df.structure)[names(df.structure) == "DBH15"] <- "dbh15"

# Exotic species
names(df.exotic.spp)[names(df.exotic.spp) == c("Family")] <- "family"
names(df.exotic.spp)[names(df.exotic.spp) == "Taxon Scientific Name"] <- "taxonWithAuthor"
names(df.exotic.spp)[names(df.exotic.spp) == "Status"] <- "establishmentStatus"
names(df.exotic.spp)[names(df.exotic.spp) == "Arch /Neo"] <- "archNeo"
names(df.exotic.spp)[names(df.exotic.spp) == "Invasi-veness"] <- "invasiveStatus"
cat("done\n")

cat("- Converting factors to character...")
df.plot <- factors.to.chr(df.plot)
df.species <- factors.to.chr(df.species)
df.cover <- factors.to.chr(df.cover)
df.structure <- factors.to.chr(df.structure)
df.site <- factors.to.chr(df.site)
df.stratum <- factors.to.chr(df.stratum)
df.vegetation <- factors.to.chr(df.vegetation)
df.exotic.spp <- factors.to.chr(df.exotic.spp)
cat("done\n")

cat("- Removing spaces from plot codes:\n")
cat("-- df.plot...")
df.plot$plotCode <- gsub("\\s+", "", df.plot$plotCode)
cat("done\n")
cat("-- df.structure...")
df.structure$plotCode <- gsub("\\s+", "", df.structure$plotCode)
cat("done\n")
cat("-- df.cover...")
df.cover$plotCode <- gsub("\\s+", "", df.cover$plotCode)
cat("done\n")

##########################################
# Initial validations
##########################################

cat("Checking for missing plots:\n")

cat("- Checking for non-matching plots:\n")
plot.plots <- sort(unique(df.plot$plotCode))
structure.plots <- sort(unique(df.structure$plotCode))
cover.plots <- sort(unique(df.cover$plotCode))
str.cov.plots <- sort(unique(c(structure.plots, cover.plots)))

cat("-- in df.structure but not in df.plot: ")
diff.str <- setdiff(structure.plots, plot.plots)
n.diff.str <- length(diff.str)
cat(n.diff.str, " plots missing\n", sep = "")

cat("-- in df.cover but not in df.plot: ")
diff.cov <- setdiff(cover.plots, plot.plots)
n.diff.cov <- length(diff.cov)
cat(n.diff.cov, " plots missing\n", sep = "")

cat("-- in df.plot but not in df.structure & df.cover combined: ")
diff.str.cov <- setdiff(plot.plots, str.cov.plots)
n.diff.str.cov <- length(diff.str.cov)
cat(n.diff.str.cov, " plots missing\n", sep = "")

if (n.diff.str > 0 || n.diff.cov > 0 | n.diff.str.cov > 0) {
  min.p.orphan <- max(n.diff.str, n.diff.cov, n.diff.str.cov)
  stop(">=", min.p.orphan, " orphan plots detected!\n")
}

########################################
# Filter plots to scope of current
# assessment. Also filter related tables.
########################################

cat("Filtering plots to scope '", scope, "':\n", sep = "")

cat("- Plots before: ", nrow(df.plot), "\n", sep = "")
if (scope == "baseline") {
  # Baseline focal + benchmark plots only
  df.plot <- df.plot[df.plot$assessCode %in% c("B", "C"), ]
} else if (scope == "current") {
  # Current focal (M) + benchmark plots only
  df.plot <- df.plot[df.plot$assessCode %in% c("B", "M"), ]
} else if (scope == "offset" || scope == "offset_current") {
  # Offset + benchmark plots only
  df.plot <- df.plot[df.plot$assessCode %in% c("B", "A", "O"), ]
  # } else if ( scope=="offset_baseline" ) {
  #   # Only offset areas needed
  #   # Under construction
  #   df.plot <- df.plot[ df.plot$assess.code %in% c("B","A","O"), ]
} else {
  stop("- ERROR: unknow value for parameter 'scope'!")
}

# Filter structure and cover data accordingly
plots.in.scope <- unique(df.plot$plotCode)
df.structure <- df.structure[df.structure$plotCode %in% plots.in.scope, ]
df.cover <- df.cover[df.cover$plotCode %in% plots.in.scope, ]
cat("- Plots after: ", nrow(df.plot), "\n", sep = "")

##########################################
# Prepare the main data frames
##########################################
##########################################

##########################################
# df.vegetation
#
# Lookup table of benchmark vegetation types
##########################################

cat("Preparing vegetation table (df.vegetation):\n")

cat("- Assigning vegetation structural type ('lc.type')...")
df.vegetation$lc.type <- vegCode.lc.type(df.vegetation$vegCode)
df.vegetation <- df.vegetation[, c("vegCode", "lc.type", "vegName")]
cat("done\n")

############################################################
# df.site
#
# Lookup table of physical spatial units along the pipeline ROW
# (treatment code "R"). Sites separate areas with different
# baseline benchmark vegetation. However, for sites with forested
# or shrubby vegetation, the central 8 m-wide "Permanent Impact
# Zone" (treatment code "P") is reassigned a grassland target
# (benchmark) vegetation, different from the baseline benchmark,
# because shrubs and trees >1 m tall will be removed to avoid root
# interference with the pipeline. The remaining temporary impact
# zone (treatment code "T") retains the original baseline benchmark,
# as do control plots (treatment code "C") collected adjacent to
# (but outside) the ROW. In summary, sites with a baseline of
# forested or shrubby vegetation are split into treatments T & P
# (with different benchmarks), and sites with a baseline of grassland
# vegetation are not split, and are coded as treatement R. Most sites,
# regardless of vegetation, will have adjacent control plots as
# well (treatment C). Note that each treatment is regarded as a
# different sampling stratum. However, the treatments from multiple
# sites in the same general area which use the same benchmark may be
# grouped into the same sampling stratum. The relationship between
# sites, treatments and strata is described in table df.siteStrata.
# Finally, note that the original area data are recorded in
# this table.
############################################################

cat("Preparing site table (df.site):\n")
df.site$notes[grepl("BmVeg", df.site$siteCode)] <- "Benchmark pseudo-site. Only SiteCode + vegetationCode required."

cat("- Adding column is.pseudoSite...")
df.site$is.pseudoSite <- FALSE
df.site$is.pseudoSite[is.na(df.site$StratumCode)] <- TRUE
df.site <- df.reorder(df.site, col.move = "is.pseudoSite", col.before = "siteCode")
cat("done\n")

##########################################
# df.stratum
#
# Sampling strata. An independent unit of
# sampling and measurement of quality.
# Equivalent to land cover. See df.landCover,
# below.
##########################################

cat("Preparing stratum table (df.stratum):\n")

# Sum site ha and add to df.stratum
cat("- Summing per-stratum site areas and adding to df.stratum...")
stratum.ha <- aggregate(
  ha ~ stratumCode,
  data = df.site[!is.na(df.site$ha), ],
  FUN = sum
)
# df.stratum <- merge( df.stratum, stratum.ha, by="stratumCode", all.x=TRUE)
df.stratum <- merge(df.stratum, stratum.ha, by = "stratumCode")
cat("done\n")

cat("- Checking for unmatched strata with no value for ha...")
n.missing <- nrow(df.stratum[is.na(df.stratum$ha), ])
if (n.missing > 0) {
  msg <- paste0(n.missing, " rows in df.stratum have no value for 'ha'!\n")
  stop_quietly(msg)
} else {
  cat("done\n")
}

df.stratum <- df.reorder(df.stratum, col.move = "ha", col.before = "vegCode")
df.stratum$notes[is.na(df.stratum$notes)] <- ""

# Name of human-readable file to save
fname.siteStratum.saved <- paste0("siteStratumSummary.", scope, ".csv")

######################################################
# Add siteCodeNumeric to df.site and save
# to results directory for inclusion in reports
######################################################

cat("Adding siteCodeNumeric to df.site and saving:\n")
df.siteStratum.saved <- df.site
df.siteStratum.saved$is.pseudoSite[grep("BmVeg", df.siteStratum.saved$siteCode)] <- TRUE

# Add integer site code field for sorting in Excel
cat("- Adding siteCodeNumeric to df.site...")
df.siteStratum.saved$siteCodeNumeric <-
  gsub("Site-O", "", df.siteStratum.saved$siteCode)
df.siteStratum.saved$siteCodeNumeric <-
  gsub("Site-P", "", df.siteStratum.saved$siteCode)
df.siteStratum.saved$siteCodeNumeric <-
  gsub("Site-Bm", "", df.siteStratum.saved$siteCodeNumeric)

df.siteStratum.saved <- df.siteStratum.saved[
  ,
  c("siteCode", "siteCodeNumeric", "stratumCode", "vegCode", "ha", "notes")
]
df.siteStratum.saved$notes[is.na(df.siteStratum.saved$notes)] <- ""
df.siteStratum.saved$notes[df.siteStratum.saved$notes == "Benchmark pseudo-site. Only SiteCode + vegetationCode required."] <- "Benchmark pseudo-site"

# Set default sort order and rename columns
df.siteStratum.saved <- df.siteStratum.saved[
  order(df.siteStratum.saved$siteCodeNumeric, df.siteStratum.saved$stratumCode),
]
colnames(df.siteStratum.saved) <- c(
  "Site Code", "siteCodeNumeric", "Stratum Code", "Benchmark Vegetation", "Area (ha)"
)
cat("done\n")

# Set file name by scope & save
cat("- Saving df.siteStratum to directory '", RESULTSDIR, "' as file '", fname.siteStratum.saved, "'...", sep = "")
fileandpath <- paste0(RESULTSDIR, fname.siteStratum.saved)
write.csv(df.siteStratum.saved, file = fileandpath, row.names = FALSE)
cat("done\n")

##########################################
# df.landCover
#
# Land cover classes (=strata) and their areas.
# Will get sample sizes later from lc.summary
##########################################

cat("Preparing land cover table (df.landCover):\n")

cat("- Getting land cover strata from df.stratum...")
df.landCover <- df.stratum[, c("stratumCode", "vegCode", "ha")]
cat("done\n")

# cat("- Adding missing strata from control sites...")
# df.stratum.all <- unique(df.siteStratum[,c("stratumCode","vegCode")])
# df.stratum.all <- merge( df.stratum.all, df.stratum[ , c("stratumCode", "ha")], by="stratumCode", all.x=TRUE)
# df.stratum.missing <- df.stratum.all[ is.na(df.stratum.all$ha),]
# df.landCover <- rbind( df.landCover, df.stratum.missing )
# df.landCover <- df.landCover[ order( df.landCover$stratumCode ), ]
# cat("done\n")

cat("- Adding column lc.type & reordering...")
df.veg.lc.type <- df.vegetation[, c("vegCode", "lc.type")]
df.landCover <- merge(df.landCover, df.veg.lc.type, by = "vegCode", all.x = TRUE)
df.landCover <- df.reorder(df.landCover, col.move = "lc.type", col.before = "stratumCode")
df.landCover <- df.reorder(df.landCover, col.move = "stratumCode", move.first = TRUE)
cat("done\n")

##########################################
# Prune benchmark data
#
# Use df.landCover to flag and keep only
# plots, speciesCover and structure data
# related to offset benchmark vegetation
##########################################

if (scope == "offset") {
  cat("Preparing benchmark data:\n")
  cat("- Pruning benchmark data to match offset benchmark vegetation...")
  vegClasses <- unique(df.landCover$vegCode)
  df.plotMetadata.bm <- df.plotMetadata.bm[df.plotMetadata.bm$vegClass %in% vegClasses, ]
  bm.plotCodes <- unique(df.plotMetadata.bm$plotCode)
  df.speciesCover.bm <- df.speciesCover.bm[df.speciesCover.bm$plotCode %in% bm.plotCodes, ]
  df.exoticCoverByStratum.bm <- df.exoticCoverByStratum.bm[df.exoticCoverByStratum.bm$plotCode %in% bm.plotCodes, ]
  df.coverByGrowthForm.bm <- df.coverByGrowthForm.bm[df.coverByGrowthForm.bm$plotCode %in% bm.plotCodes, ]
  df.speciesStems.bm <- df.speciesStems.bm[df.speciesStems.bm$plotCode %in% bm.plotCodes, ]
  cat("done\n")

  cat("- Extracting df.species.bm from data...")
  df.species.bm <- df.speciesCover.bm[, c("species"), drop = FALSE]
  df.species.bm <- rbind(df.species.bm, df.speciesStems.bm[, c("species"), drop = FALSE])
  df.species.bm <- unique(df.species.bm[, c("species"), drop = FALSE])
  cat("done\n")
}

##########################################
# df.exotic.spp
#
# Prepare exotic species list
##########################################

cat("Preparing exotic species list (df.exotic.spp):\n")

cat("- Removing unwanted columns...")
df.exotic.spp <- df.exotic.spp[, c("family", "taxonWithAuthor", "establishmentStatus", "archNeo", "invasiveStatus")]
df.exotic.spp <- df.exotic.spp[!is.na(df.exotic.spp$taxonWithAuthor), ]
cat("done\n")

cat("- Converting HTML whitespace to regular whitespace...")
df.exotic.spp <- df.exotic.spp %>%
  mutate(across(where(is.character), ~ gsub("\\s", " ", .x, perl = TRUE)))
cat("done\n")

cat("- Splitting off species from taxon+author...")
nameTokens <- strsplit(as.character(df.exotic.spp$taxonWithAuthor), split = " ")
df.exotic.spp$species <- sapply(nameTokens, function(x) paste(x[1], x[2], sep = " "))
df.exotic.spp <- unique(df.exotic.spp[, !names(df.exotic.spp) == "taxonWithAuthor"])
cat("done\n")

cat("- De-duplicating species with multiple entries...")
# These will occur if subspecies are included, but only matters if
# the subspecies have different values of establishmentStatus, archNeo
# and invasiveStatus.
# For species with >1 row, sort in order that puts Established,
# Invasive, Neophytes first and keep first row only
df.exotic.spp <- df.exotic.spp %>%
  arrange(species, establishmentStatus, invasiveStatus, desc(archNeo)) %>%
  group_by(species) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  as.data.frame()
cat("done\n")

cat("- Marking established, non-invasive archeophytes as native, for VQA purposes...")
# All Neo-introductions are flagged as exotic
# Archeophytes are flagged as exotic only if they are
# invasive or non-establish, or both. In other words, non-invasive
# established archeophytes are considered established components
# of the modern Greek flora and therefore treated as non-exotic.
df.exotic.spp$is_exotic <- 1
df.exotic.spp$is_exotic[df.exotic.spp$archNeo == "Arch" &
  df.exotic.spp$establishmentStatus == "Established" &
  df.exotic.spp$invasiveStatus == "NonInv"] <- 0
cat("done\n")

##########################################
# df.species
#
# Flag exotic species and reformat
##########################################

cat("Preparing species reference list (df.species):\n")

cat("- Correcting know taxonomic issues in species list and data...")
df.cover$species[!is.na(df.cover$species) & df.cover$species == "Pinus nigra (planted)"] <- "Pinus nigra"
df.structure$species[!is.na(df.structure$species) & df.structure$species == "Pinus nigra (planted)"] <- "Pinus nigra"
df.species <- df.species[!df.species$species == "Pinus nigra (planted)", , drop = FALSE]

if (scope == "offset") {
  df.speciesCover.bm$species[!is.na(df.speciesCover.bm$species) & df.speciesCover.bm$species == "Pinus nigra (planted)"] <- "Pinus nigra"
  df.speciesStems.bm$species[!is.na(df.speciesStems.bm$species) & df.speciesStems.bm$species == "Pinus nigra (planted)"] <- "Pinus nigra"
  df.species.bm <- df.species.bm[!df.species.bm$species == "Pinus nigra (planted)", , drop = FALSE]
}
cat("done\n")

cat("- Setting df.species to contents of lookup worksheet 'species'...")
# Start with "official" species list from sheet species, and
# add any missed species from structure and cover sheets
df.species.main <- as.data.frame(unique(df.species[, c("species")]))
colnames(df.species.main) <- c("species")
df.species.main$main <- as.logical(TRUE)
cat("done\n")

cat("- Merging missing species from field data into df.species...")
# Get supplementary species
df.species.str <- as.data.frame(unique(df.structure[, c("species")]))
colnames(df.species.str) <- c("species")
df.species.cov <- as.data.frame(unique(df.cover[, c("species")]))
colnames(df.species.cov) <- c("species")
df.species.supp <- rbind(df.species.str, df.species.cov)
df.species.supp <- as.data.frame(unique(df.species.supp))
colnames(df.species.supp) <- c("species")
rm(df.species.str, df.species.cov)

if (scope == "offset" & QH.METHOD == "empirical") {
  # Add in extra bm species, if any
  df.species.supp <- rbind(df.species.supp, df.species.bm)
  df.species.supp <- unique(df.species.supp)
  df.species.supp <- df.species.supp[!is.na(df.species.supp$species), , drop = FALSE]
}

# # Note this:
# df.species.supp[grepl("planted", df.species.supp$species), ]
# # Need to figure out if/how to track planted trees in offsets
# # Does is matter?

df.species.supp <- merge(df.species.supp, df.species.main, by = "species", all.x = TRUE)
df.species.supp$main[is.na(df.species.supp$main)] <- FALSE

if (nrow(df.species.supp[df.species.supp$main == FALSE, ]) > 0) {
  # Add the missing species
  df.species.supp <- df.species.supp[df.species.supp$main == FALSE, ]
  n.missing <- nrow(df.species.supp)
  df.species.main <- rbind(df.species.main, df.species.supp)
  cat("WARNING: ", n.missing, " missing species added to main list...", sep = "")
} else {
  cat("no missing species found...")
}
df.species <- df.species.main
df.species$src <- "main"
df.species$src[df.species$main == FALSE] <- "data"
df.species <- df.species[, !names(df.species) == "main"]
cat("done\n")

cat("- Extracting genus from species name...")
nameTokens <- strsplit(as.character(df.species$species), split = " ")
df.species$genus <- sapply(nameTokens, function(x) paste(x[1], sep = " "))
df.species <- df.reorder(df.species, col.move = "genus", col.before = "species")
cat("done\n")

cat("- Using df.exotic.spp to flag exotic species in df.species...")
df.species <- merge(df.species,
  df.exotic.spp[, c("species", "is_exotic"), drop = FALSE],
  by = "species", all.x = TRUE
)
# Mark species present in exotics list
df.species$is_in_exotics_list <- 0
df.species$is_in_exotics_list[!is.na(df.species$is_exotic)] <- 1
# Flag non-exotics
df.species$is_exotic[is.na(df.species$is_exotic)] <- 0
cat("done\n")

cat("- Manually marking known exotics not in exotics list...")
# Update by species
df.species$is_exotic[df.species$species %in% c(
  "Pinus radiata", "Pinus ponderosa"
)] <- 1
# Update by genus
df.species$is_exotic[df.species$genus %in% c("Eucalyptus")] <- 1
cat("done\n")

cat("- Summarizing counts of exotic species:\n")
df.species.status.cnt <- aggregate(
  species ~ is_exotic,
  data = df.species,
  FUN = length
)
print(df.species.status.cnt, row.names = FALSE)

##########################################
# df.plot
##########################################

cat("Preparing plot metadata table (df.plot):\n")

cat("- Removing unnecessary columns...")
cols.drop <- c("date", "latitude", "longitude", "notes")
df.plot <- df.plot[, !names(df.plot) %in% cols.drop]
cat("done\n")

cat("- Removing NA rows...")
df.plot <- df.plot[!is.na(df.plot$plotCode), ]
cat("done\n")

cat("- Merging in stratum codes...")
df.plot <- merge(df.plot, df.site[!is.na(df.site$stratumCode), c("siteCode", "stratumCode")],
  by = "siteCode", all.x = TRUE
)
cat("done\n")

cat("- Checking for missing stratum codes for non-benchmark plots...")
n.missing <- nrow(df.plot[is.na(df.plot$stratumCode) & !df.plot$assessCode == "B", ])
if (n.missing > 0) {
  msg <- paste0(n.missing, " plots are missing a stratum code!")
  stop_quietly(msg)
} else {
  cat("done\n")
}

cat("- Checking for missing assessment codes...")
n.missing <- nrow(df.plot[is.na(df.plot$assessCode), ])
if (n.missing > 0) {
  msg <- paste0(n.missing, " plots are missing a values for assessCode!")
  stop_quietly(msg)
} else {
  cat("done\n")
}

cat("- Checking for non-matching codes:\n")
msg.err <- "ERROR: not all codes match!"
cat("-- vegCode: df.plot vs. df.vegetation...")
if (all(unique(df.plot$vegCode) %in% unique(df.vegetation$vegCode))) {
  cat("passed\n")
} else {
  stop_quietly(msg.err)
}
cat("-- stratumCode: df.plot vs. df.stratum...")
if (all(unique(df.plot$stratumCode[!is.na(df.plot$stratumCode)])
%in% unique(df.stratum$stratumCode))) {
  cat("passed\n")
} else {
  stop_quietly(msg.err)
}
cat("-- siteCode: df.plot vs. df.site...")
if (all(unique(df.plot$siteCode) %in% unique(df.site$siteCode))) {
  cat("passed\n")
} else {
  stop_quietly(msg.err)
}

##########################################
#  Complete metadata in plot table
##########################################

# if (scope=="offset" ) {
#   df.plot$stratumCode <- df.plot$siteCode
# }

# Add veg name to table df.plot
cat("- Merging vegName from vegetation table into plot table...")
df.plot <- merge(
  x = df.plot[, c("plotCode", "assessCode", "siteCode", "stratumCode", "vegCode")],
  y = df.vegetation[, c("vegCode", "vegName", "lc.type")],
  by = "vegCode", all.x = TRUE
)
df.plot <- df.reorder(df.plot, col.move = "vegCode", move.last = TRUE)
df.plot <- df.reorder(df.plot, col.move = "vegName", move.last = TRUE)
df.plot <- df.reorder(df.plot, col.move = "lc.type", move.last = TRUE)
cat("done\n")

##########################################
#  Validate metadata
##########################################

cat("- Validating plot metadata:\n")

cat("-- Checking for non-benchmark plots without a stratum code...")
plot.strata.missing <- as.data.frame(df.plot[
  !df.plot$focalOrBenchmark == "b" & is.na(df.plot$stratumCode),
  c("plotCode")
])
n.plot.strata.missing <- nrow(plot.strata.missing)

if (n.plot.strata.missing > 0) {
  df.err <- plot.strata.missing
  err.file.name <- "err.plots_without_strata.csv"
  msg.err <- paste0("ERROR: ", n.plot.strata.missing, " non-bm plots have no value for stratumCode!")
  msg.action <- paste0("ACTION: codes of offending plots saved to file '", err.file.name, "'")
  df.err.save(df.err, err.file.name, err.file.path, msg.err, msg.action)
} else {
  cat("passed\n")
}

cat("-- Checking for plots without a treatment code...")
plot.assessCode.missing <- as.data.frame(df.plot[
  is.na(df.plot$assessCode),
  c("plotCode")
])
n.plot.assessCode.missing <- nrow(plot.assessCode.missing)

if (n.plot.assessCode.missing > 0) {
  df.err <- plot.assessCode.missing
  err.file.name <- "err.plots_without_assessCode.csv"
  msg.err <- paste0("ERROR: ", n.plot.assessCode.missing, " plots have no assessCode!")
  msg.action <- paste0("ACTION: codes of offending plots saved to file '", err.file.name, "'")
  df.err.save(df.err, err.file.name, err.file.path, msg.err, msg.action)
} else {
  cat("passed\n")
}

cat("-- Checking for invalid treatment codes...")
plot.assessCode.invalid <- as.data.frame(df.plot[
  !df.plot$assessCode %in% c("B", "MT", "C", "A", "O"),
  c("plotCode")
])
n.plot.assessCode.invalid <- nrow(plot.assessCode.invalid)

if (n.plot.assessCode.invalid > 0) {
  df.err <- plot.assessCode.invalid
  err.file.name <- "err.plots_assessCode_invalid.csv"
  msg.err <- paste0("ERROR: ", n.plot.assessCode.invalid, " plots have invalid assessCodes!")
  msg.action <- paste0("ACTION: codes of offending plots saved to file '", err.file.name, "'")
  df.err.save(df.err, err.file.name, err.file.path, msg.err, msg.action)
} else {
  cat("passed\n")
}

# Check for missing vegetation names (needed for final plot check below)
cat("-- Checking for vegetation with missing full vegetation name...")
vegetation.vegName.missing <- as.data.frame(df.vegetation[
  is.na(df.vegetation$vegName), c("vegCode")
])
n.vegetation.vegName.missing <- nrow(vegetation.vegName.missing)

if (n.vegetation.vegName.missing > 0) {
  df.err <- vegetation.vegName.missing
  err.file.name <- "err.vegetation_without_vegname.csv"
  msg.err <- paste0("ERROR: ", n.vegetation.vegName.missing, " rows have no value for vegCode in table vegetation!")
  msg.action <- paste0("ACTION: codes of offending vegetation units saved to file '", err.file.name, "'")
  df.err.save(df.err, err.file.name, err.file.path, msg.err, msg.action)
} else {
  cat("passed\n")
}

# Check assignment of plots to vegetation - Alternative version
cat("-- Checking for plots without a veg code...")
plot.veg.missing <- as.data.frame(df.plot[
  is.na(df.plot$vegCode), c("plotCode")
])
n.plot.veg.missing <- nrow(plot.veg.missing)

if (n.plot.veg.missing > 0) {
  df.err <- plot.strata.missing
  err.file.name <- "err.plots_without_veg.csv"
  msg.err <- paste0("ERROR: ", n.plot.veg.missing, " plots have no value for vegCode!")
  msg.action <- paste0("ACTION: codes of offending plots saved to file '", err.file.name, "'")
  df.err.save(df.err, err.file.name, err.file.path, msg.err, msg.action)
} else {
  cat("passed\n")
}

cat("-- Checking for unmatched veg codes in plot table...")
plot.veg.codes <- df.plot[
  !is.na(df.plot$vegCode),
  c("plotCode", "vegCode")
]
vegetation.veg.codes <- as.data.frame(df.vegetation$vegCode)
colnames(vegetation.veg.codes) <- "vegCode"
plot.veg.codes.unmatched <- merge(plot.veg.codes, vegetation.veg.codes, by = "vegCode", all.x = TRUE)
plot.veg.codes.unmatched <- plot.veg.codes.unmatched[is.na(plot.veg.codes.unmatched$vegCode), ]
plot.veg.codes.unmatched <- df.reorder(plot.veg.codes.unmatched,
  col.move = "plotCode",
  move.first = TRUE
)
n.plot.veg.codes.unmatched <- nrow(plot.veg.codes.unmatched)

if (n.plot.veg.codes.unmatched > 0) {
  df.err <- plot.veg.codes.unmatched
  err.file.name <- "err.plots_with_unmatched_vegcodes.csv"
  msg.err <- paste0("ERROR: ", n.plot.veg.codes.unmatched, " plots have values of vegCode unmatched in table df.vegetation!")
  msg.action <- paste0("ACTION: codes of offending plots saved to file '", err.file.name, "'")
  df.err.save(df.err, err.file.name, err.file.path, msg.err, msg.action)
} else {
  cat("passed\n")
}

# Check for missing vegName
# If above check passes but this one fails, STOP: you've
# got some weird invisible non-matching character issue
# Use the following to decode strings to character codes:
# sapply(strsplit("THE_STRING", NULL)[[1L]], utf8ToInt)
cat("-- Checking for plots without a veg code...")
plot.vegName.missing <- as.data.frame(df.plot[
  is.na(df.plot$vegName), c("plotCode")
])
n.plot.vegName.missing <- nrow(plot.vegName.missing)

if (n.plot.vegName.missing > 0) {
  msg.err <- paste0(n.plot.vegName.missing, " plots are missing a value for vegName. Stop and inspect carefully for invisible non-matching strings in plots versus vegetation tables")
  stop(msg.err)
} else {
  cat("passed\n")
}

##########################################
# Populate focalOrBenchmark
#
# Note treatment codes:
# 	B - Benchmark
# 	M - Project site Main (current) assessment
# 	C - Propject site baseline (control) assessment
# 	A - Afforestation site = Offset
# 	O - Afforestation site = Offset (synonym)
##########################################

# Populate and standardize focalOrBenchmark
cat("- Populating column focalOrBenchmark...")
df.plot$focalOrBenchmark <- NA
df.plot <- df.reorder(df.plot, col.move = "assessCode", col.before = "plotCode")
df.plot <- df.reorder(df.plot, col.move = "focalOrBenchmark", col.before = "assessCode")
df.plot$focalOrBenchmark[df.plot$assessCode == "B"] <- "b"
df.plot$focalOrBenchmark[df.plot$assessCode %in% c("M", "C", "A", "O")] <- "f"
if (!nrow(df.plot[is.na(df.plot$focalOrBenchmark), ]) == 0) {
  stop("ERROR: one or more rows not assigned value of focalOrBenchmark")
}
cat("done\n")

#################################### )
# Assign land cover and bm vegetation classes
####################################

cat("Assigning land cover and benchmark vegetation:\n")

cat("- Land cover (landCover):\n")
cat("-- Setting stratum landCover=stratumCode...")
df.stratum$landCover <- df.stratum$stratumCode
cat("done\n")
cat("-- Setting focal plot landCover=stratumCode...")
df.plot$landCover[df.plot$focalOrBenchmark == "f"] <- df.plot$stratumCode[df.plot$focalOrBenchmark == "f"]
cat("done\n")
cat("-- Setting benchmark plot landCover=vegCode...")
df.plot$landCover[df.plot$focalOrBenchmark == "b"] <- df.plot$vegCode[df.plot$focalOrBenchmark == "b"]
cat("done\n")

cat("- Benchmark vegetation (bm.veg):\n")
cat("-- Setting stratum bm.veg=vegCode...")
df.stratum$bm.veg <- df.stratum$vegCode
cat("done\n")
cat("-- Setting plot bm.veg=vegCode...")
df.plot$bm.veg <- df.plot$vegCode
cat("done\n")

####################################
# Add bm plots to offset plots
####################################

if (scope == "offset") {
  # Drop siteCode from main df to avoid mismatch
  df.plot <- df.plot[, !names(df.plot) == "siteCode"]

  df.plot.bm <- df.plotMetadata.bm
  colnames(df.plot.bm) <- c("plotCode", "focalOrBenchmark", "vegCode", "landCover")
  df.plot.bm$assessCode <- "B"
  df.plot.bm$stratumCode <- ""
  df.plot.bm$bm.veg <- df.plot.bm$vegCode
  df.plot.bm <- merge(df.plot.bm, df.vegetation[, c("vegCode", "vegName", "lc.type")],
    by = "vegCode", all.x = TRUE
  )
  df.plot.bm <- df.plot.bm[, c(
    "plotCode", "assessCode", "focalOrBenchmark",
    "stratumCode", "vegCode", "vegName",
    "lc.type", "landCover", "bm.veg"
  )]
  df.plot.bak <- df.plot
  df.plot <- rbind(df.plot, df.plot.bm)
}

####################################
# Create df lc.summary with landcover classes
# land plot sample sizes
####################################

cat("Creating land cover summary table (lc.summary):\n")

cat("- Creating lc.summary...")
# lc.summary <- df.stratum[ , c("landCover", "bm.veg")]	# Original version based on stratum
lc.summary <- unique(df.plot[df.plot$focalOrBenchmark == "f", c("landCover", "bm.veg")])
cat("done\n")

# Count focal plots
cat("- Counting focal plots...")
df.plot$cnt <- 1
lc.summary.f <- aggregate(
  cnt ~ landCover,
  data = df.plot[df.plot$focalOrBenchmark == "f", ],
  FUN = sum,
  na.rm = TRUE
)
names(lc.summary.f)[names(lc.summary.f) == "cnt"] <- "n.f.plots"
cat("done\n")

# Count bm plots
cat("- Counting bm plots...")
lc.summary.b <- aggregate(
  cnt ~ bm.veg,
  data = df.plot[df.plot$focalOrBenchmark == "b", ],
  FUN = sum,
  na.rm = TRUE
)
names(lc.summary.b)[names(lc.summary.b) == "cnt"] <- "n.b.plots"
lc.summary.b <- lc.summary.b[, c("bm.veg", "n.b.plots")]
cat("done\n")

cat("- Merging focal and benchmark plot counts into lc.summary...")
lc.summary <- merge(lc.summary, lc.summary.f, by = "landCover", all.x = TRUE)
lc.summary <- merge(lc.summary, lc.summary.b, by = "bm.veg", all.x = TRUE)
lc.summary$n.f.plots[is.na(lc.summary$n.f.plots)] <- 0
lc.summary$n.b.plots[is.na(lc.summary$n.b.plots)] <- 0
# Reorder the columns & sort
lc.summary <- df.reorder(lc.summary, col.move = "landCover", move.first = TRUE)
# lc.summary <- lc.summary[, c("landCover", "bm.veg", "n.f.plots", "n.b.plots")]
lc.summary <- lc.summary[order(lc.summary$landCover), ] # Sort by landCover
row.names(lc.summary) <- NULL # Reset row numbering
cat("done\n")

# Mark these in lc.summary table
cat("- Flagging land cover classes with f or b sample sizes<N.MIN...")
lc.summary$n.f.plots.n.min <- 0
lc.summary$n.b.plots.n.min <- 0
lc.summary$n.f.plots.n.min[lc.summary$n.f.plots >= N.MIN.ABS] <-
  lc.summary$n.f.plots[lc.summary$n.f.plots >= N.MIN.ABS]
lc.summary$n.b.plots.n.min[lc.summary$n.b.plots >= N.MIN.ABS] <-
  lc.summary$n.b.plots[lc.summary$n.b.plots >= N.MIN.ABS]
lc.summary$all.n.OK <- TRUE
lc.summary$all.n.OK[lc.summary$n.f.plots.veg < N.MIN.ABS |
  lc.summary$n.b.plots.veg < N.MIN.ABS] <- FALSE
cat("done\n")

cat("- Merging in lc.type from df.vegetation...")
veg.lc.type <- df.vegetation[, c("vegCode", "lc.type")]
colnames(veg.lc.type) <- c("bm.veg", "lc.type")
lc.summary <- merge(lc.summary, veg.lc.type, by = "bm.veg", all.x = TRUE)
lc.summary <- df.reorder(lc.summary, col.move = "lc.type", col.before = "landCover")
cat("done\n")

##########################################
##########################################
# Prepare cover data
##########################################
##########################################

cat("Preparing species cover data (df.scbsa):\n")

##########################################
# Cover per species per stratum in each plot
#
# Includes simplified stratum field and keys for aggregating by
# plot+stratum or plot+species
# df: df.scbsa
##########################################

cat("- Combining cover values in single column...")
df.scbsa <- df.cover[!is.na(df.cover$A1), c("plotCode", "species", "A1")]
names(df.scbsa)[names(df.scbsa) == "A1"] <- "coverBB"
df.scbsa$stratum <- "A1"
df.scbsaA1 <- df.scbsa

df.scbsa <- df.cover[!is.na(df.cover$A2), c("plotCode", "species", "A2")]
names(df.scbsa)[names(df.scbsa) == "A2"] <- "coverBB"
df.scbsa$stratum <- "A2"
df.scbsaA2 <- df.scbsa

df.scbsa <- df.cover[!is.na(df.cover$A3), c("plotCode", "species", "A3")]
names(df.scbsa)[names(df.scbsa) == "A3"] <- "coverBB"
df.scbsa$stratum <- "A3"
df.scbsaA3 <- df.scbsa

df.scbsa <- df.cover[!is.na(df.cover$A4), c("plotCode", "species", "A4")]
names(df.scbsa)[names(df.scbsa) == "A4"] <- "coverBB"
df.scbsa$stratum <- "A4"
df.scbsaA4 <- df.scbsa

df.scbsa <- df.cover[!is.na(df.cover$B1), c("plotCode", "species", "B1")]
names(df.scbsa)[names(df.scbsa) == "B1"] <- "coverBB"
df.scbsa$stratum <- "B1"
df.scbsaB1 <- df.scbsa

df.scbsa <- df.cover[!is.na(df.cover$B2), c("plotCode", "species", "B2")]
names(df.scbsa)[names(df.scbsa) == "B2"] <- "coverBB"
df.scbsa$stratum <- "B2"
df.scbsaB2 <- df.scbsa

df.scbsa <- df.cover[!is.na(df.cover$C), c("plotCode", "species", "C")]
names(df.scbsa)[names(df.scbsa) == "C"] <- "coverBB"
df.scbsa$stratum <- "C"
df.scbsaC <- df.scbsa

df.scbsa <- df.cover[!is.na(df.cover$D), c("plotCode", "species", "D")]
names(df.scbsa)[names(df.scbsa) == "D"] <- "coverBB"
df.scbsa$stratum <- "D"
df.scbsaD <- df.scbsa

df.scbsa <- df.scbsaA1
df.scbsa <- rbind(df.scbsa, df.scbsaA2)
df.scbsa <- rbind(df.scbsa, df.scbsaA3)
df.scbsa <- rbind(df.scbsa, df.scbsaA4)
df.scbsa <- rbind(df.scbsa, df.scbsaB1)
df.scbsa <- rbind(df.scbsa, df.scbsaB2)
df.scbsa <- rbind(df.scbsa, df.scbsaC)
df.scbsa <- rbind(df.scbsa, df.scbsaD)
df.scbsa <- df.reorder(df.scbsa, col.move = "coverBB", move.last = TRUE)
cat("done\n")

# Convert Braun Blanquet cover codes to percent cover midpoints
cat("- Converting Braun Blanquet codes to percent cover...")
df.scbsa <- within(df.scbsa, {
  percCover <- NA
  percCover[is.na(coverBB)] <- 0
  percCover[coverBB == "0"] <- 0
  percCover[coverBB == "r"] <- 0.1 # Present but rare
  percCover[coverBB == "+"] <- 0.5 # <1 %
  percCover[coverBB == "1"] <- 3 # 1-5%
  percCover[coverBB == "1a"] <- 2 # 1-2.5%
  percCover[coverBB == "1b"] <- 4 # 2.5-5%
  percCover[coverBB == "1d"] <- 4.5 #  Total guess
  percCover[coverBB == "1s"] <- 1.5 #  Total guess
  percCover[coverBB == "2"] <- 8.5 # 5-25%
  percCover[coverBB == "2m"] <- 4 # "Many but cover <5%" --> 4%
  percCover[coverBB == "2a"] <- 8.5 # 5-12%
  percCover[coverBB == "2b"] <- 18.5 # 12-25%
  percCover[coverBB == "3"] <- 37.5 # 25-50%
  percCover[coverBB == "3a"] <- 30 # 25-35%
  percCover[coverBB == "3b"] <- 42.5 # 35-50%
  percCover[coverBB == "4"] <- 62.5 # 50-75%
  percCover[coverBB == "5"] <- 87.5 # >75%
})
# df.scbsa$prop.cover <- df.scbsa$percCover / 100
cat("done\n")

# Add key field plot.species
cat("- Adding composite key plot.species...")
df.scbsa$plot.species <- paste0(
  df.scbsa$plotCode, "-", df.scbsa$species
)
df.scbsa <- df.scbsa[, !(names(df.scbsa) == "coverBB")]
df.scbsa <- df.reorder(df.scbsa, col.move = "plot.species", col.before = "species")
cat("done\n")


# Drop Braun-Blanquet column and separate into A, B, & C/D subsamples
df.scbsa.bak <- df.scbsa
df.scbsa.A.all <- df.scbsa[df.scbsa$stratum %in% c("A1", "A2", "A3", "A4"), ]
df.scbsa.B.all <- df.scbsa[df.scbsa$stratum %in% c("B1", "B2"), ]
df.scbsa.CD.all <- df.scbsa[df.scbsa$stratum %in% c("C", "D"), ]

# Sum cover in the A1-A4 quadrats as new stratum A,
# then scale over [0:100]
df.scbsa.A.cover <- aggregate(
  percCover ~ plot.species,
  data = df.scbsa.A.all,
  FUN = sum,
  na.rm = TRUE,
  na.pass = NULL
)
names(df.scbsa.A.cover)[names(df.scbsa.A.cover) == "percCover"] <- "percCover.summed"
df.scbsa.A <- unique(df.scbsa.A.all[, c("plotCode", "species", "plot.species")])
df.scbsa.A <- merge(df.scbsa.A, df.scbsa.A.cover, by = "plot.species", all.x = TRUE)
if (nrow(df.scbsa.A[is.na(df.scbsa.A$percCover.summed), ]) > 0) {
  # set null values of cover to zero
  df.scbsa.A$percCover.summed[is.na(df.scbsa.A$percCover.summed)] <- 0
}
# Scale summed percent cover back to [0:100] by dividing by total quadrats
df.scbsa.A$percCover <- df.scbsa.A$percCover.summed / 4
# Add the new stratum & tidy up
df.scbsa.A$stratum <- "A"
df.scbsa.A <- df.scbsa.A[, !names(df.scbsa.A) %in%
  c("percCover.summed", "plot.species")]
df.scbsa.A <- df.scbsa.A[, c("plotCode", "stratum", "species", "percCover")]

# Sum cover in the B1 and B2 quadrats as new stratum B,
# then scale over [0:100]
df.scbsa.B.cover <- aggregate(
  percCover ~ plot.species,
  data = df.scbsa.B.all,
  FUN = sum,
  na.rm = TRUE,
  na.pass = NULL
)
names(df.scbsa.B.cover)[names(df.scbsa.B.cover) == "percCover"] <- "percCover.summed"
df.scbsa.B <- unique(df.scbsa.B.all[, c("plotCode", "species", "plot.species")])
df.scbsa.B <- merge(df.scbsa.B, df.scbsa.B.cover, by = "plot.species", all.x = TRUE)
if (nrow(df.scbsa.B[is.na(df.scbsa.B$percCover.summed), ]) > 0) {
  # set null values of cover to zero
  df.scbsa.B$percCover.summed[is.na(df.scbsa.B$percCover.summed)] <- 0
}
# Scale summed percent cover back to [0:100] by dividing by total quadrats
df.scbsa.B$percCover <- df.scbsa.B$percCover.summed / 2
# Add the new stratum & tidy up
df.scbsa.B$stratum <- "B"
df.scbsa.B <- df.scbsa.B[, !names(df.scbsa.B) %in%
  c("percCover.summed", "plot.species")]
df.scbsa.B <- df.scbsa.B[, c("plotCode", "stratum", "species", "percCover")]

# Calculate aggregated cover of the combined C+D strata as the midpoint
# of the mean and the max. This approach is required because C is contained
# in D, and D is the entire plot. Therefore, (a) cover cannot be less than D percCover,
# and (b) cover cannot be greater than the sum of the two (assuming cover in D & C [scaled
# to the enire plot] are non-overlapping) truncated at 100
df.scbsa.CD.cover <- df.scbsa.CD.all %>%
  group_by(plot.species) %>%
  summarise(
    pc_max = max(percCover),
    pc_sum = sum(percCover) # ,
    # .groups = 'drop' # Drop the grouping structure after summary
  ) %>%
  as.data.frame()
df.scbsa.CD.cover$pc_exp <- (df.scbsa.CD.cover$pc_max + df.scbsa.CD.cover$pc_sum) / 2
df.scbsa.CD.cover$percCover <- pmin(df.scbsa.CD.cover$pc_exp, 100)
df.scbsa.CD <- unique(df.scbsa.CD.all[, c("plotCode", "species", "plot.species")])
df.scbsa.CD <- merge(df.scbsa.CD, df.scbsa.CD.cover, by = "plot.species", all.x = TRUE)
df.scbsa.CD$percCover[is.na(df.scbsa.CD$percCover)] <- 0 # NA cover to zero, if any
df.scbsa.CD$stratum <- "CD"
df.scbsa.CD <- df.scbsa.CD[, c("plotCode", "stratum", "species", "percCover")]

# Append all stratum subsets and sort
df.scbsa <- rbind(df.scbsa.A, df.scbsa.B)
df.scbsa <- rbind(df.scbsa, df.scbsa.CD)
df.scbsa <- df.scbsa[order(df.scbsa$plotCode, df.scbsa$stratum, df.scbsa$species), ]

cat("- Adding metadata fields...")
df.scbsa <- merge(df.scbsa, df.plot[, c("plotCode", "focalOrBenchmark")],
  by = "plotCode", all.x = TRUE
)
df.scbsa <- merge(df.scbsa, df.plot[, c("plotCode", "landCover", "bm.veg")],
  by = "plotCode", all.x = TRUE
)
cat("done\n")

# Add some composite key fields
# Add key field plot.species
cat("- Adding composite key fields...")
df.scbsa$plot.species <- paste0(df.scbsa$plotCode, "-", df.scbsa$species)
df.scbsa$plot.stratum <- paste0(df.scbsa$plotCode, "-", df.scbsa$stratum)
cat("done\n")

cat("- Adding metadata fields...")
# Species native status
# df.exotic.spp$is_exotic <- 1
df.scbsa <- merge(df.scbsa, df.exotic.spp[df.exotic.spp$is_exotic == 1, c("species", "is_exotic")],
  by = "species", all.x = TRUE
)
df.scbsa$is_exotic[is.na(df.scbsa$is_exotic)] <- 0
cat("done\n")

if (cover.stratum.names.convert == TRUE) {
  cat("- Converting stratum codes to stratum names...")
  # function cover.stratum.name() defined in
  # project-specific parameters file
  df.scbsa$stratum <- cover.stratum.name(df.scbsa$stratum)
  df.vegetation$lc.type <- vegCode.lc.type(df.vegetation$vegCode)
  cat("done\n")
}

cat("- Reordering and renaming columns...")
names(df.scbsa)[names(df.scbsa) == "bm.veg"] <- "vegClass"
names(df.scbsa)[names(df.scbsa) == "percCover"] <- "cover" # Use percent cover
df.scbsa <- df.scbsa[, c(
  "plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum",
  "species", "is_exotic", "cover",
  "plot.species", "plot.stratum"
)]
cat("done\n")

##########################################
##########################################
# Prepare structure data (df.stem)
#
# Create df.stem, with one line per stem, also
# keeping track of plot, species and individual
##########################################
##########################################

cat("Preparing stem data (df.stem):\n")

# ****************************************************
# IMPORTANT!
# Need more flexible method to accommodate different
# numbers of dbh columns, not just 10
# ****************************************************

cat("- Adding plot metadata to df.structure...")
df.structure <- merge(
  df.structure, df.plot[, c("plotCode", "focalOrBenchmark", "landCover", "vegCode")],
  by = "plotCode", all.x = TRUE
)
names(df.structure)[names(df.structure) == "vegCode"] <- "vegClass"
cat("done\n")

cat("- Adding column 'ind' to df.structure...")
df.structure$ind <- NA
df.structure <- df.reorder(df.structure, col.move = "ind", col.before = "zone")
str.plots <- unique(df.structure$plotCode)
for (p in str.plots) {
  ind <- nrow(df.structure[df.structure$plotCode == p, ])
  inds <- seq(1, ind)
  df.structure$ind[df.structure$plotCode == p] <- inds
}
cat("done\n")

cat("- Compiling individual trees and stem measurements in data frame df.stem...")
# stem.cols <- c("plotCode", "subplot", "zone", "ind", "species" )
stem.cols <- c("plotCode", "subplot", "ind", "species")

df.stem <- df.structure[!is.na(df.structure$dbh1), c(stem.cols, "dbh1")]
names(df.stem)[names(df.stem) == "dbh1"] <- "dbh"
df.stem1 <- df.stem

df.stem <- df.structure[!is.na(df.structure$dbh2), c(stem.cols, "dbh2")]
names(df.stem)[names(df.stem) == "dbh2"] <- "dbh"
df.stem2 <- df.stem

df.stem <- df.structure[!is.na(df.structure$dbh3), c(stem.cols, "dbh3")]
names(df.stem)[names(df.stem) == "dbh3"] <- "dbh"
df.stem3 <- df.stem

df.stem <- df.structure[!is.na(df.structure$dbh4), c(stem.cols, "dbh4")]
names(df.stem)[names(df.stem) == "dbh4"] <- "dbh"
df.stem4 <- df.stem

df.stem <- df.structure[!is.na(df.structure$dbh5), c(stem.cols, "dbh5")]
names(df.stem)[names(df.stem) == "dbh5"] <- "dbh"
df.stem5 <- df.stem

df.stem <- df.structure[!is.na(df.structure$dbh6), c(stem.cols, "dbh6")]
names(df.stem)[names(df.stem) == "dbh6"] <- "dbh"
df.stem6 <- df.stem

df.stem <- df.structure[!is.na(df.structure$dbh7), c(stem.cols, "dbh7")]
names(df.stem)[names(df.stem) == "dbh7"] <- "dbh"
df.stem7 <- df.stem

df.stem <- df.structure[!is.na(df.structure$dbh8), c(stem.cols, "dbh8")]
names(df.stem)[names(df.stem) == "dbh8"] <- "dbh"
df.stem8 <- df.stem

df.stem <- df.structure[!is.na(df.structure$dbh9), c(stem.cols, "dbh9")]
names(df.stem)[names(df.stem) == "dbh9"] <- "dbh"
df.stem9 <- df.stem

df.stem <- df.structure[!is.na(df.structure$dbh10), c(stem.cols, "dbh10")]
names(df.stem)[names(df.stem) == "dbh10"] <- "dbh"
df.stem10 <- df.stem

df.stem <- df.stem1
df.stem <- rbind(df.stem, df.stem2)
df.stem <- rbind(df.stem, df.stem3)
df.stem <- rbind(df.stem, df.stem4)
df.stem <- rbind(df.stem, df.stem5)
df.stem <- rbind(df.stem, df.stem6)
df.stem <- rbind(df.stem, df.stem7)
df.stem <- rbind(df.stem, df.stem8)
df.stem <- rbind(df.stem, df.stem9)
df.stem <- rbind(df.stem, df.stem10)
cat("done\n")

# Adding stratum (size class)
# Stratum names are in project-specific parameters
df.stem$stratum <- as.numeric(NA)
df.stem$stratum[df.stem$dbh < 10] <- stem.class.2.5to10
df.stem$stratum[df.stem$dbh >= 10] <- stem.class.ge.10

# # Basal area
# cat( "- Calculating basal area per stem..." )
# df.stem$ba.cm2 <- ( pi * df.stem$dbh^2 ) / 4
# df.stem$ba.m2 <- df.stem$ba.cm2 * 0.0001
# cat("done\n")

# Add unique individual id by combining "ind" with "plotCode"
cat("- Adding unique individual id plot.ind...")
df.stem$plot.ind <- paste0(
  df.stem$plotCode, "_", df.stem$ind
)
cat("done\n")

# Add integer id to df
df.stem$stem_id <- seq.int(nrow(df.stem))

cat("- Reordering columns...")
df.stem <- df.reorder(df.stem, col.move = "plot.ind", col.before = "ind")
df.stem <- df.reorder(df.stem, col.move = "stratum", col.before = "plot.ind")
df.stem <- df.reorder(df.stem, col.move = "stem_id", move.first = TRUE)
cat("done\n")

##########################################
##########################################
# Prepare VQA input data frames
##########################################
##########################################

cat("\n")
cat("******************************************\n")
cat("Preparing final VQA input data frames\n")
cat("******************************************\n")
cat("\n")

#######################################
# Land cover
# df: landCover
#######################################

cat("landCover:\n")

landCover <- df.landCover
colnames(landCover) <- c("landCover", "vegClass", "lc.type", "area_ha")

cat("- Adding sample size columns...")
# Get sample sizes from lc.summary
lc.n <- lc.summary[, c("landCover", "n.f.plots", "n.b.plots", "all.n.OK")]
landCover <- merge(landCover, lc.n, by = "landCover", all.x = TRUE)
cat("done\n")

cat("- Setting setting NA sample sizes to 0 and all.n.OK to FALSE ...")
landCover$n.f.plots[is.na(landCover$n.f.plots)] <- 0
landCover$n.b.plots[is.na(landCover$n.b.plots)] <- 0
landCover$all.n.OK[landCover$n.f.plots < N.MIN.ABS | landCover$n.b.plots < N.MIN.ABS] <- FALSE
cat("done\n")

cat("- Renaming and reordering columns...")
names(landCover)[names(landCover) == "n.f.plots"] <- "focal_plots"
names(landCover)[names(landCover) == "n.b.plots"] <- "bm_plots"
landCover <- landCover[, c(
  "landCover", "lc.type", "vegClass", "area_ha", "focal_plots", "bm_plots", "all.n.OK"
)]
cat("done\n")

cat("- Checking all land cover classes unique...")
n.rows <- nrow(landCover)
n.vegClasses <- length(unique(landCover$landCover))

if (n.vegClasses == n.rows) {
  cat("passed\n")
} else {
  stop_quietly("WARNING: 1 or more duplicated values of 'landCover' in df 'landcover'! ")
}

#######################################
# Benchmark vegetation
# df: vegetation
#######################################

cat("vegetation:\n")

cat("- Extracting final columns..")
vegetation <- df.vegetation
names(vegetation)[names(vegetation) == "vegCode"] <- "vegClass"
colnames(vegetation) <- c("vegClass", "lc.type", "vegName")
cat("done\n")

cat("- Pruning benchmark vegetation classes to those linked to land cover classes in current assessment...")
# Get sample sizes from lc.summary
lc.vegClasses <- unique(landCover$vegClass)
vegetation <- vegetation[vegetation$vegClass %in% lc.vegClasses, ]
cat("done\n")

#####################
# Species attributes
# df: species
#####################

cat("species...")
species <- df.species
# names(species)[names(species) == 'user_id'] <- "species_id"

# Set final columns
# species <- species[ , c("species_id", "family", "genus", "species", "is_exotic")]
species <- species[, c("genus", "species", "is_exotic")]
cat("done\n")

#################
# Plot metadata
# df: plotMetadata
#################

cat("plotMetadata...")
plotMetadata <- unique(df.plot[, c("plotCode", "focalOrBenchmark", "bm.veg", "landCover")])
# Rename fields
names(plotMetadata)[names(plotMetadata) == "plotCode"] <- "plotCode"
names(plotMetadata)[names(plotMetadata) == "bm.veg"] <- "vegClass"
names(plotMetadata)[names(plotMetadata) == "landCover"] <- "landCover"

# Make vector of basic plot metadata columns for later use
plot.meta.cols <- c(
  "plotCode", "focalOrBenchmark",
  "landCover", "vegClass"
)

cat("done\n")

#####################
# Stems, individuals and species
# df: speciesStems
#####################

cat("speciesStems:\n")

cat("- Adding and populating stratum column based on df.stratum...")
speciesStems <- df.stem
cat("done\n")

# Merge in plot metadata, rename and prune columns
cat("- Merging in plot metadata and formatting final data frame...")
speciesStems <- merge(speciesStems, plotMetadata, by = "plotCode")
names(speciesStems)[names(speciesStems) == "dbh"] <- "dbh_cm"
names(speciesStems)[names(speciesStems) == "plot.ind"] <- "ind_id"
speciesStems <- df.reorder(speciesStems, col.move = "stem_id", move.first = TRUE)
speciesStems <- df.reorder(speciesStems, col.move = "focalOrBenchmark", col.before = "plotCode")
speciesStems <- df.reorder(speciesStems, col.move = "landCover", col.before = "focalOrBenchmark")
speciesStems <- df.reorder(speciesStems, col.move = "vegClass", col.before = "landCover")
cat("done\n")

# If offset assessment, add bm stem data
if (scope == "offset") {
  cat("- Appending benchmark data from main assessment...")

  # Remove basal area column so fields match
  df.speciesStems.bm2 <- df.speciesStems.bm[, !names(df.speciesStems.bm) == "ba_m2"]

  # Reset stem_id to continue after last offset record
  last_id <- max(speciesStems$stem_id)
  df.speciesStems.bm2 <- df.speciesStems.bm2[order(
    df.speciesStems.bm2$vegClass, df.speciesStems.bm2$plotCode,
    df.speciesStems.bm2$plotCode, df.speciesStems.bm2$subplot,
    df.speciesStems.bm2$ind, df.speciesStems.bm2$stem_id
  ), ]
  df.speciesStems.bm2$stem_id <- seq(last_id + 1:nrow(df.speciesStems.bm2))

  # Append the benchmark data
  speciesStems <- rbind(speciesStems, df.speciesStems.bm2)
  cat("done\n")
}

##########################################
# Cover per species per *plot* (not strata)
#
# Species cover aggregated at plot level.
# Cover is MAX cover among all strata.
# df: speciesCover
# Indicators: SR, PCES
##########################################

cat("speciesCover:\n")

cat("- Aggregating cover by plot+species using max() function...")
plot.species.cover <- aggregate(
  cover ~ plot.species,
  data = df.scbsa,
  FUN = max,
  na.rm = TRUE,
  na.pass = NULL
)
speciesCoverAll <- unique(df.scbsa[
  ,
  c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "species", "plot.species")
])
speciesCoverAll <- merge(speciesCoverAll, plot.species.cover, by = "plot.species", all.x = TRUE)
cat("done\n")

cat("- Setting NA cover to 0...")
speciesCoverAll$cover[is.na(speciesCoverAll$cover)] <- 0
cat("done\n")

cat("- Restructuring the data frame...")
# Make the final dfs
speciesCoverAll <- speciesCoverAll[
  ,
  c(
    "plotCode", "focalOrBenchmark", "landCover", "vegClass",
    "species", "cover"
  )
]
speciesCover <- speciesCoverAll
cat("done\n")

cat("- Converting percent to proportions...")
# Convert cover values to proportions
speciesCover <- speciesCover[!is.na(speciesCover$cover), ] # Remove NAs, if any still left
speciesCover$cover <- speciesCover$cover / 100 # convert to proportions
cat("done\n")

cat("- Removing unnecessary columns...")
# Remove unwanted columns
speciesCover <- speciesCover[
  ,
  c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "species", "cover")
]
cat("done\n")

# If offset assessment, add bm cover data
if (scope == "offset") {
  cat("- Appending benchmark data from main assessment...")
  df.speciesCover.bm <- df.speciesCover.bm[order(
    df.speciesCover.bm$vegClass, df.speciesCover.bm$plotCode,
    df.speciesCover.bm$species
  ), ]
  speciesCover <- rbind(speciesCover, df.speciesCover.bm)
  cat("done\n")
}

##########################################
# Species cover by stratum
#
# * This is simply the original species cover
# in the original strata (but combining any
# within-stratum subsamples. E.g., uses cover
# in stratum A, not in A1, A2, A3, and A4.
# * Include column is_exotic
# df: speciesCoverByStratum
# Indicators: PCGF
##########################################

cat("speciesCoverByStratum...")
speciesCoverByStratum <- df.scbsa[, c(
  "plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "species",
  "is_exotic", "cover"
)]
# Convert cover to proportions
speciesCoverByStratum$cover <-
  speciesCoverByStratum$cover / 100
cat("done\n")

#############################################
# Percent cover exotic species by stratum
#
# Total cover of all exotic species SUMMED in each
# stratum, regardless of species.
# If total cover exceeds 100%, total cover is
# scaled over [0:100], relative to the maximum
# value of cover observed among all plots.
# species not included
#
# df: exoticCoverByStratum
# Indicators: SR, PCES
#############################################

cat("exoticCoverByStratum:\n")

# Cover: Exclude native species
cat("- Extracting cover data for non-native species only...")
df.scbsaExotic <- df.scbsa[df.scbsa$is_exotic == 1, ]
cat("done\n")

cat("- Aggregating cover by plot+strata using sum() function...")
# Aggregate exotic cover by plot+strata
plot.stratum.cover.exotic <- aggregate(
  cover ~ plot.stratum,
  data = df.scbsaExotic,
  FUN = sum,
  na.rm = TRUE,
  na.pass = NULL
)
exoticCoverByStratum <- unique(df.scbsa[, c(
  "plotCode", "stratum",
  "plot.stratum", "focalOrBenchmark", "landCover", "vegClass"
)])
exoticCoverByStratum <- merge(
  exoticCoverByStratum, plot.stratum.cover.exotic,
  by = "plot.stratum", all.x = TRUE
)
exoticCoverByStratum$cover[is.na(exoticCoverByStratum$cover)] <- 0
cat("done\n")

cat("- Removing NA rows...")
exoticCoverByStratum <- exoticCoverByStratum[
  !is.na(exoticCoverByStratum$focalOrBenchmark),
]
cat("done\n")

if (max(plot.stratum.cover.exotic$cover) > 100) {
  cat("- Scaling percent cover over [0:100]...")
  # Scale percent cover over [0:100] *if* percCover>100%
  # Note temporary column cover.orig
  exoticCoverByStratum$percCover <- exoticCoverByStratum$cover
  pc.raw <- unique(exoticCoverByStratum$percCover)
  # pc.min <- min(pc.raw)
  pc.min <- 0 # Avoid making lowest value = 0
  pc.max <- max(pc.raw) + 1 # Avoid issues at exactly 1
  exoticCoverByStratum$cover <- as.numeric(NA)

  for (i in 1:nrow(exoticCoverByStratum)) {
    pc <- exoticCoverByStratum$percCover[i]
    pc.norm <- (pc - pc.min) / (pc.max - pc.min)
    exoticCoverByStratum$cover[i] <- pc.norm
  }
  cat("done\n")
}

# Set final column names
names(exoticCoverByStratum)[names(exoticCoverByStratum) == "percCover"] <- "cover"

if (max(exoticCoverByStratum$cover) > 1) {
  # Convert cover to proportions
  cat("- Converting percent to proportions...")
  exoticCoverByStratum$cover <- exoticCoverByStratum$cover / 100
  cat("done\n")
}

cat("- Removing unnecessary columns...")
# Save the relevant columns only
exoticCoverByStratum <- exoticCoverByStratum[
  ,
  c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "cover")
]
cat("done\n")

# Tidy up
rm(df.scbsaExotic, plot.stratum.cover.exotic)

# If offset assessment, add bm cover data
if (scope == "offset") {
  cat("- Appending benchmark data from main assessment...")
  df.exoticCoverByStratum.bm <- df.exoticCoverByStratum.bm[order(
    df.exoticCoverByStratum.bm$vegClass, df.exoticCoverByStratum.bm$plotCode,
    df.exoticCoverByStratum.bm$stratum
  ), ]
  exoticCoverByStratum <- rbind(exoticCoverByStratum, df.exoticCoverByStratum.bm)
  cat("done\n")
}

##########################################################
# Percent cover by stratum (=Percent cover by growth form)
#
# Total cover of all species SUMMED in each stratum,
# regardless of species.
# If total cover exceeds 100%, total cover is
# scaled over [0:100], relative to the maximum
# value of cover observed among all plots.
#
# df: coverByStratum (=coverByGrowthForm)
# Indicators: PCGF
##########################################################

cat("coverByGrowthForm:\n")

cat("- Aggregating cover by plot+strata using sum() function...")
# Sum cover by plot+strata
plot.stratum.cover <- aggregate(
  cover ~ plot.stratum,
  data = df.scbsa,
  FUN = sum,
  na.rm = TRUE,
  na.pass = NULL
)

# Make new df of unique values of plot + stratum only
coverByStratum <- unique(df.scbsa[
  ,
  c("plotCode", "stratum", "plot.stratum", "focalOrBenchmark", "landCover", "vegClass")
])

# Merge the two together
coverByStratum <- merge(
  coverByStratum, plot.stratum.cover,
  by = "plot.stratum", all.x = TRUE
)
cat("done\n")

cat("- Setting NA values of cover to 0...")
coverByStratum$cover[is.na(coverByStratum$cover)] <- 0
coverByStratum <- coverByStratum[!is.na(coverByStratum$focalOrBenchmark), ]
cat("done\n")

if (max(coverByStratum$cover) > 100) {
  cat("- Scaling percent cover over [0:100]...")
  # Scale percent cover over [0:100] *if* percCover>100%
  coverByStratum$percCover <- coverByStratum$cover
  pc.raw <- unique(coverByStratum$percCover)
  # pc.min <- min(pc.raw)
  pc.min <- 0 # Set lowest value = 0
  pc.max <- max(pc.raw) + 1 # Avoid issues at exactly 100%
  coverByStratum$cover <- as.numeric(NA)

  for (i in 1:nrow(coverByStratum)) {
    pc <- coverByStratum$percCover[i]
    pc.norm <- (pc - pc.min) / (pc.max - pc.min)
    coverByStratum$cover[i] <- pc.norm
  }
  cat("done\n")
}

# Save the relevant columns
coverByStratum <- coverByStratum[
  ,
  c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "cover")
]

# Tidy up
rm(plot.stratum.cover)

if (max(coverByStratum$cover) > 1) {
  # Convert cover to proportions
  cat("- Converting percent to proportion...")
  coverByStratum$cover <- coverByStratum$cover / 100
  cat("done\n")
}

# Rename the df (workaround until develop separate import for coverByStratum)
cat("- Renaming coverByStratum to coverByGrowthForm")
coverByGrowthForm <- coverByStratum
cat("done\n")

# Remove problematic height strata, if applicable
# See discussion of this parameter in project-
# specific parameters file
if (length(PCGF.STRATA.OMIT) > 0) {
  cat("- Checking for problematic growth forms...")
  gf.omitted <- intersect(PCGF.STRATA.OMIT, unique(coverByGrowthForm$stratum))
  gf.omitted <- paste(gf.omitted, collapse = ", ")
  n.gf.omitted <- length(gf.omitted)
  gf <- "growth form"
  if (n.gf.omitted > 1) gf <- "growth forms"

  if (n.gf.omitted > 0) {
    cat("WARNING: removing ", gf, " ", gf.omitted, "...", sep = "")
    coverByGrowthForm <- coverByGrowthForm[!coverByGrowthForm$stratum %in% PCGF.STRATA.OMIT, ]
    cat("done\n")
  } else {
    cat("none found\n")
  }
}

# If offset assessment, add bm cover data
if (scope == "offset") {
  cat("- Appending benchmark data from main assessment...")
  df.coverByGrowthForm.bm <- df.coverByGrowthForm.bm[order(
    df.coverByGrowthForm.bm$vegClass, df.coverByGrowthForm.bm$plotCode,
    df.coverByGrowthForm.bm$stratum
  ), ]
  coverByGrowthForm <- rbind(coverByGrowthForm, df.coverByGrowthForm.bm)
  cat("done\n")
}

##########################################
##########################################
# Summarize per-indicator sample sizes
##########################################
##########################################

cat("\n")
cat("******************************************\n")
cat("Preparing final land cover summary tables\n")
cat("******************************************\n")
cat("\n")

############################################
# Prepare detailed sample size summary
#
# Make detailed summary by land cover class of actual, final
# sample sizes for each indicator, plus new determination of
# land  cover classes with n>N.MIN for all indicators
############################################

cat("Preparing indicator-specific sample size summary (lc.summary.detailed):\n")

cat("- Extracting basic table from lc.summary...")
lc.summary.detailed <- lc.summary[
  ,
  c("bm.veg", "landCover", "lc.type", "n.f.plots", "n.b.plots")
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
# Add input df-specific sample sizes
#

cat("- Adding input df-specific sample size:\n")

cat("-- speciesStems (stem data)...")
# focal plots
df.uniq <- unique(speciesStems[, c("plotCode", "focalOrBenchmark", "landCover")])
if (nrow(df.uniq[df.uniq$focalOrBenchmark == "f", ] > 0)) {
  n.f.df <- aggregate(
    plotCode ~ landCover,
    data = df.uniq[df.uniq$focalOrBenchmark == "f", ],
    FUN = length
  )
  colnames(n.f.df) <- c("landCover", "n.f.speciesStems")
  lc.summary.detailed <- merge(lc.summary.detailed, n.f.df, by = "landCover", all.x = TRUE)
  lc.summary.detailed$n.f.speciesStems[is.na(lc.summary.detailed$n.f.speciesStems)] <- 0
} else {
  # No focal plots have stem data
  # Insert column of n=0
  lc.summary.detailed$n.f.speciesStems <- 0
}
# bm plots
df.uniq <- unique(speciesStems[, c("plotCode", "focalOrBenchmark", "vegClass")])
n.b.df <- aggregate(
  plotCode ~ vegClass,
  data = df.uniq[df.uniq$focalOrBenchmark == "b", ],
  FUN = length
)
colnames(n.b.df) <- c("bm.veg", "n.b.speciesStems")
lc.summary.detailed <- merge(lc.summary.detailed, n.b.df, by = "bm.veg", all.x = TRUE)
lc.summary.detailed$n.b.speciesStems[is.na(lc.summary.detailed$n.b.speciesStems)] <- 0
cat("done\n")

cat("-- coverByGrowthForm (cover data)...")
# focal plots
df.uniq <- unique(coverByGrowthForm[, c("plotCode", "focalOrBenchmark", "landCover")])
n.f.df <- aggregate(
  plotCode ~ landCover,
  data = df.uniq[df.uniq$focalOrBenchmark == "f", ],
  FUN = length
)
colnames(n.f.df) <- c("landCover", "n.f.coverByStratum")
lc.summary.detailed <- merge(lc.summary.detailed, n.f.df, by = "landCover", all.x = TRUE)
lc.summary.detailed$n.f.coverByStratum[is.na(lc.summary.detailed$n.f.coverByStratum)] <- 0
# bm plots
df.uniq <- unique(coverByGrowthForm[, c("plotCode", "focalOrBenchmark", "vegClass")])
n.b.df <- aggregate(
  plotCode ~ vegClass,
  data = df.uniq[df.uniq$focalOrBenchmark == "b", ],
  FUN = length
)
colnames(n.b.df) <- c("bm.veg", "n.b.coverByStratum")
lc.summary.detailed <- merge(lc.summary.detailed, n.b.df, by = "bm.veg", all.x = TRUE)
lc.summary.detailed$n.b.coverByStratum[is.na(lc.summary.detailed$n.b.coverByStratum)] <- 0
cat("done\n")

#
# Add indicator-specific sample sizes
#

cat("- Adding indicator-specific sample sizes...")

# Species Richness
col.n.f <- "n.f.SR"
col.n.b <- "n.b.SR"
ind.plots <- as.data.frame(unique(speciesCover[, c("plotCode"), drop = FALSE]))
lc.summary.detailed <- lc.detailed.update(
  lc.summary.detailed,
  plot.meta.fb, ind.plots, col.n.f, col.n.b
)

# TD
col.n.f <- "n.f.TD"
col.n.b <- "n.b.TD"
ind.plots <- as.data.frame(unique(speciesCover[, c("plotCode"), drop = FALSE]))
lc.summary.detailed <- lc.detailed.update(
  lc.summary.detailed,
  plot.meta.fb, ind.plots, col.n.f, col.n.b
)

# PCESS
col.n.f <- "n.f.PCESS"
col.n.b <- "n.b.PCESS"
ind.plots <- as.data.frame(unique(exoticCoverByStratum[, c("plotCode"), drop = FALSE]))
lc.summary.detailed <- lc.detailed.update(
  lc.summary.detailed,
  plot.meta.fb, ind.plots, col.n.f, col.n.b
)

# PCGF
# Note df name "coverByGrowthForm" instead of "coverByStratum",
# specific to this projects tap-gr and tap-al
col.n.f <- "n.f.PCGF"
col.n.b <- "n.b.PCGF"
ind.plots <- as.data.frame(unique(coverByGrowthForm[, c("plotCode"), drop = FALSE]))
lc.summary.detailed <- lc.detailed.update(
  lc.summary.detailed,
  plot.meta.fb, ind.plots, col.n.f, col.n.b
)

cat("done\n")

######################################
# Reorder and rename land cover summary tables
######################################

cat("Restructuring land cover summary tables...")

lc.summary <- df.reorder(lc.summary, col.move = "landCover", move.first = TRUE)

# Standardize area column names in case not done already
names(lc.summary)[names(lc.summary) %in% c("ha", "Ha")] <- "area_ha"

# Add area of each land cover class
df.area <- df.stratum[, c("landCover", "ha")]
names(df.area)[names(df.area) %in% c("ha", "Ha")] <- "area_ha"
lc.summary <- merge(lc.summary, df.area, by = "landCover", all.x = TRUE)
n.missing <- nrow(lc.summary[is.na(lc.summary$area_ha), ])
if (n.missing > 0) stop("One or more values of 'area_ha' missing for lc.summary!\n")

# lc.summary  <- within(lc.summary, rm(all.n.OK.orig))
lc.summary <- df.reorder(lc.summary, col.move = "area_ha", col.before = "lc.type")

# lc.summary.detailed gets fancy column names
lc.summary.detailed <- df.reorder(lc.summary.detailed, col.move = "landCover", move.first = TRUE)
names(lc.summary.detailed)[names(lc.summary.detailed) == "landCover"] <- "Land cover class"
names(lc.summary.detailed)[names(lc.summary.detailed) == "bm.veg"] <- "Benchmark vegetation"
names(lc.summary.detailed)[names(lc.summary.detailed) == "lc.type"] <- "Vegetation structural type"

cat("done\n")

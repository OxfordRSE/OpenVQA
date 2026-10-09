################################################
# Import raw VQA data
#
# Runs custom import script for a specific project &
# assessment, as set in params.R and the project-specific
# parameters file.
#
# Requirements:
# 1. Parameters proj and assess in file "pa.params.R" must be set
# 2. Raw data present in raw data directory
# 3. Check that all project-specific parameters in file
#    params.<proj>.<assess>.R are correct.
#
# Date created: 2024-12-09
# Author: Brad Boyle (brad@hg-llc.com)
################################################

#########################################
# Set paths, load functions & parameters
#########################################

rm(list = ls())

if (!exists("params.loaded", mode = "function")) {
  # Set working directory (base application directory)
  # Must set first to set remaining directory parameters
  wd <- getwd()
  setwd(wd)
  SRCDIR <- paste0(wd, "/")
}

# Set job so params.R know which parameters to load
job <- "import"

# Load functions
source("includes/functions.R", local = TRUE)

# Load parameters if not already loaded
# Also loads project-specific parameters
# file in directory params/
# Load global params file if not already loaded
# if(!exists("global.params.loaded", mode="function")) source(paste0(SRCDIR, "params.R"))
source(paste0(SRCDIR, "params.R"))

# Set project-specific import file name
ps.import.file.name <- paste0("import.", PROJ, ".R")

if (exists("IMPORT.USE.ASSESS")) {
  if (IMPORT.USE.ASSESS == TRUE) {
    ps.import.file.name <- paste0("import.", PROJ, ".", ASSESS, ".R")
  }
}

# Display parameters & confirm operation
cat(paste0("Import raw data for project '", PROJ, "', assessment '", ASSESS, "' using the following settings: \n", sep = ""))
cat(paste0(MSG.CONF.START, MSG.CONF.IMP))

if (interactive() == FALSE) {
  yes <- c("y", "Y", "Yes", "yes")
  cat("Continue? (y/n):")
  response <- readLines("stdin", n = 1)
  if (!response %in% yes) stop_quietly("Operation cancelled\n\n")
}

cat("\n##########################################\n")
cat("Begin operation\n\n")

source("libraries.R")

# if ( QH.METHOD=="empirical" ) {
###########################################################
# Prepare vector of standard VQA input data frames to be
# generated for this project/assessment.
# Preparing df.input.list here allows it to be accessed from
# inside project-specific import file.
############################################################
cat("Assembling vectors of input data frames to prepare...")
df.input.list.comb <- input_file_list_all(DF.LANDCOVER, DF.PLOTMETADATA, DF.SPECIES, EI.vec)
df.input.list.a <- df.input.list.comb[[1]]
df.input.list.b <- df.input.list.comb[[2]]
df.input.list <- df.input.list.comb[[3]]
cat("done\n")
# }

######################################
######################################
# Project-specific import
######################################
######################################

# Run the project-specific import script, if applicable
# ps.import.file <- paste0("import/", ps.import.file.name)
ps.import.file <- paste0(BASEDIR_PSFILES, "import/", ps.import.file.name)

cat("\n")
cat("**********************************************\n")
cat("**********************************************\n")
cat("Performing project-specific import operations\n")
cat("**********************************************\n")
cat("**********************************************\n")
cat("\n")

cat("Loading project-specific import file: '", ps.import.file, "'...", sep = "")
if (file.exists(ps.import.file)) {
  cat("done\n")
  source(ps.import.file)
} else {
  stop("file not found!\n")
}

######################################
######################################
# Final generic import steps
######################################
#####################################

cat("\n")
cat("******************************************\n")
cat("******************************************\n")
cat("Performing generic import operations\n")
cat("*******************************************\n")
cat("*******************************************\n")
cat("\n")

if (QH.METHOD == "empirical") { # START QH.METHOD=="empirical" #2
  # Update df.input.list if applicable
  if (exists("vegetation")) {
    # Keep this condition until back-update all projects & assessments
    df.input.list.a <- c(DF.LANDCOVER, DF.VEG, DF.PLOTMETADATA, DF.SPECIES)
    df.input.list <- c(df.input.list.a, df.input.list.b)
  }

  ######################################
  # Check required data frames present
  #####################################

  cat("Checking all required input data frames present:\n")
  df.missing <- FALSE
  for (df.input in df.input.list) {
    cat("  ", df.input, "...", sep = "")
    if (exists(df.input)) {
      cat("done\n")
    } else {
      cat("MISSING!\n")
      df.missing <- TRUE
    }
  }

  if (df.missing == TRUE) {
    msg <- "One or more required data frames missing!\n"
    msg <- paste0(msg, "Please ensure that all of the above data frames\n")
    msg <- paste0(msg, "are generated by project-specific import script.\n")
    stop(msg)
  }

  ############################################
  # Check that all cover field are proportions,
  # not percentages. Stop and throw error message
  # if one or more dfs contain percentages
  ############################################

  # Prune indicator data frames
  cat("Checking input data frames for illegal values of 'cover':\n")
  any.bad.cover <- FALSE

  for (df.input in df.input.list.b) {
    cmd <- paste0("curr.df <- ", df.input)
    eval(parse(text = cmd))
    df.input.name <- eval(parse(text = "df.input"))

    if (df.input.name %in% c(
      "coverByGrowthForm", "exoticCoverByStratum", "groundCover",
      "speciesCover", "speciesCoverByStratum"
    )) {
      cat(paste0("- ", df.input, "..."))
      cmd <- paste0("curr.df <- ", df.input)
      eval(parse(text = cmd))

      # cover.vals <- unique(exoticCoverByStratum$cover[ !is.na(exoticCoverByStratum$cover) ] )
      cmd <- paste0("cover.vals <- unique(", df.input, "$cover[ !is.na(", df.input, "$cover) ] )")
      eval(parse(text = cmd))
      cover.max <- max(cover.vals)
      cover.min <- min(cover.vals)

      cat("(min=", cover.min, ", max=", cover.max, ")...", sep = "")

      if (all(cover.vals >= 0) && all(cover.vals <= 1)) {
        cat("OK\n")
      } else {
        cat("FAILED: one or more values of cover <0 or >1!\n")
        any.bad.cover <- TRUE
      }
    }
  }

  if (any.bad.cover == TRUE) {
    msg <- "(fatal) One or more input data frames have values of cover outside range [0:1]\n"
    msg <- paste0(msg, "Please convert all percentages to proportions.\n\n")
    msg <- paste0(msg, "If the percent cover method allows total cover to sum to >100%,\n")
    msg <- paste0(msg, "then rescale to a maximum cover of 100% or truncate at 100%.\n\n")
    # cat(msg)
    stop(msg)
  }

  ############################################
  # Prune plots and data for all land cover
  # classes having one or more indicators
  # with sample sizes below N.MIN.ABS.
  # Also prune any orphan benchmark plots &
  # their data. Requires "df.plot" from
  # project-specific import script
  ############################################

  if (DELETE.LC.BELOW.N.MIN.ABS == TRUE) {
    cat("Removing data for plots with land cover having sample sizes below N.MIN.ABS:\n")

    # Make list of land cover classes to keep
    cat("- Preparing list of land cover classes to keep...")
    lc.keep <- unique(lc.summary[
      lc.summary$all.n.OK == TRUE,
      c("landCover")
    ])
    cat("done\n")

    # Remove focal plots by landCover & make vector of focal plots to keep
    cat("- Dropping landCover classes with plot sample sizes below N.MIN.ABS...")
    bad.lc <- unique(landCover$landCover[landCover$all.n.OK == FALSE])
    landCover <- landCover[!landCover$landCover %in% bad.lc, ]
    cat("done\n")

    # Remove focal plots by landCover & make vector of focal plots to keep
    cat("- Dropping plots from 'plotMetadata':\n")
    cat("-- Dropping focal plots by landCover class...")
    plotMetadata <- plotMetadata[(plotMetadata$landCover %in% lc.keep & plotMetadata$focalOrBenchmark == "f") | plotMetadata$focalOrBenchmark == "b", ]
    f.plots.keep <- unique(plotMetadata$plotCode[plotMetadata$focalOrBenchmark == "f"])
    cat("done\n")

    # Now identify and drop orphan benchmark plots
    cat("-- Dropping orphan benchmark plots...")
    bm.plots.all <- plotMetadata[plotMetadata$focalOrBenchmark == "b", c("plotCode", "vegClass")]
    f.vegClasses <- as.data.frame(unique(plotMetadata$vegClass[plotMetadata$focalOrBenchmark == "f"]))
    colnames(f.vegClasses) <- "vegClass"
    df.bm.plots.keep <- merge(bm.plots.all, f.vegClasses)
    bm.plots.keep <- df.bm.plots.keep$plotCode
    plotMetadata <- plotMetadata[(plotMetadata$plotCode %in% bm.plots.keep & plotMetadata$focalOrBenchmark == "b") | plotMetadata$focalOrBenchmark == "f", ]
    cat("done\n")

    # Make vector of all plots to keep
    cat("-- Preparing list of plots to keep...")
    plots.keep <- c(f.plots.keep, bm.plots.keep)
    cat("done\n")

    # Prune indicator data frames
    cat("- Dropping plots from indicator data frames:\n")
    for (df.input in df.input.list.b) {
      cat(paste0("-- ", df.input, "..."))
      cmd <- paste0(df.input, " <- ", df.input, "[ ", df.input, "$plotCode %in% plots.keep, ]")
      eval(parse(text = cmd))
      cat("done\n")
    }
  }

  ############################################
  # Add basal area to speciesStems if exists
  ############################################

  if ("speciesStems" %in% df.input.list) {
    cat("Calculating basal area and adding to df speciesStems...")
    speciesStems$ba_m2 <- pi * ((speciesStems$dbh_cm / 2)^2) / 10000
    speciesStems <- df.reorder(speciesStems, col.move = "ba_m2", col.before = "dbh_cm")
    cat("done\n")
  }

  ####################################################
  # Generate new whitelist and blacklist files
  #
  # blacklist: All indicator + stratum combinations.
  #   Can be edited to exclude specific indicators or
  #   indicator + stratum combinations.
  # whitelist: All indicator + stratum + landCover
  #   combinations. Can be edited to include indicator +
  #   stratum + landCover combinations excluded by the
  #   blacklist.
  ####################################################

  cat("Preparing indicator-stratum include files:\n")
  # Check if include files exist
  # Creates both if only one missing
  cat("- Checking if include files exist...")
  f.blacklist <- paste0(INPUTDIR, BLACKLIST.FILE)
  f.whitelist <- paste0(INPUTDIR, WHITELIST.FILE)
  include.files.missing <- FALSE
  if (!file.exists(f.blacklist) || !file.exists(f.whitelist)) {
    include.files.missing <- TRUE
    cat("files not found\n")
  } else {
    cat("files found\n")
  }

  # Prepare message based on above and value of REPLACE.INCLUDE.FILES
  if (include.files.missing == TRUE) {
    cat("- Creating new files...")
  } else {
    if (REPLACE.INCLUDE.FILES == TRUE) {
      cat("- Replacing existing files (REPLACE.INCLUDE.FILES==TRUE)...")
    } else {
      cat("- Keeping existing files (REPLACE.INCLUDE.FILES==FALSE)\n")
    }
  }

  if (REPLACE.INCLUDE.FILES == TRUE || include.files.missing == TRUE) {
    n.EIs <- length(EI.vec)

    for (i in 1:n.EIs) {
      ei <- EI.vec[i]
      has.stratum <- ei.params(ei)$has.stratum
      df.input <- ei.params(ei)$df.input
      fg.ei <- df.EI.FG[df.EI.FG == ei, "f.group"]

      # Form the data frames for current ei
      if (has.stratum == FALSE) {
        # Blacklist row(s) for this ei (simple: only one row)
        df.blacklist.ei <- data.frame(t(c(ei, "nostrata", fg.ei)))
        colnames(df.blacklist.ei) <- c("EI", "stratum", "f.group")
        df.blacklist.ei$include <- "T" # Include all
        df.blacklist.ei$edit.notes <- ""

        # Whitelist row(s) for this ei (get all bm veg classes from df.ei)
        cmd <- paste0("veg.ei <- unique(", df.input, "$vegClass)")
        eval(parse(text = cmd))
        reps <- length(veg.ei)
        df.whitelist.ei <- cbind(as.data.frame(rep(ei, reps)), as.data.frame(rep("nostrata", reps)), as.data.frame(veg.ei))
        colnames(df.whitelist.ei) <- c("EI", "stratum", "bm.vegetation")

        # Default for 'include' is blank. Only set to true (T) if restoring
        # following previous exclusion by blacklist
        df.whitelist.ei$include <- ""
        df.whitelist.ei$edit.notes <- ""
      } else {
        # Blacklist row(s) for this ei (get all strata from df.input)
        cmd <- paste0("ei.strata <- unique(", df.input, "$stratum)")
        eval(parse(text = cmd))
        reps <- length(ei.strata)
        df.blacklist.ei <- cbind(as.data.frame(rep(ei, reps)), ei.strata, as.data.frame(rep(fg.ei, reps)))
        colnames(df.blacklist.ei) <- c("EI", "stratum", "f.group")
        df.blacklist.ei$include <- "T" # Include all
        df.blacklist.ei$edit.notes <- ""

        # Whitelist row(s) for this ei (get all bm veg classes from df.ei)
        cmd <- paste0("stratum.veg.ei <- unique( ", df.input, "[ , c('stratum', 'vegClass') ] )")
        eval(parse(text = cmd))
        reps <- nrow(stratum.veg.ei)
        df.whitelist.ei <- cbind(as.data.frame(rep(ei, reps)), stratum.veg.ei)
        colnames(df.whitelist.ei) <- c("EI", "stratum", "bm.vegetation")

        # Default for 'include' is blank. Only set to true (T) if restoring
        # following previous exclusion by blacklist
        df.whitelist.ei$include <- ""
        df.whitelist.ei$edit.notes <- ""
      }

      # Build the combined data frames
      if (i == 1) {
        # Start the data frames
        df.blacklist <- df.blacklist.ei
        df.whitelist <- df.whitelist.ei
      } else {
        # Append to existing
        df.blacklist <- rbind(df.blacklist, df.blacklist.ei)
        df.whitelist <- rbind(df.whitelist, df.whitelist.ei)
      }
    }
    cat("done\n")

    # # Save the files
    # cat("- Saving include files:\n")
    # cat("-- '", f.blacklist, "'\n", sep="")
    # write.csv(df.blacklist, file=f.blacklist, row.names=FALSE)
    # cat("-- '", f.whitelist, "'\n", sep="")
    # write.csv(df.whitelist, file=f.whitelist, row.names=FALSE)
  }
} # END QH.METHOD=="empirical" #2

############################################
# Save the finished data files to data directory
############################################

cat("Saving VQA input files to directory '", INPUTDIR, "':\n")

# Save landCover / stratum definition file to "landCover.csv"
# Do for all, including QH.METHOD=="pristine"
if (exists("landCover")) {
  # Remove embedded commas in area_ha, just in case
  landCover$area_ha <- as.numeric(gsub(",", "", landCover$area_ha))
  filename <- LANDCOVER.FILE
  fileandpath <- paste0(INPUTDIR, filename)
  # cat(paste0("- ", DF.LANDCOVER, " as '", LANDCOVER.FILE, "'\n"))
  cat(paste0("- ", DF.LANDCOVER, " as '", filename, "'\n"))
  # write.csv(landCover, file=fileandpath, row.names=FALSE)
  suppressMessages(write_excel_csv(landCover, file = fileandpath))
  file.done <- filename
} else if (exists("df.strata")) {
  # Handles synonym parameters
  # Get rid of this after checking comptibility with all projects
  df.strata$area_ha <- as.numeric(gsub(",", "", df.strata$area_ha))
  filename <- STRATA.FILE
  fileandpath <- paste0(INPUTDIR, filename)
  cat(paste0("- ", DF.STRATA, " as '", filename, "'\n"))
  # write.csv(df.strata, file=fileandpath, row.names=FALSE)
  suppressMessages(write_excel_csv(df.strata, file = fileandpath))
  file.done <- filename
}

if (QH.METHOD == "empirical") { # START QH.METHOD=="empirical" #23
  # Save remaining files relevant to empirical analysis

  for (df.input in df.input.list) {
    filename <- paste0(df.input, ".csv")

    if (!filename == file.done) {
      fileandpath <- paste0(INPUTDIR, filename)
      cat(paste0("- ", df.input, " as '", filename, "'\n"))
      # cmd <- paste0( "write.csv(", df.input, ", file='", fileandpath, "', row.names=FALSE)" )
      cmd <- paste0("suppressMessages( write_excel_csv(", df.input, ", file='", fileandpath, "') )")
      eval(parse(text = cmd))
    }
  }

  if (REPLACE.INCLUDE.FILES == TRUE || include.files.missing == TRUE) {
    cat("Saving blacklist/whitelist files to directory '", INPUTDIR, "':\n", sep = "")
    fileandpath <- paste0(INPUTDIR, BLACKLIST.FILE)
    # write.csv(df.blacklist, file=fileandpath, row.names=FALSE)
    suppressMessages(write_excel_csv(df.blacklist, file = fileandpath))
    cat(paste0("- df.blacklist as '", BLACKLIST.FILE, "'\n"))

    fileandpath <- paste0(INPUTDIR, WHITELIST.FILE)
    # write.csv(df.whitelist, file=fileandpath, row.names=FALSE)
    suppressMessages(write_excel_csv(df.whitelist, file = fileandpath))
    cat(paste0("- df.whitelist as '", WHITELIST.FILE, "'\n"))
  }

  if (exists("df.plot.zero.cover")) {
    cat("Saving zero cover plots to directory '", RESULTSDIR, "':\n", sep = "")
    zero.cover.plots.file <- "plots.zero.cover.csv"
    fileandpath <- paste0(RESULTSDIR, zero.cover.plots.file)
    # write.csv(df.plot.zero.cover, file=fileandpath, row.names=FALSE)
    suppressMessages(write_excel_csv(df.plot.zero.cover, file = fileandpath))
    cat(paste0("- df.plot.zero.cover as '", zero.cover.plots.file, "'\n"))
  }

  cat("Saving summary files to directory '", RESULTSDIR, "':\n", sep = "")

  fileandpath <- paste0(RESULTSDIR, LC.SUMMARY.FILE)
  # write.csv(lc.summary, file=fileandpath, row.names=FALSE)
  suppressMessages(write_excel_csv(lc.summary, file = fileandpath))
  cat(paste0("- lc.summary as '", LC.SUMMARY.FILE, "'\n"))

  if (exists("lc.summary.detailed")) {
    fileandpath <- paste0(RESULTSDIR, LC.SUMMARY.DETAILED.FILE)
    # write.csv(lc.summary.detailed, file=fileandpath, row.names=FALSE)
    suppressMessages(write_excel_csv(lc.summary.detailed, file = fileandpath))
    cat(paste0("- lc.summary.detailed as '", LC.SUMMARY.DETAILED.FILE, "'\n"))
  }
} # END QH.METHOD=="empirical" #3

cat("\nOperation completed\n")
cat("##########################################\n")

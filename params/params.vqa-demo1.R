##################################################
##################################################
# Teck DPm Mine Test Offset Assessment
# (Resolute Averted Loss offset)
#
# Version: 2026-03-09-02
# DB version: 2025-02-27
#
# Author: Brad Boyle (brad@hg-llc.com)
###################################################

#################################################
#################################################
#################################################
# Project-specific parameters
#
# These parameters are unique to this project
# (and assessment, if applicable). They are not
# found in the general parameters file (params.R).
# Most are called only in project_specific import
# script (import.<PROJ>.R)
#################################################
#################################################
#################################################

# Vectors of all synonyms for the same assessments
# Synonyms allow testing of an assessment in a different
# temporary directory, separate from production run
ASSESSLIST.MAIN.BASELINE <- c(
  "test_baseline",
  "main_000_baseline"
)
ASSESSLIST.MAIN.CURRENT <- c(
  "test_current",
  "main_001_current"
)
ASSESSLIST.OFFSET.BASELINE <- c(
  "test_offset_baseline",
  "offset_000_baseline"
)
ASSESSLIST.OFFSET.CURRENT <- c(
  "test_offset_current",
  "offset_001_current"
)

########################################
# Set scope
# Values:
# baseline|current|offset_baseline|offset_baseline
# IMPORTANT: update assessment vectors to include
# the names of any new assessments added!
########################################

# Set scope based on assessment code
if (ASSESS %in% ASSESSLIST.MAIN.BASELINE) {
  scope <- "baseline"
} else if (ASSESS %in% ASSESSLIST.MAIN.CURRENT) {
  scope <- "current"
} else if (ASSESS %in% ASSESSLIST.OFFSET.BASELINE) {
  scope <- "offset_baseline"
} else if (ASSESS %in% ASSESSLIST.OFFSET.CURRENT) {
  scope <- "offset_current"
} else {
  msg.err <- paste0("Assessment code ASSESS='", ASSESS, ", not not matched! Parameter 'scope' not set.")
  stop_quietly(msg.err)
}

# Update landCover and bm veg names to custom short codes?
# See code transformations in ps parameters file (search
# 'use.lc.bm.short.codes')
use.lc.bm.short.codes <- TRUE

# Error file names
# Define as needed
# Each error file should contain ID of rows with errors, as described in main error file
# File name includes name of original table and columns, where applicable
# Naming convention:
#   err_TABLE-NAME_ERROR-DESCRIPTION.csv
# Note:
# * Variable components in CAPS
# * Components separated by period
# * Multiple words within a component separated by underscore or hyphen
# * Include file extension
file.dupe.plot.codes <- "err.plot.data.dupe.PlotNumber.csv"
file.veg.cover.missing <- "err.veg.data.cover.missing.csv"
file.seral.missing <- "err.plot.data.seral.missing.csv"
file.plots.bm.disturbed <- "err.bm.plots-disturbed.csv"

#########################
# Land cover parameters
#########################

# Set to TRUE to set reclation code portion ', r.pl' to ' (reclaimed)'
# Set to FALSE if not applicable (no reclamation in dataset)
# This is a Teck Elk Valley-specific setting, will not apply to other applications
# and may not apply to future Teck EV applications if coding conventions
# change
reclaimed_make_pretty <- FALSE

#############################
# Ecosystem types (eg.type)
#
# Assigns land cover classes
# to "eg.type" groups during import.
# Eg.type affects handling during
# summary operations in
# vqa.summary.R
#############################

# Strictly anthropogenic land cover classes.
# These are typically assigned an a priori
# quality of zero during QH calculations.
eg.anthro <- c(
  "Road surface",
  "Industrial Corridor",
  "Reclaimed mine",
  "Mine",
  "Rubble",
  "Exposed soil",
  "Gravel pit",
  "Reservoir",
  "Rural"
)

# Natural but non-vegetated land cover classes.
# These are typically excluded
# from all VQA QH calculations
eg.non.veg.strict <- c(
  "Pond/Shallow Open Water",
  "River",
  "Disclimax",
  "Lake"
)

# Anthropogenic + natural non-vegetated
# Ecosystem Groupings (EGs)
# For simpler handing in operations where
# both classes are handled similarity
eg.non.veg <- c(eg.anthro, eg.non.veg.strict)

# Forested vegetation classes.
# Vegetation type only; do not
# include seral stage
eg.forested <- c(
  "Balsam Fir/Black Spruce Forest",
  "White Birch Forest",
  "White Spruce Forest",
  "Wet forest"
)

# Natural vegetation with insufficient data
# Used to distinguish from anthropogenic land cover classes.
# For final summary tables.
veg.nodata <- ""

#########################
# Plot parameters
#########################

# Ignore conflicting values of PlotStatus and DisturbanceHistory
# for 3 plots at DPm and set all to "Undisturbed"
# Obviously highly assessment-specific. Check before
# using this option in future assessments.
dpm_conflicting_disturbance_set_undisturbed <- TRUE

# Bad plots
# plotCodes of any known bad plots
# These are always removed
# Not affect by value of bad.plots.delete
known.bad.plots <- c(
  "DP23-12" # bm plot with only one species (Kalmia)
)

# Delete plots with duplicate plot codes?
dupe.plots.delete <- FALSE

# Abort if duplicate plots found?
# If FALSE will warn only & continue
dupe.plots.abort <- TRUE

# Delete "bad" plots
# These are plots that fail validation for any of several reasons
# Metadata for this plots is save to df with name beginning "bad.plots."
# Should be TRUE for production run
bad.plots.delete <- TRUE
bad.plots.delete <- FALSE # Warn but continue

# Delete plots in non-vegetated (anthropogenic, water, etc) land cover classes?
# TRUE | FALSE
# For initial counts of samples size, set to FALSE
# For final production run of VQA imput files, you MUST set to TRUE,
# otherwise VQA will crash
non.veg.plots.delete <- TRUE # production run
non.veg.plots.delete <- FALSE # testing & full sample size summaries

# Include seral stage in benchmark for forested vegetation
# TRUE | FALSE
# Be careful when setting to FALSE: this will obscure benchmark
# plots in successional vegetation. If the intent is to compare to a
# benchmark of mature/old growth vegetation only, then early- and
# mid-seral benchmark plots should be flagged and removed,
# not inadvertently used because they could not be detected!
bm.forested.include.seral <- TRUE

# Append disturbance code ' [Undisturbed]' to undisturbed land.cover
# vegetation classes and all benchmark vegetation classes?
# If FALSE, lack of an explicit "Undisturbed" disturbance code
# is interpreted as undisturbed
# TRUE | FALSE
dist.code.append.undisturbed <- FALSE

#########################
# Vegetation data parameters
#########################

# Use simplified strata for columns stratum and plot.stratum?
# If FALSE, use verbatim strata
# If TRUE, use simplified strata (Herbs, Bryophytes, Shrubs, Trees)
# My need additional coding to support new (verbatim) strata
# Be sure to check stratum mappings for df speciesCoverByStratumAll
use.stratum.simple <- TRUE

# TRUE: Merge species by latin name
# FALSE: merge by common name
spp.merge.by.latin <- FALSE

#########################
# Indicator input file parameters
#########################

# Delete rows in pcess (indicator PCESS) with NA or 0 perc cover?
# TRUE | FALSE
# If FALSE, will set NA perc cover to 0
pcess.na.delete <- FALSE

# Delete rows in gc with NA perc cover?
# TRUE | FALSE
# If FALSE, will set NA perc cover to 0
gc.na.delete <- FALSE

# Delete rows in gc with -999 for one or more ground cover indicators?
# If FALSE, set -999 to 0
gc.missing.val.delete <- TRUE

# Delete rows in pcgf with NA perc cover?
# TRUE | FALSE
# If FALSE, will set NA perc cover to 0
pcgf.na.delete <- FALSE

# Delete rows in spp.cov with NA cover?
# TRUE | FALSE
# If FALSE, will set NA perc cover to 0
spp.cov.na.delete <- TRUE

# Delete rows in spp.cov with perc cover=0?
# TRUE | FALSE
# If FALSE, will keep rows with perc cover=0
spp.cov.zero.delete <- TRUE

#################################################
#################################################
#################################################
# General parameters
#
# Parameter *values* unique to this project. The
# parameters themselves are also found in the
# general parameters file (params.R). Values set
# here over-ride values set previously in params.R.
#################################################
#################################################
#################################################

######################################
######################################
# DIRECTORY OPTIONS
#
# All are set relative to BASEDIR, which
# is defined at the start of this file.
# You should not have to change these
######################################
######################################

# Raw data directory
# Default: RAWDATADIR <- paste0( DATA_BASEDIR, 'raw/')
# You can change this to use data stored at different location
RAWDATADIR <- paste0(DATA_BASEDIR_PROJ, "raw/") # Shared raw/ directory

#####################################
#####################################
# Raw data
#
# Input for "import.R" (first step in pipeline)
# All should be in RAWDATADIR
# If files are further buried in subdirectories, be
# sure to prepend the path to the file name,
# relative  to RAWDATADIR
#####################################
#####################################

# DATA.TYPE (cover|ind|mixed)
DATA.TYPE <- "cover"

# Raw plot data file name
# Main and offset in same file
RAW_PLOTDATA_FILENAME <- "vqa-demo1_data.accdb"

# File name of raw land cover master list
# If no file, set to ""
RAW_LANDCOVER_FILENAME <- ""

# Species attributes data file name
# If separate file or database of species attributes will
# be imported, list it here. If no file, set to ""
RAW_SPP_FILENAME <- ""

#########################
#########################
# Import options
#########################
#########################

# Make new include files? (BLACKLIST.FILE, WHITELIST.FILE)
# TRUE: Replace existing files on import, including all
#   indicators and strata present in the data
# FALSE: reuse existing include files. Use this
#   option to preserve previously created include
#   files with manual edits
# For first-time imports, include files are detected
# as missing from the inputs/ folder and generated
# automatically without checking this parameter.
# For subsequent imports, existing include files
# are preserved (i.e., not replaced) unless this
# parameter is set to TRUE
# Generally set to FALSE.
# Only set to TRUE when repeating import and
# wish to retain previous include files.
REPLACE.INCLUDE.FILES <- FALSE

####################################
####################################
# vqa.batch OPTIONS
####################################
####################################

# Calculate all indicators from scratch?
# TRUE: Calculate raw indicator values for all indicators.
# FALSE: Values in ei.params determine action taken.
# Generally, set to FALSE after first run, as indicator
# values don't change after initial calculation, and the
# latter operation can be very time consuming.
force.prepare.raw <- TRUE

# Run indicator TD only?
# TRUE: TD only
# FALSE: run all indicators in EI.vec
# Set to TRUE when doing multiple TD runs to
# adjust NMDS parameters (see below)
# ***IMPORTANT NOTE***
# If you run import with TD.ONLY<-TRUE,
# the include files will contain only indicator TD.
# You will need to run the import again with TD=FALSE
# to rebuild the full include files with all indicators.
TD.ONLY <- FALSE

# Remove outlier plots?
# TRUE: delete plots with outliers in TD score
# Such plots may be very early successionk, or they may be misclassified--in which
# case they should be reclassified to a different land cover class (and possibly
# bm veg). TD outlier plots can crash quality calculations for indicator TD, and
# may distort quality scores (generally, inflating them).
# Generally leave REMOVE.TD.OUTLIERS<-TRUE. Outliers will be saved to outlier
# file (F.TD.OUTLIERS) on the first run of td.R, and new outliers appended
# to this file on subsequent runs, until no new outliers are found.
# Generally, keep -re-running td.R until no new outliers are found.
REMOVE.TD.OUTLIERS <- TRUE

# Number of standard deviations from mean NMDS
# score for plot to qualify as an NMDS outlier
# Only put this as low as needed to remove outliers
# which are causing calculation of TD to crash. If
# no crashes, but outliers are being removed, either
# increase this value or set REMOVE.TD.OUTLIERS <- FALSE
# Recommend start at 4 and work down
TD.OUTLIER.STDEVS <- 4

# Plot figures only?
# TRUE=just plot figures, skip all calculations
# FALSE=do everything (calculations and figures)
PLOT.FIGS.ONLY <- FALSE

# Run final vqa summary only
# All processed data and quality scores
# must be present and complete to run this option
VQA.SUMMARY.ONLY <- FALSE

# Calculate Quality only (TRUE|FALSE)
# TRUE: Omit Quality Hectares calculations from final summary
# FALSE: Include Quality Hectares (default)
QH.OMIT <- TRUE

# Import raw data from scratch?
IMPORT <- FALSE

##############################
##############################
# MODEL OPTIONS
##############################
##############################


# What to group by when calculating function group quality?
# Values: indicator|indicator_group
#  indicator: group by individual Q.i values in land cover + F.group class
#  indicator_group: Aggregate Q.i (indicators quality) to Q.ig (indicator group quality)
#    within each indicator group (EI) + land cover + F.group class, then aggregate the Q.ig
#    by land cover + F.group. This remove the excessive weight of indicators group with
#    many component indicator (=strata)
Q.FG.GROUP.BY <- "indicator"

# Type of mean used to aggregate indicator qualities (Q.i).
# Also, indicator group qualities (Q.ig) if Q.FG.GROUP.BY==
# "indicator_group".
# Values: arithmetic|generalized_mean
# If Q.FG.METHOD=="generalized_mean" uses custom function
# generalized_mean(), which behaves like geometric
# mean, but with a less severe penalty for low values; also,
# it drops to zero only if *all* input values are zero.
# As called in VQA, generalized_mean() uses the default
# value of p=3, which applies a moderate penalty for low values.
# See functions.R.
Q.FG.METHOD <- "arithmetic"

# Overall quality calculation method
# Values: gmean|prod
#   gmean: Geometric mean of all Q.fg
#   prod: Product of all Q.fg
#   mixed: (geomean of all non-integrity indicators) * (geomean of all integrity indicators)
Q.OVERALL.METHOD <- "gmean"

# Include plots with no data for a given indicator
# Exact actions are indicator-specific, but for most
# indicators this means insert a row for the plot,
# with an indicator value of zero. For indicators with
# strata, this mean insert a row for each plot+stratum
# combination, plus an indicator value of zero.
# Fixes issues of inflated quality scores due to
# ommission of highly disturbed / early succession
# plots with no regeneration in some or all strata
# Values: TRUE|FALSE
# Keeping FALSE as default for backwards-
# compatibility with early applications, prior
# to the additiona of this feature/bugfix
INCLUDE.PLOTS.NODATA <- TRUE

# Standard deviation penalties to apply when calculating
# indicator values of no-data plots
# Used for indicators where 0 is not the default value
# Ignored if INCLUDE.PLOTS.NODATA == TRUE
NODATA.SD <- 3 # Number of standard deviations of no-data value

# Bootstrap replicates
boot.reps <- 10 # For rapid testing only
boot.reps <- 10000 # For production run with accurate CLs; slow
boot.reps <- 100 # For trial run with approximate CLs

##############################################
##############################################
# Quality Hectares Assessment Method
#
# Values:
#   "empirical" [default]: Determine Q and QH
#     empirically using plot data.
#   "assume.1": Q=1 (100%) and QH=actualHa for all
#     vegetation.
#   "assume.0": Assume Q=0 and QH=0 for all vegetation
#
# Almost always, QH.METHOD="empirical"
# QH.METHOD="assume.1" is used only for
#   baseline assessments where no empirical data
#   are available and only justifiable assumption
#   is that all vegetation was pristine.
# QH.METHOD="assume.0": For (a) project baseline
#   assessment where entire area was destroyed by
#   project, or (b) project baseline assessment
#   where empirical data not available but impacts
#   are so extensive that empirical measurement
#   of the remaining QH is not cost-effective,
#   or (c) offset baseline assessment averted-loss
#   scenario, where the counterfactual is complete
#   and permanent destruction of offset biodiversity
#   value.
#
# This parameter affects data import (import.R),
# the main VQA pipleline (vqa.batch.R) and
# Net Quality Hectares (qh.net.R)
##############################################
##############################################

if (scope == "baseline") {
  QH.METHOD <- "empirical" # Determine quality from data
} else if (scope == "current") {
  QH.METHOD <- "empirical" # Determine quality from data
} else if (scope == "offset_baseline") {
  QH.METHOD <- "assume.0" # Assume all quality=0 (100% degraded)
} else if (scope == "offset_current") {
  QH.METHOD <- "empirical" # Determine quality from data
}

##########################################################
##########################################################
# FIGURE OPTIONS
##########################################################
##########################################################

# Type of figure file
# Options: "pdf", "png" (just these for now; pdf for hi res only)
# If using pdf, may need to adjust additional parameters currently in graph.dists.R
FIG.TYPE <- "pdf"
FIG.TYPE <- "png"

# Set to false to omit legends entirely from indicator histograms
plot.legends <- TRUE

# Set to FALSE to omit focal and benchmark color key from legend
LEGEND.NO.FB <- FALSE

# Include sample sizes in focal and benchmark color key?
show.n <- FALSE

# Combine all legends on right side of histogram
legend.combined <- FALSE

# Which set of figures to print?
# Selected options only.
# See main params file for full set of options
plot.dists_fitted_grouped_rescaled <- FALSE # Enable if any indicators have strata
plot.dists_fitted_rescaled <- TRUE # Default single figures

#####################################
# Intermediate data file names
# * Temporary files produced by this script
# * Include extension in file name
#####################################

# Table for translating species codes to species names
SPECIES_CODES.FILE <- "species_codes.txt"

####################################
####################################
# INDICATOR OPTIONS
# Review carefully!
####################################
####################################

##################################
##################################
# Functional groups
# List of all indicators and functional groups to which they belong
# CRITICAL! Indicators not listed here will not be included in
# final tables!
##################################
##################################

EI.F.GROUPS <- t(as.data.frame(list(
  c("SR", "composition"),
  c("TD", "composition"),
  c("PCGF", "structure"),
  c("GC", "function"),
  c("PCESS", "integrity")
)))
if (TD.ONLY == TRUE) {
  # Temporarily reset for this run if TD.ONLY
  EI.F.GROUPS <- t(as.data.frame(list(
    c("TD", "composition")
  )))
}
rownames(EI.F.GROUPS) <- NULL
colnames(EI.F.GROUPS) <- c("EI", "f.group")
df.EI.FG <- as.data.frame(EI.F.GROUPS)
EI.list <- data.frame(fname = EI.F.GROUPS[, 1])
EI.vec <- EI.F.GROUPS[, 1]

##################################
# Aggregate indicators
# Ecological Indicators (abbreviations) for which stratum
# scores will be combined in results files
# Set EI.agg.list to NA if not applicable for this analysis
##################################

EI.agg.list <- data.frame(fname = c(
  "PCGF",
  "PCESS",
  "GC"
))
# # Removing PCESS as only 1 stratum used (Herb)
# EI.agg.list <- data.frame(fname=c(
#   'PCGF',
#   'GC'
# ))

# Include only stratum "Herbs" in indicator PCESS?
# Also allows "Hierba", "Hierbas", etc.
pcess.herbs.only <- TRUE

##################################
# Indicator parameters
#
# Make sure all indicators in
# current analysis are included
# REVIEW CAREFULLY!
##################################
ei.params <- function(ei.code) {
  ####################################
  # Returns all parameters associated
  # with a given indicator (EI)
  #
  # * 'ei.code' is the indicator code,
  #   not the name (e.g., use 'SR',
  #   not 'Species Ricness').
  # * For indicators which are one of
  #   several strata of an indicator group,
  #   submit the indicator group code only.
  #   For example, for indicator "Percent
  #   Cover Trees", submit the code of
  #   indicator group PCGF (Percent Cover
  #   by Growth Form)
  # * Parameter bm.val only required if
  #   q.method=='fixed', otherwise NA.
  # * "source.file" is the name of the
  #   input file used to calculate indicator
  #   values
  ####################################

  #############################
  # Default values
  #############################

  convert.percent <- FALSE
  remove.zero.cover.plots <- FALSE
  prepare.raw <- FALSE # All over-ridden by force.prepare.raw
  ei.data.type <- DATA.TYPE # Global variable accessible here due to R scoping

  # Transformation: convert raw abundance to proportional (TRUE|FALSE)
  # abundance, relative to maximum abundance.
  # *** Important: MUST be set to TRUE if values are absolute abundance ***
  # Generally set to FALSE for percent cover, unless some cover values>100%
  scale.abund <- TRUE

  # Logit transformation, for proportions only
  # Generally a bad idea, esp. for NMDS, & not needed for overlap
  # MUST be FALSE for absolute abundance
  logit <- FALSE

  # Initialize TD-specific indicators
  td.multiplier <- NA
  td.multiplier.omit <- ""

  # Exclude non-native species from calculations
  # for current indicator?
  exclude.exotics <- FALSE

  # Scalar multiplier for this indicator
  # Transformation used to improve distribution fitting
  # Default MUST be 1 to avoid distorting all indicators
  # Make changes ONLY inside specific indicator
  # parameter sets
  ei.multiplier <- 1

  # Scale indicators to values expected
  # in subsamples of same area==normalize.m2?
  # Corrects for use of different sample
  # areas for different stem sizes (DBH classes).
  # Applies only to indicators for which DATA.TYPE="ind".
  # normalize.m2 <- 10000 for 1 ha.
  # normalize.m2 <- FALSE (default) turns
  #   off normalization
  normalize.m2 <- FALSE

  # Set NA values to 0 for this indicator
  # Values: TRUE|FALSE
  #  TRUE: Issue warning, set NA values to 0, and continue
  #  FALSE: Report error and stop
  NA.TO.ZERO <- TRUE

  ####################################
  # Indicator-specific values
  # Override defaults as needed
  ####################################

  if (ei.code == "GC") {
    ei.name <- "Ground Cover"
    ei.name.with.units <- ei.name
    distn <- "Bet"
    q.method <- "empirical"
    has.stratum <- TRUE
    test.tail <- "both"
    bm.val <- NA
    proportions <- TRUE # FALSE if raw data are percent

    # Input data frame and file for this indicator
    ei.data.type <- "cover" # Always cover, by definition
    df.input <- "groundCover"
    source.file <- paste0(df.input, ".csv") # Input file required for this indicator
  } else if (ei.code == "PCESS") {
    # Percent cover exotic species, separately by stratum
    ei.name <- "Percent Cover Exotic Species"
    ei.name.with.units <- ei.name
    distn <- "Bet"
    q.method <- "fixed" # c('empirical','fixed')
    has.stratum <- TRUE
    test.tail <- "upper"
    bm.val <- 0 # should be integer if q.method='fixed', otherwise numeric
    proportions <- TRUE # FALSE if raw data are percent
    remove.zero.cover.plots <- FALSE # all-zero cover possible for this EI
    NA.TO.ZERO <- TRUE # Set NA to 0 for this indicator?

    # Input data frame and file for this indicator
    ei.data.type <- "cover" # Always cover, by definition
    df.input <- "exoticCoverByStratum"
    source.file <- paste0(df.input, ".csv") # Input file required for this indicator
  } else if (ei.code == "PCGF") {
    ei.name <- "Percent Cover by Growth Form"
    ei.name.with.units <- ei.name
    distn <- "Bet"
    proportions <- TRUE # FALSE if raw data are percent
    q.method <- "empirical" # c('empirical','fixed')
    has.stratum <- TRUE
    test.tail <- "both"
    bm.val <- NA
    remove.zero.cover.plots <- FALSE # all-zero cover possible for this EI

    # Input data frame and file for this indicator
    ei.data.type <- "cover" # Always cover, by definition
    df.input <- "coverByGrowthForm"
    source.file <- paste0(df.input, ".csv") # Input file required for this indicator
  } else if (ei.code == "SR") {
    ei.name <- "Species Richness"
    ei.name.with.units <- ei.name
    distn <- "NBin"
    q.method <- "empirical"
    has.stratum <- FALSE
    test.tail <- "lower"
    bm.val <- NA

    # Input data frame and file for this indicator
    ei.data.type <- DATA.TYPE
    if (ei.data.type == "ind") {
      df.input <- "speciesStems" # Individuals data (stems of individual trees)
    } else {
      df.input <- "speciesCover" # Prefer species cover data if available
    }
    source.file <- paste0(df.input, ".csv") # Input file required for this indicator
  } else if (ei.code == "TD") {
    ei.name <- "Taxonomic distance"
    ei.name.with.units <- ei.name
    distn <- "gamma"
    q.method <- "empirical"
    has.stratum <- FALSE
    test.tail <- "both"
    bm.val <- NA
    proportions <- TRUE # Must be FALSE if individuals data (stems)
    convert.percent <- FALSE # Must be FALSE if individuals data (stems)
    remove.zero.cover.plots <- TRUE # all-zero cover impossible for this EI; ignored if individuals data

    #  Set TRUE to scale abundance values between 0 and 1
    # Use if species cover sums to >100% (or >1 proportional abundance)
    scale.abund <- FALSE

    # Transform all TD values by multiplying by td.multiplier?
    # td.multiplier <-1 skips transformation
    # Strongly recommend td.multiplier <- 20
    # Low values, especially close to one, result in poor fit with high overlap
    td.multiplier <- 20

    # Vector of land cover classes to omit from scaling
    # If no omissions, set to empty string ""
    td.multiplier.omit <- ""
    td.multiplier.omit <- c("")

    # Input data frame and file for this indicator
    ei.data.type <- DATA.TYPE
    if (ei.data.type == "ind") {
      df.input <- "speciesStems" # Individuals data (stems of individual trees)
    } else {
      df.input <- "speciesCover" # Prefer species cover data if available
    }
    source.file <- paste0(df.input, ".csv") # Input file required for this indicator
  } else {
    stop("ERROR: unknown EI (function ei.params)")
  }

  # Be careful with these next two assignments
  # that every parameter is included.
  # Any parameter omitted will be silently omitted!
  param.list <- list(
    ei.name, distn, q.method, ei.data.type,
    has.stratum, test.tail, bm.val, proportions, df.input,
    source.file, prepare.raw, convert.percent, logit,
    remove.zero.cover.plots, scale.abund,
    td.multiplier, td.multiplier.omit, exclude.exotics, ei.multiplier,
    normalize.m2, NA.TO.ZERO
  )
  names(param.list) <- c(
    "ei.name", "distn", "q.method", "ei.data.type",
    "has.stratum", "test.tail", "bm.val", "proportions", "df.input",
    "source.file", "prepare.raw", "convert.percent", "logit",
    "remove.zero.cover.plots", "scale.abund",
    "td.multiplier", "td.multiplier.omit", "exclude.exotics", "ei.multiplier",
    "normalize.m2", "NA.TO.ZERO"
  )

  return(param.list)
}

##############################
# Custom NMDS parameters
#
# Applies to indicator script "td.R" only
# Default values used for land cover
# classes not listed
##############################

nmds.params <- function(land.cover) {
  ##############################
  # Sets key NMDS parameters
  # If vegetation not listed uses default
  # values. In most cases the default
  # values should work. Use this function
  # if different parameters are required
  # to achieve convergence for a particular
  # vegetation type
  # *** Application-specific ***
  # CHECK CAREFULLY!
  ##############################

  # General NMDS options

  # NMDS verbose mode
  # Generally set to FALSE, unless want
  # verbose output from each iteration
  nmds.verbose <- FALSE

  # Use randomization seed?
  # Should always be=TRUE unless testing
  # Possibly no longer used?
  nmds.set.seed <- TRUE

  # Default metaMDS options
  nmds.seed <- 10
  nmds.trymax <- 1000
  nmds.k <- 3
  nmds.maxit <- 200

  # Adjust vegetation-specific metaMDS options here
  # Add more vegetation classes as needed
  if (land.cover == "LC.EXAMPLE") {
    nmds.seed <- 19590731
    nmds.trymax <- 5000
    nmds.maxit <- 400
  } else if (land.cover == "LC.EXAMPLE2") {
    nmds.seed <- 19590731
    nmds.trymax <- 5000
    nmds.maxit <- 400
  }

  # Compile final list of option values
  nmds.param.list <- list(
    land.cover, nmds.verbose, nmds.set.seed,
    nmds.seed, nmds.trymax, nmds.k, nmds.maxit
  )
  names(nmds.param.list) <- c(
    "land.cover", "nmds.verbose", "nmds.set.seed",
    "nmds.seed", "nmds.trymax", "nmds.k", "nmds.maxit"
  )

  return(nmds.param.list)
}

####################################################
####################################################
# Custom confirmation parameters & messages
#
# For appending to basic parameters messages from
# generic parameters file
####################################################
####################################################

####################################################
# General confirmation message options
#
# Will replace defaults if
# different
####################################################

####################################################
# Import confirmation message options
####################################################

if (pcess.na.delete == TRUE) {
  pcess.na.delete.disp <- "delete"
} else {
  pcess.na.delete.disp <- "Set zero"
}
if (pcgf.na.delete == TRUE) {
  pcgf.na.delete.disp <- "delete"
} else {
  pcgf.na.delete.disp <- "Set zero"
}
if (gc.na.delete == TRUE) {
  gc.na.delete.disp <- "delete"
} else {
  gc.na.delete.disp <- "Set zero"
}
if (spp.cov.na.delete == TRUE) {
  spp.cov.na.delete.disp <- "delete"
} else {
  spp.cov.na.delete.disp <- "Set zero"
}
if (spp.cov.zero.delete == TRUE) {
  spp.cov.zero.delete.disp <- "delete"
} else {
  spp.cov.zero.delete.disp <- "Set zero"
}
# if (bad.plots.delete ==TRUE) {
#   bad.plots.delete.disp <- "delete"
# } else {
#   bad.plots.delete.disp <- "keep"
# }

####################################################
# Reload general confirmation messages to pick up
# any changes
####################################################

source(paste0(SRCDIR, "params.conf.general.R"))

####################################################
# Add project-specific messages
####################################################

# Project-specific import parameters (appended to existing)
MSG.CONF.IMP.PS <- ""
MSG.CONF.IMP.PS <- paste0(MSG.CONF.IMP.PS, "  Append seral stage to benchmark vegetation?: ", bm.forested.include.seral, "\n")
MSG.CONF.IMP.PS <- paste0(MSG.CONF.IMP.PS, "  Species cover NA action: ", spp.cov.na.delete.disp, "\n")
MSG.CONF.IMP.PS <- paste0(MSG.CONF.IMP.PS, "  Species cover zero action: ", spp.cov.zero.delete.disp, "\n")
MSG.CONF.IMP.PS <- paste0(MSG.CONF.IMP.PS, "  Percent Cover Exotic Species by Stratum (PCESS) all NA/zero action: ", pcess.na.delete.disp, "\n")
MSG.CONF.IMP.PS <- paste0(MSG.CONF.IMP.PS, "  Percent Cover by Growth Form (PCGF) all NA/zero action: ", pcgf.na.delete.disp, "\n")
MSG.CONF.IMP.PS <- paste0(MSG.CONF.IMP.PS, "  Ground cover (GC) NA/zero action: ", gc.na.delete.disp, "\n")

# Project-specific vqa.batch messages
MSG.CONF.BATCH.PS <- ""
MSG.CONF.BATCH.PS <- paste0(MSG.CONF.BATCH.PS, "  pcess.herbs.only='", pcess.herbs.only, "'\n")

##################################################
##################################################
# Project parameters
#
# Some of these are project-specific values of
# general parameters set in the general parameters
# file (params.R). Others are project-specific
# parameters (i.e., unique to this project).
# Project-specific parameters are listed first,
# followed by general parameters. Check carefully!
##################################################
##################################################

#################################################
#################################################
#################################################
# Project-specific parameters
#
# These parameters are specific to this project
# (and assessment, if applicable). They are not
# found in the general parameters file (params.R)
#################################################
#################################################
#################################################

####################################
# Custom model options
####################################

# Height class strata to omit from input data
# frame coverByGrowthForm during import.
# This option was made necessary by discovery that
# Braun Blanquet (BB) cover classes distort
# PCGF quality scores due to few, fixed cover values
# at high values of cover. This is a critical problem
# under two conditions: (1) 1 to few species in the
# stratum (e.g., "D: Trees"), and (2) one or more with
# high values of cover (>30% cover).
# The BB cover scale allows only 3 possible values of
# percent cover above 30% (42.5%, 62.5% and 87.5),
# resulting in leptokurtic or unimodal distributions,
# artificially low distribution overlaps, and
# unrealistically low to zero quality scores.
# This parameter is a vector. List codes of all
# strata to omit, double quoted and comma separated.
# To deactivate this parameters and keep all available
# strata, set to empty vectir: PCGF.STRATA.OMIT<-c()
# Be sure to use *final* codes used, as present just
# before final mention of this df in the project-
# specific import script.
PCGF.STRATA.OMIT <- c("C", "D", "CD", "Trees")

####################################
# Custom import options
####################################

# Names of land cover worksheets in RAW_PLOTDATA_FILENAME
SHEET.VEG <- "Vegetation" # bm vegetation
SHEET.STRATA <- "Strata" # Sampling strata
SHEET.SITES <- "Sites" # Physical sites along the pipeline

# Names of plot data worksheets in RAW_PLOTDATA_FILENAME
SHEET.PLOTS <- "Plots"
SHEET.SPECIES <- "Species"
SHEET.COVER <- "Cover data"
SHEET.STRUCTURE <- "Structure data"

# Name of worksheet in RAW_SPP_FILENAME containing exotic species data
RAW_SPP_SHEET <- "Checklist"

# Source assessment of benchmark data for offset assessment
# Benchmark data will be imported from raw data directory
# in assessment directory bm.data.source.assess.
# Offset benchmark data for this project were not included
# in the assessment raw data.
# Values: <ASSESS>|<ASSESS.BASELINE>|<...>
# * Value option <...> is any custom location
#   other than "<ASSESS>/raw/" or
#   "<ASSESS.BASELINE>/raw/"
bm.data.source.assess <- "project_baseline"

####################################
# Structure plots dimensions and area
# Areas are total m2 across all subplots
# in size class
####################################

sample.area.m2_stems2.5to10 <- 48 # stems 2.5-<10 cm
sample.area.m2_stems.ge10 <- 192 # stems >=10 cm

######################################################
# Functions for area correction by stem size
#
# Require globally-accessible project-specific
# parameters sample.area.m2_stems2.5to10 and
# sample.area.m2_stems.ge10 (see above)
######################################################

# Required for case_when statements
# Tidyverse is included in the general VQA libraries,
# but these are loaded after this file
suppressMessages(library("tidyverse"))

dbh.ba.scaled <- function(dbh, targetArea_m2 = NULL) {
  #################################################
  # Converts stem dbh to basal area and scales it
  # to the BA expected in a plot of size targetArea_m2,
  # as determined by the actual sampling area used for
  # stems of that size.
  #
  # Accepts: vector of numeric stem DBHs, plus
  #   optional target area in m2
  # Returns: vector of basal areas, scaled up to m2 / 1 ha
  # Requires: globally-accessible size class-specific
  #   area parameters, as set by project-specific
  #   parameter file
  #
  # Notes:
  # * All areas MUST be in m2
  # * If targetArea_m2 not supplied, assumes 1 ha (10,000 m2)
  # * sampleArea1_m2, sampleArea2_m2, etc., MUST be in m2
  # * Use only as intermediate step in calculating
  #   total BA (m2) per targetArea_m2. Individual
  #   ba.scaled values are meaningless by themselves.
  # * Multiplies the measured BA by the area
  #   in m2 of the target area divided by the actual
  #   area in which that size class was measured.
  # * This method assumes constant stem density
  #   per area. E.g., one stem per 0.1 ha (the
  #   sampleArea) becomes 10 stems per 1 ha
  #   (the targetArea). Seems odd, but the sum of all
  #   ba.scaled for all stems within a size class
  #   is the same as the sum of the actual BAs of
  #   all stems within that size class,
  #   multiplied by the targetArea:sampleArea ratio.
  # * Number of size classes (strata) and the
  #   stem diameters that define them are hard-
  #   wired into this function. Need to find
  #   more general way to do this.
  #################################################

  sampleArea1_m2 <- sample.area.m2_stems2.5to10
  sampleArea2_m2 <- sample.area.m2_stems.ge10
  if (is.null(targetArea_m2)) targetArea_m2 <- 10000 # Assume 1 ha

  ba.scaled <- case_when(
    dbh >= 2.5 & dbh < 10 ~ pi * ((dbh / 2)^2) / 10000 * targetArea_m2 / sampleArea1_m2,
    dbh >= 10 ~ pi * ((dbh / 2)^2) / 10000 * targetArea_m2 / sampleArea2_m2,
    is.na(dbh) ~ NA,
    .default = NA
  )

  return(ba.scaled)
}

ind.scaled <- function(dbh, targetArea_m2 = NULL) {
  #################################################
  # Calculates the expected number of individuals
  # in a plot of size targetArea_m2, as determined
  # by the actual sampling area used for
  # stems of that size.
  #
  # Accepts: vector of numeric stem DBHs, plus
  #   optional target area in m2
  # Returns: vector of ind (expected number of
  #   individuals) in a 1 ha plot
  # Requires: globally-accessible size class-specific
  #   area parameters, as set by project-specific
  #   parameter file
  #
  # Notes:
  # * sampleArea1, sampleArea2, etc., MUST be in m2
  # * Use only for single individual, not counts of
  #   multiple individuals
  # * CRITICAL: if raw data contain multiple stems
  #   per individual, MUST use largest stem
  #   for that individual and discard all other
  #   rows.
  # * As ind.scaled is a float (not an integer),
  #   you MUST either (a) truncate or round, or (b)
  #   use a distribution suitable for continuous
  #   values. I.e., use distn="gamma" instead of
  #   distn="NBin"
  # * Use only as intermediate step in calculating
  #   total individuals per ha.
  # * Multiplies the observed number of individuals
  #   by the target area divided by the actual
  #   area in which that size class was measured.
  # * This method assumes constant stem density
  #   per area. E.g., one stem per 0.1 ha (the
  #   sampleArea) becomes 10 stems per 1 ha
  #   (the targetArea). Seems odd, but the sum of all
  #   ba.scaled for all stems within a size class
  #   is the same as the sum of the actual BAs of
  #   all stems within that size class,
  #   multiplied by the targetArea:sampleArea ratio.
  # * Number of size classes (strata) and the
  #   stem diameters that define them are hard-
  #   wired into this function. Need to find
  #   more general way to do this.
  #################################################

  ind <- 1 # Supplying this as parameter to make it obvious
  sampleArea1_m2 <- sample.area.m2_stems2.5to10
  sampleArea2_m2 <- sample.area.m2_stems.ge10
  if (is.null(targetArea_m2)) targetArea_m2 <- 10000 # Assume 1 ha

  ind.scaled <- case_when(
    dbh >= 2.5 & dbh < 10 ~ ind * targetArea_m2 / sampleArea1_m2,
    dbh >= 10 ~ ind * targetArea_m2 / sampleArea2_m2,
    is.na(dbh) ~ NA,
    .default = NA
  )

  return(ind.scaled)
}

######################################
# Names and alt names of strata, etc.
######################################

# Structure strata
# Stratum names for DBH size classes
# Parameters make it easier to change names on the floy
stem.class.2.5to10 <- "Stems 2.5-10 cm DBH"
stem.class.ge.10 <- "Stems >=10 cm DBH"

# Convert stratum codes to name using
# function cover.stratum.name()?
cover.stratum.names.convert <- TRUE

# Vegetation cover strata
cover.stratum.name <- function(codes) {
  #########################################
  # Assigns more meaningful name to cover
  # stratum codes.
  # Works on single values or vectors.
  #########################################

  names <- case_when(
    codes == "A" ~ "Herbs & Subshrubs",
    codes == "B" ~ "Shrubs",
    codes == "C" ~ "Trees", # Note lumping of strata C & D
    codes == "D" ~ "Trees", # Note lumping of strata C & D
    codes == "CD" ~ "Trees", # New code for lumped strata C & D
    is.na(codes) ~ NA,
    .default = "[UNKNOWN]"
  )

  return(names)
}

# Vegetation structural type (lc.type)
vegCode.lc.type <- function(vc) {
  ##############################################
  # Assigns structural type (lc.type) for each
  # EUNIS vegetation code
  ##############################################

  lc.types <- case_when(
    vc %in% c("U16", "U17") ~ "Tall-herb meadow",
    vc %in% c("W2") ~ "Flooded woodland",
    vc %in% c("W12", "W18", "W11", "W17", "W10", "W16") ~ "Forest",
    vc %in% c("U4") ~ "Grassland",
    vc %in% c("M24") ~ "Mesic grassland",
    vc %in% c("W21") ~ "Shrubland",
    is.na(vc) ~ NA,
    .default = "[UNKNOWN]"
  )

  return(lc.types)
}

####################################
# Error file parameters
####################################

err.file.path <- RESULTSDIR

#################################################
#################################################
#################################################
# General parameters
#
# These settings over-ride parameters from
# the general parameters file (params.R)
#################################################
#################################################
#################################################

#####################################
# Raw data file names
# Input to "import.R" (first step in pipeline)
# All should be in RAWDATADIR
# If files are further buried in subdirectories, be
# sure to prepend the path to the file name,
# relative  to RAWDATADIR
#####################################

# File name of raw land cover master list
# If no file, set to ""
RAW_LANDCOVER_FILENAME <- "" # All landcover data are in raw plot spreadsheet

# Name of file or database of raw plot and landcover data
if (grepl("offset", ASSESS, ignore.case = TRUE)) {
  RAW_PLOTDATA_FILENAME <- "Demo_data_offsets.xlsx" # Offset assessment data
} else {
  (
    RAW_PLOTDATA_FILENAME <- "Demo_data_main.xlsx" # Project site assessment data (baseline and current)
  )
}

# Species attributes file name (this one is exotic species only)
# If separate file or database of species attributes will
# be imported, list it here. If no file, set to ""
RAW_SPP_FILENAME <- "Exotic_species.xlsx"

##########################################################
##########################################################
# Import options
##########################################################
##########################################################

# DATA.TYPE (cover|ind|mixed)
# Type of species observation data.
# Affects both import (import.R) and analysis (vqa.batch.R)
# Note: for mixed data, the cover data component is
# generally the most inclusive of all species, and
# should be the data used for composition indicators
# such as SR and TD.
# Values:
#   cover: percent cover by species. Must have "cover" column
#   ind: individuals. Must have "ind_id" columm
#   mixed: both individuals and cover data present
# IMPORTANT: Declare *before* parameter function ei.params()
DATA.TYPE <- "mixed"

# Offset dataset appears to run OK at very low sample size
# of n=4.
N.MIN.ABS <- 4

# Make new include files? (BLACKLIST.FILE, WHITELIST.FILE)
# TRUE: Replace existing files on import, including all
#   indicators and strata present in the data. In other
#   indicators and strata are extracted automatically
#   from the data.
# FALSE: reuse existing include files. Use this
#   option to preserve include files with manual
#   changes to remove/include specific indicators
#   or vegetation-indicator combinations.
# For first-time imports, new include files are
# generated automatically without checking this
# parameter. For subsequent imports, existing include
# files are preserved (i.e., not replaced) unless this
# parameter is set to TRUE.
# Generally keep set to FALSE. Only set to TRUE when
# repeating import and wish to replace previously
# created include files.
REPLACE.INCLUDE.FILES <- FALSE

##############################################
##############################################
# VQA Pipeline (vqa.batch) options
#
# Used by script vqa.batch.
# These options control the behavior of the main
# VQA pipeline which imports the standardized
# VQA input files, calculates and saves the
# indicator values from the raw data, calculates
# indicator quality, functional group quality,
# overall quality and quality hectares, and
# finally, generates the figures show the
# empirical sampling distributions and fitted
# probability distributions for all indicators.
##############################################
##############################################

# Calculate all indicators from scratch?
# If TRUE, overrides any indicator-specific setting of
# parameter prepare.raw.
# TRUE: Prepare raw data for all indicators.
# FALSE: Do not force preparation of raw data for all indicators.
# Values of prepare.raw in ei.params (see below) will
# determine action taken.
# Generally, set to FALSE after first run, as indicator
# values don't change after initial calculation, and the
# latter operation can be very time consuming.
force.prepare.raw <- TRUE

# Plot figures only?
# TRUE=just plot figures, skip all calculations
# FALSE=do everything (calculations and figures)
PLOT.FIGS.ONLY <- FALSE

# Run indicator TD only?
# TRUE: TD only
# FALSE: run all indicators in EI.vec
# Set to TRUE when doing multiple TD runs to
# adjust NMDS parameters (see below)
TD.ONLY <- FALSE

# Run final vqa summary only
# Skips indicator quality calculations and figures.
# All processed data and quality scores must be
# present to use this option
VQA.SUMMARY.ONLY <- FALSE

##############################################
##############################################
# Model options
#
# These options determine the methods and
# algorithms used to calculate quality.
# Review carefully!
##############################################
##############################################

# What to group by when calculating function group quality?
# Values: indicator|indicator_group
#  indicator: group by individual Q.i values in land cover + F.group class
#  indicator_group: Aggregate Q.i (indicators quality) to Q.ig (indicator group quality)
#    within each indicator group (EI) + land cover + F.group class, then aggregate the Q.ig
#    by land cover + F.group. This remove the excessive weight of indicators group with
#    many component indicator (=strata)
Q.FG.GROUP.BY <- "indicator_group"

# Name of mean function used to aggregate indicator qualities (Q.i).
# Values: arithmetic|generalized_mean
#   arithmetic: arithmetic mean function mean()
#   generalized_mean: generalized_mean()
Q.FG.METHOD <- "generalized_mean"

# Type of mean used to aggregate functional group qualities (Q.fg)
# when calculating overall quality
# Values: gmean|prod
#   gmean: Geometric mean of all Q.fg
#   prod: Product of all Q.fg
#   mixed: (geomean of all non-integrity indicators) * (geomean of all integrity indicators)
Q.OVERALL.METHOD <- "mixed"

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
# Ignored if INCLUDE.PLOTS.NODATA == FALSE
NODATA.SD <- 4 # Number of standard deviations of no-data value

# Bootstrap replicates
# If running qh.net.R, value of boot.reps used for all
# assessments MUST match the value set here
boot.reps <- 10000 # For production run with accurate CLs; slow
boot.reps <- 100 # For quick trial run with approximate CLs
boot.reps <- 10 # For testing with small readable bootstrap files

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
# QH.METHOD="assume.0" is typically used for
#   footprint current assessments where no empirical
#   data are not available and impacts are sufficiently
#   severe that empirical measurement of the few
#   remaining QH is not cost-effective.
#
# This parameter affects data import (import.R),
# the main VQA pipleline (vqa.batch.R) and
# Net Quality Hectares (qh.net.R)
##############################################
##############################################

if (ASSESS == "offset_baseline") {
  QH.METHOD <- "assume.0"
} else {
  QH.METHOD <- "empirical"
}

##############################################
##############################################
# QH.net options
# * Affect script "qh.net.R" only
# * Note that boot.reps for all assessments
#   MUST match the value in this file (see above)
##############################################
##############################################

# Include offset assessment?
# IMPORTANT: This gets reset to FALSE if scope=="offset"
# See section "Set scope"
INCLUDE.OFFSET <- TRUE

# Baseline assessment code
if (grepl("offset", ASSESS, ignore.case = TRUE)) {
  ASSESS.BASELINE <- "offset_baseline"
} else {
  ASSESS.BASELINE <- "project_baseline"
}

# Offset current assessment code
ASSESS.OFFSET <- "offset_current"

# Keep these set to FALSE
# See params.R for details
PREPARE.QH.NET.P <- FALSE
ALLOW.TRADE.UP <- FALSE

# Note that the following directories MUST be re-set here!

# QH.net results directory
# Make it a subdirectory of current assessment data directory
# Comparison is always to the baseline assessment
QH.NET.DIR <- paste0(DATA_BASEDIR, "qh.net/")
QH.NET.RESULTSDIR <- paste0(QH.NET.DIR, "results/")

# QH.net input file directories
QH.NET.INPUTDIR.CURRENT <- paste0(DATA_BASEDIR, "results/") # QH files
QH.NET.INPUTDIR.BASELINE <- paste0(DATA_BASEDIR_PROJ, ASSESS.BASELINE, "/results/") # QH files
QH.NET.INPUTDIR.OFFSET <- paste0(DATA_BASEDIR_PROJ, ASSESS.OFFSET, "/qh.net/results/") # Net QH files

####################################
# Species & taxonomy options
####################################

# Action taken if native status value (is_exotic) is
# missing for one or more species (TRUE|FALSE)
# TRUE: Mark missing species is_exotic<-0
# FALSE: Echo error message and quit
IS.EXOTIC.MISSING.ASSUME.NATIVE <- TRUE

##########################################################
##########################################################
# Figure options
##########################################################
##########################################################

# Type of figure file
# Options: "pdf", "png" (just these for now; pdf for hi res only)
# If using pdf, may need to adjust additional parameters currently in graph.dists.R
FIG.TYPE <- "pdf" # Better for report and publications
FIG.TYPE <- "png" # Better for easy viewing during testing & trial runs

# Set to false to omit legends entirely from indicator histograms
plot.legends <- TRUE

# Set to FALSE to omit focal and benchmark color key from legend
LEGEND.NO.FB <- FALSE

# Include sample sizes in focal and benchmark color key?
show.n <- FALSE

# Combine all legends on right side of histogram
legend.combined <- FALSE

# Which graphs to plot? TRUE/FALSE
# Only include variant options which differ from the default settings
# For default settings for full list of parameters, see main params file
plot.dists_fitted_grouped_rescaled <- FALSE # Enable if any indicators contain strata
plot.dists_fitted_rescaled <- TRUE # Always enable this one

#########################
# Land cover classes
# Check carefully: highly project-specific!
#########################

# Strictly anthropogenic land cover classes
# Will be lumped into "Anthropogenic" and ignored
eg.anthro <- c(
  ""
)

# Natural but non-vegetated land cover classes
# Will be ignored in quality and QH calculations
eg.non.veg.strict <- c(
  ""
)

# Anthropogenic + natural non-vegetated EGs
eg.non.veg <- c(eg.anthro, eg.non.veg.strict)

# Forested vegetation classes
eg.forested <- c(
  ""
)

# Natural vegetation with insufficient data
# Used to distinguish from anthropogenic land cover classes.
# For final summary tables.
veg.nodata <- c(
  ""
)

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
  c("TS", "composition"),
  c("PCES", "integrity"),
  c("PCGF", "structure"),
  c("BA", "structure"),
  c("ASC", "structure")
)))
if (TD.ONLY == TRUE) {
  # Temporarily reset for this run if TD.ONLY
  EI.F.GROUPS <- t(as.data.frame(list(
    c("TD", "composition")
  )))
}
rownames(EI.F.GROUPS) <- NULL
colnames(EI.F.GROUPS) <- c("EI", "f.group")
EI.list <- data.frame(fname = EI.F.GROUPS[, 1])
EI.vec <- EI.F.GROUPS[, 1]
df.EI.FG <- as.data.frame(EI.F.GROUPS)

##################################
# Aggregate indicators
# Ecological Indicators (abbreviations) for which stratum
# scores will be combined in results files
# Set EI.agg.list to NA if not applicable for this analysis
##################################

EI.agg.list <- data.frame(fname = c(
  "PCGF",
  "ASC"
))

# Indicator-specific parameters
# Make sure all indicators in current analysis are included
# REVIEW CAREFULLY!
ei.params <- function(ei.code) {
  ##############################
  # Returns parameters associated with
  # this EI.
  # Parameter bm.val only required
  # if q.method=='fixed', otherwise NA
  # "source.file" is the name of the
  # input file used to calculate indicator
  # values
  ##############################

  #########################
  # Default values
  # Over-ride for individual
  # indicators as needed
  #########################

  convert.percent <- FALSE
  remove.zero.cover.plots <- FALSE
  prepare.raw <- FALSE
  ei.data.type <- DATA.TYPE

  # Transform raw abundance to proportional
  # abundance, relative to maximum abundance?
  # TRUE|FALSE
  # *** MUST be TRUE if values are abs. abundance ***
  # Generally set to FALSE for percent cover,
  # unless some cover values>100%
  scale.abund <- TRUE

  # Logit transformation, for proportions only
  # Generally a bad idea, esp. for NMDS, & not
  # needed for overlap
  # MUST be FALSE for absolute abundance
  logit <- FALSE

  # Potentially applies to indicators TD, PCGF and GC
  proportions <- FALSE

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
  # Can be used to scale area-dependent indicators
  # (such as number of individuals) to different area
  # to improve readability of graphs. For example, set
  # ei.multiplier <- 10 to scale BA per 0.1 ha to
  # BA per ha. Also used to improve fit to gamma
  # distributed indicators with many values <1
  # so that all values are >>1  (e.g., TD).
  ei.multiplier <- 1

  # Scale indicators to values expected
  # in subsamples of constant area (=normalize.m2)?
  # Corrects for use of different sample
  # areas for different stem sizes (DBH classes).
  # Applies only to indicators for which DATA.TYPE="ind".
  # normalize.m2 <- 10000: normalize to 1 ha
  # normalize.m2 <- FALSE (default): turn off normalization
  # Recommend turn off (set to FALSE) if all size classes
  #   already sampled in same area; use parameter
  #   ei.multiplier to scale the indicator values to a
  #   different area, if desired.
  normalize.m2 <- FALSE

  # Set NA values to 0 for this indicator
  # Values: TRUE|FALSE
  #  TRUE: Issue warning, set NA values to 0, and continue
  #  FALSE: Report error and stop
  NA.TO.ZERO <- TRUE

  if (ei.code == "PCES") {
    # Percent cover exotic species (for entire plot, not by stratum)
    ei.name <- "Percent Cover Exotic Species"
    ei.name.with.units <- ei.name
    distn <- "Bet"
    q.method <- "fixed" # c('empirical','fixed')
    has.stratum <- FALSE
    test.tail <- "upper"
    bm.val <- 0 # should be integer if q.method='fixed', otherwise numeric
    proportions <- TRUE # FALSE if raw data are percent
    remove.zero.cover.plots <- FALSE # Zero cover common for this EI
    ei.data.type <- "cover" # Always cover, by definition
    df.input <- "exoticCoverByStratum"
    source.file <- paste0(df.input, ".csv") # Input file full name
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
    test.tail <- "both"
    bm.val <- NA

    # Input data frame and file for this indicator
    ei.data.type <- "cover" # For this project use cover, not structure data
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
    proportions <- FALSE # Must be FALSE if individuals data (stems)
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
    td.multiplier.omit <- c("")

    # Input data frame and file for this indicator
    ei.data.type <- "cover" # For this project use cover, not structure data
    if (ei.data.type == "ind") {
      df.input <- "speciesStems" # Individuals data (stems of individual trees)
    } else {
      df.input <- "speciesCover" # Prefer species cover data if available
    }
    source.file <- paste0(df.input, ".csv") # Input file required for this indicator
  } else if (ei.code == "TS") {
    # Pairwise taxonomic similarity based on Sorensen Index
    # Compare distributions of focal<-->benchmark and
    # benchmark<-->benchmark between-plot similarity
    ei.name <- "Taxonomic Similarity"
    ei.name.with.units <- ei.name
    distn <- "Bet"
    q.method <- "empirical"
    has.stratum <- FALSE
    # Lower test tail only: focal plots more similar on average
    # to benchmark plots than the benchmark plots themselves
    # receive 100% quality score
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
  } else if (ei.code == "ASC") {
    ei.name <- "Abundance by Size Class"
    ei.name.with.units <- ei.name
    distn <- "gamma" # Beter than NBin, don't have to set zeroes to 1
    q.method <- "empirical"
    has.stratum <- TRUE
    test.tail <- "both"
    bm.val <- NA
    exclude.exotics <- FALSE
    ei.multiplier <- 1
    NA.TO.ZERO <- FALSE # Do not set NA abundance to zero; report error  & abort instead

    # Input data frame and file for this indicator
    ei.data.type <- "ind" # By definition for this indicator
    df.input <- "speciesStems"
    source.file <- paste0(df.input, ".csv")
  } else if (ei.code == "BA") {
    ei.name <- "Basal Area"
    ei.name.with.units <- ei.name
    distn <- "gamma"
    q.method <- "empirical"
    has.stratum <- FALSE
    test.tail <- "both"
    bm.val <- NA
    exclude.exotics <- FALSE

    # Normalize indicators to values expected
    # in a common area of normalize.m2?
    # normalize.m2<-10000 scales to 1 ha
    # normalize.m2<-FALSE turns off normalization
    normalize.m2 <- 10000

    # Additional multiplier applied to final ei value?
    # Delete or set to 1 to keep original value (default)
    ei.multiplier <- 1

    # Revise ei.name.with.units to reflect area changes
    # due to use of normalize.m2 and ei.multiplier
    ei.name <- "Basal Area (m2/ha)"
    NA.TO.ZERO <- TRUE # Set NA to 0 for this indicator?

    # Input data frame and file for this indicator
    ei.data.type <- "ind" # By definition for this indicator
    df.input <- "speciesStems"
    source.file <- paste0(df.input, ".csv")
  } else if (ei.code == "BAES") {
    ei.name <- "Basal Area Exotic Species"
    ei.name.with.units <- ei.name
    distn <- "gamma"
    q.method <- "empirical"
    has.stratum <- FALSE
    test.tail <- "upper"
    bm.val <- NA
    exclude.exotics <- FALSE
    ei.multiplier <- 100
    # Revised ei.name.with.units to reflect ei.multiplier
    ei.name.with.units <- "Basal Area Exotic Species (m2/ha)"

    # Input data frame and file for this indicator
    ei.data.type <- "ind" # By definition for this indicator
    df.input <- "speciesStems"
    source.file <- paste0(df.input, ".csv")
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

  # Set vegetation-specific metaMDS options here
  # Adjust and expand as needed
  if (land.cover %in% c("AAA", "BBB")) {
    nmds.seed <- 19590731
    nmds.k <- 3
    nmds.trymax <- 5000
    nmds.maxit <- 400
  } else if (land.cover %in% c("XXX", "YYY")) {
    nmds.seed <- 19590731
    nmds.k <- 2
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

########################################
# Set scope
# Values: baseline|current|offset
# IMPORTANT: update assessment vectors to include
# the names of any new assessments, otherwise code
# will abort.
########################################

# cat( "Setting scope (data and treatment to include) based on assessment code..." )

# Assessment codes MUST contain one of the four suffixes shown below
if (ASSESS == "project_baseline") {
  scope <- "baseline" # Control (baseline) assessment plots + benchmark plots only
} else if (ASSESS == "project_current") {
  scope <- "current" # Main (current assessment) plots + benchmark plots only
} else if (grepl("offset_current", ASSESS, ignore.case = TRUE)) {
  scope <- "offset" # Offset current assessment focal + benchmark plots
} else if (ASSESS == "offset_baseline") {
  # Baseline assessment, assumes all vegetation quality=0
  # This scope will load the same plots as for the current assessment, but only input
  # file "landCover.csv" will be generated by the import, and only the QH and bootstrap
  # QH results files will be generated by vqa.batch.
  scope <- "offset"
} else {
  msg.err <- paste0("Assessment code ASSESS='", ASSESS, ", not not matched! Parameter 'scope' not set.")
  stop(msg.err)
}
# cat("scope='", scope, "'...done\n", sep="")

# Reset QH.net behavior based on scope
if (scope == "offset") {
  # Ignore request to include offset in qh.net if
  # calculating qh.net for an offset
  INCLUDE.OFFSET <- FALSE
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
# Declare any additional changed parameters here
####################################################

# td.multiplier <- ei.params("TD")$td.multiplier

####################################################
# Reload general confirmation messages to apply the
# above changes, if any
####################################################

source(paste0(SRCDIR, "params.conf.general.R"))

####################################################
# Add project-specific messages
####################################################

# Project-specific import parameters (appended to existing)
MSG.CONF.IMP.PS <- ""
if (QH.METHOD == "empirical") MSG.CONF.IMP.PS <- paste0(MSG.CONF.IMP.PS, "  PCGF.STRATA.OMIT: ", paste(PCGF.STRATA.OMIT, collapse = ", "), "\n")

if (job == "qh.net") {
  MSG.CONF.QH.NET.PS <- ""
  MSG.CONF.QH.NET.PS <- paste0(MSG.CONF.QH.NET.PS, "  Scope: ", scope, "\n")
}

# Project-specific vqa.batch messages
MSG.CONF.BATCH.PS <- ""

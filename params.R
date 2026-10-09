#######################################################
# Default VQA parameters
#
# Important! Read this:
# * Always keep this file in the application base
#   directory
# * Some parameter values listed here are example
#   values only, and should be changed to values
#   appropriate for your analysis
# * If any parameters need to be changed, copy them
#   to project-specific parameters file (in params/)
#   and set them there. Parameters set in project-
#   specific parameters file are loaded later and will
#   override default values set here.
#
# Version: 2024-02-16
# Author: Brad Boyle (brad@hg-llc.com)
#######################################################

#####################################
# Functions needed before loading
# functions file
#####################################

stop_quietly <- function(exit.msg = NULL) {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))

  if (is.null(exit.msg)) {
    stop()
  } else {
    cat(exit.msg)
    stop()
  }
}

#####################################
# Base directory options
#####################################

# Don't change these
SRCDIR <- paste0(wd, "/") # Source code base directory
BASEDIR <- paste0(dirname(SRCDIR), "/") # Application base directory

# Data directory location
# relative to VQA code directory (repo)
# I.e., is it IN or OUT of repo?
# Values: out|in
# Default: "in" (for public demo data only)
# Recommended: "out" (keeps private data out of VQA repo)
LOC_DATA_DIR <- "out"

# Project-specific scripts directory location
# relative to VQA code directory (repo)
# I.e., are they IN or OUT of repo?
# Values: out|in
# Default: "in" (for public demo scripts only)
# Recommended: "out" (keeps private/custom code out of VQA repo)
# Project-specific scripts include the following:
# 1. Directory params and everything in it
# 2. Directory imports/ and everything in it
# 3. Script params.pa.R
# Project-specific scripts directory name set below
LOC_PSFILES_DIR <- "out"

# Project-specific scripts directory (PSSD) name
# Default: "src_conf/"
# Recommended: whatever you want!
# You can maintain multiple PSSDs if you wish, adjusting
# the value of this parameter as needed.
# Recommended naming convention:
# `src_conf_<PROJ>` or `src_conf_<CLIENT_CODE>`
# Used only if LOC_PSFILES_DIR=="out"
# Locate inside application base application, one level
# above the main VQA code directory (repository)
# Include trailing forward slash "/"
# SRCDIR_CONF <- "src_conf/"  # Default
SRCDIR_CONF <- "src_conf_demo/"
SRCDIR_CONF <- "src_conf_tap/"

# Apply the above options
if (LOC_DATA_DIR == "out") {
  # Data are *outside* SRCDIR
  DATA_BASEDIR_FINAL <- paste0(BASEDIR, "data/")
} else {
  # Data are *inside* SRCDIR
  DATA_BASEDIR_FINAL <- paste0(SRCDIR, "data/")
}
if (LOC_PSFILES_DIR == "out") {
  # PSSD is *outside* SRCDIR
  BASEDIR_PSFILES <- paste0(BASEDIR, SRCDIR_CONF)
} else {
  # PSSD is *inside* SRCDIR
  BASEDIR_PSFILES <- SRCDIR
}

#####################################
# Startup
#####################################

# rm(list=ls())	# Clear memory

# Set working directory (base application directory)
wd <- getwd()
setwd(wd)
wd

# # Initialize flow control parameter job
# job=""
#
# Dummy parameter & function used to detect if parameters
# already loaded. Keep both versions for backwards-
# compatibility (for now). Do not delete!
params.loaded <- function() {}
global.params.loaded <- ""
legacy.run.env <- if (exists("VQA_RUN_ENV")) VQA_RUN_ENV else topenv()

# Load separate parameter file specifying
# project and assessment to be analyzed. The environment bridge is temporary:
# Phase 2 replaces this legacy edge with an in-process runner.
legacy.data.root <- if (exists("VQA_DATA_ROOT")) VQA_DATA_ROOT else Sys.getenv("VQA_DATA_ROOT", "")
legacy.project <- if (exists("VQA_PROJECT")) VQA_PROJECT else Sys.getenv("VQA_PROJECT", "")
legacy.assessment <- if (exists("VQA_ASSESSMENT")) VQA_ASSESSMENT else Sys.getenv("VQA_ASSESSMENT", "")
legacy.selection <- c(legacy.data.root, legacy.project, legacy.assessment)

if (any(nzchar(legacy.selection))) {
  if (!all(nzchar(legacy.selection))) {
    stop_quietly("ERROR: VQA_DATA_ROOT, VQA_PROJECT, and VQA_ASSESSMENT must be supplied together.\n")
  }
  if (any(!grepl("^[[:alnum:]_-]+$", legacy.selection[-1L]))) {
    stop_quietly("ERROR: VQA_PROJECT and VQA_ASSESSMENT may contain only letters, numbers, '_' and '-'.\n")
  }
  if (!file.exists(file.path(SRCDIR, "params", paste0("params.", legacy.project, ".R")))) {
    stop_quietly("ERROR: VQA_PROJECT has no project parameter file.\n")
  }
  legacy.data.root <- normalizePath(legacy.data.root, mustWork = TRUE)
  if (!dir.exists(file.path(legacy.data.root, legacy.assessment))) {
    stop_quietly("ERROR: VQA_ASSESSMENT directory is not present in VQA_DATA_ROOT.\n")
  }
  PROJ <- legacy.project
  ASSESS <- legacy.assessment
  PARAMS.USE.ASSESS <- FALSE
  IMPORT.USE.ASSESS <- FALSE
  BASEDIR_PSFILES <- SRCDIR
} else {
  params.pa.file <- paste0(BASEDIR_PSFILES, "params.pa.R")
  sys.source(params.pa.file, envir = legacy.run.env)
}

# Throw intelligible error if PROJ or ASSESS not properly set
if (!any(nzchar(legacy.selection)) && !all(sapply(c("PROJ", "ASSESS"), exists))) {
  msg.err <- "ERROR: One or both parameters PROJ and ASSESS are undefined!\n"
  msg.err <- paste0(msg.err, "Please set both in 'params.pa.R' before proceeding.\n")
  stop_quietly(msg.err)
} else if (any(sapply(list(PROJ, ASSESS), function(x) is.null(x) || x == ""))) {
  msg.err <- "ERROR: One or both parameters PROJ and ASSESS are empty or null!\n"
  msg.err <- paste0(msg.err, "Please set both in 'params.pa.R' before proceeding.\n")
  stop_quietly(msg.err)
}

# Load libraries silently (TRUE|FALSE)
# Affects how packages are loaded at the
# end of this script. Can be over-ridden
# in project-specific parameters file, which
# is sourced before libraries are loaded
# Will suppress display of namespace
# conflicts ("masking").
LIB.LOAD.SILENT <- TRUE

######################################
######################################
# DIRECTORY OPTIONS
#
# All are set relative to BASEDIR, which
# is defined at the start of this file.
# Keep near the top of params file as
# many directories are set relative
# to SRCDIR, BASEDIR, etc.
######################################
######################################

# # Project and assessment directory relative paths
# DATADIR_REL_PROJ <- paste0( "data/", PROJ, "/" )
# DATADIR_REL_ASSESS <- paste0( DATADIR_REL_PROJ, ASSESS, "/" )

# Project base data directory
# This is used mostly by qh.net.R to build qh.net input directory paths
if (nzchar(legacy.data.root)) {
  DATA_BASEDIR_PROJ <- paste0(legacy.data.root, "/")
} else {
  DATA_BASEDIR_PROJ <- paste0(DATA_BASEDIR_FINAL, PROJ, "/")
}

# Main data base directory (= assessment data directory)
# In general, everthing live here
DATA_BASEDIR <- paste0(DATA_BASEDIR_PROJ, ASSESS, "/")
data_base_dir <- DATA_BASEDIR # for backward-compatibility (remove if not needed)

# Raw data directory
# Default: RAWDATADIR <- paste0( DATA_BASEDIR, 'raw/')
# You can change this to use data stored at different location
RAWDATADIR <- paste0(DATA_BASEDIR, "raw/")

# these directories are relative to working directory (wd) of parent file
INPUTDIR <- paste0(DATA_BASEDIR, "inputs/") # Standaridized input files
RESULTSDIR <- paste0(DATA_BASEDIR, "results/") # results directory (text file not figures)
FIGDIR <- paste0(DATA_BASEDIR, "figs/") # figures directory
# FIGDIR_REL <- paste0( DATADIR_REL, "figs/")
FIGDIR_REL <- paste0("data/", PROJ, "/", ASSESS, "/figs/")
figs.dir <- FIGDIR # For compatibility with legacy variable names
figdir.abs <- paste(wd, figs.dir, sep = "/")
LOGDIR <- paste0(DATA_BASEDIR, "log/") # log file directory

# Create data directories if not exist
dir.create(file.path(INPUTDIR), showWarnings = FALSE)
dir.create(file.path(RESULTSDIR), showWarnings = FALSE)
dir.create(file.path(FIGDIR), showWarnings = FALSE)
dir.create(file.path(LOGDIR), showWarnings = FALSE)

# General include scripts directory (functions, etc.)
INCLUDESDIR <- paste0(SRCDIR, "includes/")

#########################
#########################
# Import options
#########################
#########################

#####################################
# Raw data file names
#
# * All files should be in RAWDATADIR
# * If files are further buried in subdirectories, be
#   sure to include the path to the file relative to
#   RAWDATADIR
# * If you have multiple files (e.g., sheets from an Excel
#   workbook) then create those parameters and supply their
#   values in the project-specific parameters file
#####################################

# File name of raw land cover master list
# If no file, set to ""
RAW_LANDCOVER_FILENAME <- "my_land_cover_data_file"

# Name of file or database of raw plot and landcover data
RAW_PLOTDATA_FILENAME <- "my_plot_data_file"

# Species attributes data file name
# If separate file or database of species attributes will
# be imported, list it here. If no file, set to ""
RAW_SPP_FILENAME <- "my_reference_species_list_file"

#####################################
# Other import options
#####################################

# DATA.TYPE (cover|ind|mixed)
# Type of species observation data.
# Affects both import (import.R) and analysis (vqa.batch.R)
# Values (add others as needed):
#   cover: percent cover by species. Must have "cover" column
#   ind: individuals. Must have "ind_id" columm
#   mixed: both individuals and cover data present
# Note: for mixed data, the cover data component is
# generally the most inclusive of all species, and
# should be the data used for composition indicators
# such as SR and TD.
# IMPORTANT: Declare *before* parameter function ei.params()
DATA.TYPE <- "cover"

# Absolute minimum number of focal and benchmark plots
# per land cover class required for VQA to run. Does NOT
# guarantee adequate power! If DELETE.PLOTS.BELOW.N.MIN=TRUE,
# all plot & vegetation data for this land cover class will
# be deleted from final df and saved input files.
# Recommend N.MIN.ABS=6
N.MIN.ABS <- 6

# Remove all focal plots linked to land cover with
# focal or benchmark plot sample sizes below N.MIN.ABS?
# TRUE | FALSE
# Will also delete any orphan benchmark plots and vegetation
# (i.e., not linked to any focal land cover after removal
# of focal land cover and plots with sample sizes below
# N.MIN.ABS)
# Generally always set to TRUE: indicator scripts will fail
# if too few plots.
DELETE.LC.BELOW.N.MIN.ABS <- TRUE

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
# Generally keep set to FALSE.
# Only set to TRUE when repeating import and
# wish to replace previously created include files.
REPLACE.INCLUDE.FILES <- FALSE

####################################
# Species & taxonomy options
####################################

# Action taken if native status value (is_exotic) is
# missing for one or more species (TRUE|FALSE)
# TRUE: Mark missing species is_exotic<-0
# FALSE: Echo error message and quit
IS.EXOTIC.MISSING.ASSUME.NATIVE <- TRUE

# For input df species, export only the subset of
# columns currently used for populating field "is_exotic"
# TRUE|FALSE
DF.SPECIES.MINIMAL <- TRUE

################################
# Land cover-specific parameters
# Check carefully: highly
# project-specific!
################################

# Natural vegetation with insufficient data
# Add to this vector as needed.
# If none, set to empty string ""
# Prevents errors due to attempting to analyse
# vegetation for which there is not data. Also
# used to distinguish native vegetation not analyzed
# fom anthropogenic land cover classes in final
# summary tables.
veg.nodata <- c("")

##############################################
##############################################
# VQA.BATCH OPTIONS
#
# Parameters that govern behaviour of script
# vqa.batch.R (the main VQA analysis pipeline).
#
#
# These options control the behavior of vqa.batch.R,
# the main VQA code pipeline which (a) imports
# standardized input files, (b) calculates and saves
# indicator values, (c) calculates indicator, functional
# group, and overall quality, (d) calculates quality
# hectares, and (e) generates the figures show the
# empirical sampling distributions and fitted
# probability distributions for all indicators.
##############################################
##############################################

# Calculate all indicators from scratch?
# If TRUE, overrides any indicator-specific setting of
# parameter prepare.raw.
# TRUE: Prepare raw data for all indicators.
# FALSE: Do not force preparation of raw data for all indicators.
# If force.prepare.raw==FALSE, then values of prepare.raw
# in ei.params (see below) will determine action taken.
# Generally, set to FALSE after first run, as indicator
# values don't change after initial calculation, and the
# latter operation can be very time consuming.
force.prepare.raw <- FALSE

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
QH.OMIT <- FALSE

# Log file base name
LOGFILE.BASENAME <- "log_vqa.batch"

# Replace log file each time (TRUE) or preserve each log file
# by adding timestamp to name (FALSE)?
REPLACE.LOG <- FALSE

# Run indicator TD only?
# TRUE: TD only
# FALSE: run all indicators in EI.vec
# Set to TRUE when doing multiple TD runs to
# adjust NMDS parameters (see below)
TD.ONLY <- FALSE

# Bootstrap replicates
boot.reps <- 10000 # For production run with accurate CLs; slow
boot.reps <- 10 # For rapid testing only
boot.reps <- 100 # For trial run with approximate CLs

##############################
##############################
# Model options
#
# These options determine the methods and
# algorithms used to calculate quality.
# Review carefully!
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
Q.FG.METHOD <- "generalized_mean"

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
# Normaly should always be TRUE, but may need to set
# to FALSE for early applications which have not been
# updated with speciall handling of no-data plots
INCLUDE.PLOTS.NODATA <- TRUE

# Stadard deviation penalties to apply when calculating
# indicator values of no-data plots
# Used for indicators where 0 is not the default value
# Ignored if INCLUDE.PLOTS.NODATA == FALSE
NODATA.SD <- 3 # Number of standard deviations of no-data value

# Quality algorithm for beta-distributed indicators
# "mixed" applies to beta-distributed indicators only, and only when q.method =
# "empirical". Parameter q.method is set for individual indicators only, at the
# start of the script, not in this (global parameters file (this one).
# Two options:
# overlap:	Quality based on overlap only for all indicators. For beta-distributed
# 					indicators, attempts to correct  artifacts cause by zero- and
# 					one-inflation, and narrow distributions close to 1 & 0. Still has issues
# 					so do not use without further fixes.
# mixed:		Use weighted average of overlap-based quality and means-base
# 					quality for beta-distributed indicators.
# beta.algorithm <- "overlap"
beta.algorithm <- "mixed"

# Use raw means-based quality
# Applies to beta distributed indicators only. If FALSE, adjusts q.diff to 1 (100%)
# when means not sig. diff (p.diff>=0.05). If TRUE, set q.diff to actual q.diff,
# regardless of significance. This parameter does not effect whether or not
# tranformations are applied to q.dist; this is set separately by parameter
# discount.method
use.q.diff.raw <- TRUE

# Method of discounting for means-based quality (beta distributions only)
# Affects final transformation of q.dist. Value discount.method <- "linear"
# means no transformation. discount.method <- "scaled" causes means-
# based quality to be transformed using function q.scaled.beta.
# Parameters for function q.scaled.beta are set directly in the function itself.
# Currently, values s1=0.3, s2=1 give best (=moderate) discount
# Values:
# 		linear 		No discount; Q declines linearly toward 0 at farthest boundary
# 		scaled		Q declines exponentially toward zero
discount.method <- "scaled"

# Method for weighting overlap- vs means-based quality
# Applies to beta distributed indicators where beta.algorithm <- "mixed" only,
# otherwise ignored. See function "qual.beta" for details.
# Values: "linear", "u.beta", "u.beta.buffered"
wt.method <- "u.beta.buffered"

# Edge-buffer to use for wt.method="u.beta.buffered"
# Ignored for other weighting methods
# See function "u.beta.buffered" for details.
# Recommend 0.01
buffer <- 0.01

# Set to FALSE to turn off permutation tests for q.diff when calculating bootstrap CLs
# Will increase speed, but CLs will no longer be accurate if any p>=0.05
# Applies to beta-distributed indicators only
test.diff.boot <- FALSE

# Permutation test replicates
perm.reps <- 1000

# Fix randomization seed for repeatable (fixed) bootstrap results?
set.seed <- FALSE

# Randomization seed. Only applies if set.seed=TRUE
seed <- 123

# Infer missing confidence limits due to all-zero indicators,
# division-by-zero errors, and other issues. This action
# performed in script summary.R. Setting to FALSE may
# result in some indicators being excluded from CL
# calculations.
infer.missing.cls <- FALSE

# Infer missing CLs for observed CLs close to zero
infer.missing.cls.strict <- TRUE

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
# This parameter is used by import (import.R),
# and the main VQA pipleline (vqa.batch.R) in
# producing output used by Net Quality
# Hectares (qh.net.R), but QH.METHOD is not
# directly referenced by qh.net.R.
##############################################
##############################################

# QH.METHOD <- "assume.0"
# QH.METHOD <- "assume.1"
QH.METHOD <- "empirical"

##############################################
##############################################
# QH.net options
# * Affect script "qh.net.R" only
# * By default, assumes ASSESS is the current
#   offset to be compared to baseline, as set
#   in params.pa.R
##############################################
##############################################

# Include offset assessment?
# Values: TRUE|FALSE*
#
# Requirements if TRUE:
# 1. Bm QH and bootstrapped Bm QH CSV files for current assessment,
#    in qh.net/results/ folder of current assessment
# 2. Bm QH and bootstrapped Bm QH CSV files for baseline assessment
#    in qh.net/results/ folder of baseline assessment
# 3. QH.net and bootstrapped QH.net results for offset assessment,
#    in qh.net/results/ folder of current offset assessment
#
# Notes:
# 1. Currently only 1 offset supported
# 2. All bootstrap file must contain same number of
#    bootstrap replicates.
INCLUDE.OFFSET <- FALSE

# Baseline assessment code
# MUST be same as name of baseline assessment
# subdirectory (e.g., value of ASSESS in "data/PROJ/ASSESS")
ASSESS.BASELINE <- "mine_baseline"

# Offset assessment code
# MUST be name of a current offset assessment directories.
# Only NET quality hectares for the offset is used,
# therefore the offset listed must have it's own qh.net
# directory, containing the results of a qh.net comparison
# against it's own (offset) baseline.
# Current only one offset site recorded, will expand
# to multiple offsets in later versions.
# If no offset, set to empty string ("")
ASSESS.OFFSET <- ""

# Prepare df & file qh.net.comp?
# qh.net.comp is similar to qh.net but calculates
# QH and QH.net using Qc (the complement of Q),
# where Qc = 1- Q. This provides an estimate of
# the total potential additional QH available,
# assuming quality of all classes of vegetation
# is increased to 100%
# Values: TRUE|FALSE
PREPARE.QH.NET.P <- FALSE

# Reset quality of offset vegetation with area_ha1==0
# back to zero, under assumption that absent vegetetion
# will not be created from scratch. This is essentially
# a restoration management decision.
# Ignored if PREPARE.QH.NET.P==FALSE
# Values: TRUE|FALSE
QH.NET.P.ZERO.AREA.RESET <- TRUE

# Allow transfer of QH from vegetation
# with excess net QH to vegetation
# with negative QH?
# Values: TRUE|FALSE
ALLOW.TRADE.UP <- FALSE

# Vector of vegetation types to trade up
# * QH are reassigned in order, starting with the first
#   vegetation type and reassigning its "spare" QH (i.e.,
#   until its H.net==0) then moving onto the next vegetation
#   type, until either (a) all vegetation types have positive
#   QH.net, or (b) QH.net==0 for all vegetation types listed
# * To use all vegetation with  QH.net>0, set this
#   vector to a single empty string ("")
# * Only applies if ALLOW.TRADE.UP==TRUE
VEG.TRADE.UP <- "" # Use this value to use all veg with QN.net>0

# QH.net results directory
# Set to subdirectory of current assessment data
# directory, as defined by parameter ASSESS
# Comparison is always to the baseline assessment
QH.NET.DIR <- paste0(DATA_BASEDIR_PROJ, ASSESS, "/qh.net/")
QH.NET.RESULTSDIR <- paste0(QH.NET.DIR, "results/")

# QH.net input file directories
QH.NET.INPUTDIR.CURRENT <- paste0(DATA_BASEDIR_PROJ, ASSESS, "/results/") # QH files
QH.NET.INPUTDIR.BASELINE <- paste0(DATA_BASEDIR_PROJ, ASSESS.BASELINE, "/results/") # QH files
QH.NET.INPUTDIR.OFFSET <- paste0(DATA_BASEDIR_PROJ, ASSESS.OFFSET, "/qh.net/results/") # Net QH files

# Transform one or more vegetation classes? (TRUE|FALSE)
# Generally, keep this set to FALSE
# However, if appropriate can be used to combine different
# benchmark vegetation types to make them equivalent trades
# during QH.net analysis.
# If TRUE, then you MUST define the mapping vector
# BM.VEG.TRANSFORM.VECTOR (see below)
BM.VEG.TRANSFORM <- FALSE

# Vector of benchmark vegetation types to transform.
# For example, "91M0" and mixed "91M0x62A0" can be treated
# as equivalents by converting as follows:
# BM.VEG.TRANSFORM.VECTOR <- c(
#   "91M0" <- "91M0x62A0 or 91M0",
#   "91M0x62A0" <- "91M0x62A0 or 91M0"
# )
# Can also be used to rename vegetation types on the fly.
# Use with caution, after carefully comparing input and output.
# If different bm.veg types mapping to the same transformed value
# are present in the same data frame, you will generate multiple
# rows for the same bm.veg, rather than merging.
# Ignored if BM.VEG.TRANSFORM=FALSE
BM.VEG.TRANSFORM.VECTOR <- c(
  "original.vegetation" <- "new.vegetation",
  "apples" <- "oranges"
)

####################################
####################################
# INDICATOR OPTIONS
# Review carefully!
####################################
####################################

##############
# TD options
##############

# Remove TD outlier plots?
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

# Append outliers to existing outlier file? (TRUE|FALSE)
# If FALSE, existing file will be replaced
# Generally keep set to TRUE to accumulate outliers
# across multiple runs until no more outliers found.
TD.OUTLIERS.APPEND <- TRUE

# Upper threshold beyond which value of TD
# should be regarded automatically as an outlier
# This check is needed for outliers which
# are so extreme they blow up the TD stdev,
# thereby preventing the distribution-based
# threshold (TD.OUTLIER.STDEVS) from working.
# You shouldn't have to change this.
# 500 seems to work well.
TD.OUTLIER.UPPER.THRESHOLD <- 500

# Name of TD NMDS outlier plot file
# Saved to RESULTSDIR
F.TD.OUTLIERS <- "TD_outliers.csv"

# Prevent bm plots from being replicated
# while running NMDS for indicator TD.
# Used in script td.R only.
# A temporary hack until package "vegan"
# version 7.0 is released
TD.BM.PLOTS.DEDUPLICATE <- TRUE

###############################
# BA
#
# Similar to TD.
# See TD options for details.
###############################

# Remove BA outlier plots?
# TRUE|FALSE
REMOVE.BA.OUTLIERS <- TRUE

# Number of standard deviations from mean value of BA
# Recommend start at 4 and work down
BA.OUTLIERS.STDEVS <- 4

# Append outliers to existing outlier file? (TRUE|FALSE)
# Generally keep set to TRUE
BA.OUTLIERS.APPEND <- TRUE

# Upper threshold beyond which value of BA
# should be regarded automatically as an outlier
# You shouldn't have to change this.
# 500 seems to work well.
BA.OUTLIERS.UPPER.THRESHOLD <- 500

# Name of BA outlier plot file
# Saved to RESULTSDIR
F.BA.OUTLIERS <- "BA_outliers.csv"

####################################
# Indicators and functional groups
#
# This parameters does two things:
# 1. Sets the indicators to include in
#   the current analysis
# 2. Sets the functional group to which
#   each indicator belongs
#
# CRITICAL! Indicators not listed here
# will not be included in the analysis!
#
# Indicators which consist of multiple
# subindicators that vary depending on
# the data are called "indicator groups".
# For example, indicator group PCGF could
# consist of c("Herbs", "Shrubs") in one
# dataset and c("Herbs", "Lianas", "Trees")
# in a different dataset. For these
# indicators, include only the high level
# indicator group, not the subindicators.
# For example, include 'PCGF' but omit
# "Herbs", "Shrubs" and "Trees".
####################################

EI.F.GROUPS <- t(as.data.frame(list(
  c("SR", "composition"),
  c("TD", "composition"),
  c("PCGF", "structure"),
  c("GC", "function"),
  c("PCESS", "integrity")
)))

# Temporarily reset this parameter if
# running TD only to fine-tune NMDS
# parameters
if (TD.ONLY == TRUE) {
  EI.F.GROUPS <- t(as.data.frame(list(
    c("TD", "composition")
  )))
}

# Final adjustments to EI.F.GROUPS
rownames(EI.F.GROUPS) <- NULL
colnames(EI.F.GROUPS) <- c("EI", "f.group")

# Some important derived parameters
df.EI.FG <- as.data.frame(EI.F.GROUPS)
EI.list <- data.frame(fname = EI.F.GROUPS[, 1])
EI.vec <- EI.F.GROUPS[, 1]

##################################
# Aggregate indicators
#
# Ecological Indicators (abbreviations) for which stratum
# scores will be combined in results files
# Set EI.agg.list to NA if not applicable for this analysis
##################################

EI.agg.list <- data.frame(fname = c(
  "PCGF",
  "PCESS",
  "GC"
))

# Include only stratum "Herbs" in indicator PCESS?
# Also allows "Hierba", "Hierbas", etc.
pcess.herbs.only <- FALSE

############################################
# Indicator group properties
#
# Notes:
# 1. Parameter function ei.params() takes an
#   indicator group code (e.g., "SR", "PCGF")
#   as its argument. The parameter values
#   returned are applicable to that indicator
#   group only. Make sure all indicators in
#   current analysis are included.
#   You can use this function to store all
#   indicator groups and their settings. But
#   only the indicator groups listed in
#   parameter EI.F.GROUPS (see above) will
#   be included in the current analysis.
# 2. Some indicator groups consist
#   of multiple subindicators that vary
#   depending on the dataset (e.g., PCGF
#   contains growth forms such as "Herbs",
#   "Shrubs", "Trees"). Other indicator groups
#   are also standalone indicators that never
#   contain subindicators (e.g., SR, TD).
#
# REVIEW CAREFULLY!
############################################

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
  scale.abund <- FALSE

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

    # Set NA to 0 for this indicator?
    NA.TO.ZERO <- TRUE

    # Input data frame and file for this indicator
    ei.data.type <- "cover" # Always cover, by definition
    df.input <- "exoticCoverByStratum"
    source.file <- paste0(df.input, ".csv") # Input file required for this indicator
  } else if (ei.code == "PCES") {
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
  } else if (ei.code == "PCS") {
    # Equivalent to PCGF but with vegetation height classes instead
    ei.name <- "Percent Cover by Stratum"
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
    df.input <- "coverByStratum"
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
    td.multiplier.omit <- ""
    td.multiplier.omit <- c(
      "Greenleaf Fescue - Hood's Sedge - Lupine species Subalpine Mesic Meadow Alliance - Reclaimed",
      "Idaho Fescue - Bluebunch Wheatgrass - Sandberg Bluegrass Dry Grassland Alliance - Reclaimed"
    )

    # Input data frame and file for this indicator
    ei.data.type <- DATA.TYPE
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
    distn <- "NBin"
    q.method <- "empirical"
    has.stratum <- TRUE
    test.tail <- "both"
    bm.val <- NA
    exclude.exotics <- FALSE
    ei.multiplier <- 1
    # NORMALIZE.BY.DBH <- TRUE
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

    # Set NA to 0 for this indicator?
    NA.TO.ZERO <- TRUE

    # Input data frame and file for this indicator
    ei.data.type <- "ind" # By definition for this indicator
    df.input <- "speciesStems"
    source.file <- paste0(df.input, ".csv")
  } else if (ei.code == "CH") {
    ei.name <- "Canopy Height"
    ei.name.with.units <- "Canopy Height (m)"
    distn <- "gamma"
    q.method <- "empirical"
    has.stratum <- FALSE
    test.tail <- "both"
    bm.val <- NA
    exclude.exotics <- FALSE
    ei.multiplier <- 1

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

##########################################
##########################################
# NMDS options
#
# Applies to indicator script "td.R" only
##########################################
##########################################

nmds.params <- function(land.cover) {
  ##############################
  # Key NMDS parameters
  #
  # In most cases the default
  # values should work.
  # Use this function to fiddle with
  # parameters to achieve convergence
  # for specific vegetation types.
  ##############################

  ###################
  # Default options
  ###################

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

  ######################################
  # Set landCover-specific metaNMDS
  # options here. Add more land cover
  # classes as needed.
  ######################################

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

###################################
# General transformation options
###################################

# Transform final indicator quality scores using inverse logit transformation
# Like logistic transformation but anchored at 0 & 1
# MUST set parameter logit.inverse.beta if set this parameter to TRUE
q.tr.logit.inverse <- FALSE

# Inflection steepness parameter for logit.inverse function.
# Only used if q.tr.logit.inverse=TRUE
logit.inverse.beta <- 2

# Convert zeros to ones (NBin distributions only)
# Currently implemented only for indicator SR (species
# richness). Prevents zero-related crashes.
zero_to_one <- TRUE

##########################################################
##########################################################
# Figure options
##########################################################
##########################################################

# Type of figure file
# Options: "pdf", "png" (just these for now; pdf for hi res only)
# If using pdf, may need to adjust additional parameters currently in graph.dists.R
FIG.TYPE <- "pdf"
FIG.TYPE <- "png"

# Set true to plot overlap graphs
# These are very time consuming if using many bootstrap iterations
# so don't print unless you need them
plot.overlap <- FALSE

# Set true to plot histograms of focal and bm distributions
plot.hist <- TRUE

# Combine all legends on right side of histogram
legend.combined <- TRUE

# Display quality only, plus CLs, in main legend text?
# If TRUE, omits overlap in percent.
# Over-rides other legend settings except plot.n
quality.legend.only <- TRUE

# Formal quality legend as percent?
# If false, keeps quality a proportion
Q.LEGEND.PERCENT <- FALSE

# Set to false to omit legends entirely from indicator histograms
plot.legends <- TRUE

# Set to FALSE to omit focal and benchmark color key from legend
LEGEND.NO.FB <- FALSE

# Subsample bootstrap pdfs for figures?
# Improves clarity and plotting time
boot.graph.subsample <- TRUE

# Allow use of discrete bar plots instead of histograms
# for discrete data. Sets bin size to exactly 1, so in
# most cases will want to set this to FALSE
allow.discrete <- FALSE

# Add mean and confidence limit lines to graph?
# over-ride by setting this variable in EI script
plot.mean <- TRUE

# Display sample sizes on histograms legends?
show.n <- TRUE

# Display overlap in results legend on figure
show.overlap <- TRUE

remove.grid <- TRUE # Set true to remove background grid

# Set TRUE to plot bootstrapped pdfs and confidence limits on overlap graph
# Set FALSE to omit
plot.boot.pdfs <- TRUE

# Set TRUE to overlay pdf fit curve on histograms of empirical sampling distribution
plot.pdf <- TRUE

# Set the following to TRUE to keep graphs without pdf fit in separate directory
# Ignored if plot.pdf = TRUE
no.fit.keep.separate <- FALSE

# Group figure mtext "line" option
# (multi-panel figures only)
# Sets vertical placement of group title
# Integer from 0:3
GROUP.MTEXT.LINE <- 1

# Which graphs to plot? TRUE/FALSE
# Each parameter is "plot." concatenated with the name
# of a subdirectory of figures based directory "figs/"
# Set to FALSE to skip printing figures in that directory
# and save a lot of hard drive space!
# The one you should generally always enable is
# plot.dists_fitted_rescaled, also
# plot.dists_fitted_grouped_rescaled if any indicators
# you are using consist of multiple strata.
# Setting all to TRUE can result in the production of
# 100 MB or more of figures per VQA run
plot.dists_fitted <- FALSE
plot.dists_fitted_bm <- FALSE
plot.dists_fitted_bm_grouped <- FALSE
plot.dists_fitted_bm_grouped_nofit <- FALSE
plot.dists_fitted_bm_grouped_rescaled <- FALSE
plot.dists_fitted_bm_grouped_rescaled_nofit <- FALSE
plot.dists_fitted_bm_nofit <- FALSE
plot.dists_fitted_bm_rescaled <- FALSE
plot.dists_fitted_bm_rescaled_nofit <- FALSE
plot.dists_fitted_focal <- FALSE
plot.dists_fitted_focal_grouped <- FALSE
plot.dists_fitted_focal_grouped_nofit <- FALSE
plot.dists_fitted_focal_grouped_rescaled <- FALSE
plot.dists_fitted_focal_grouped_rescaled_nofit <- FALSE
plot.dists_fitted_focal_nofit <- FALSE
plot.dists_fitted_focal_rescaled <- FALSE
plot.dists_fitted_focal_rescaled_nofit <- FALSE
plot.dists_fitted_grouped <- FALSE
plot.dists_fitted_grouped_nofit <- FALSE
plot.dists_fitted_grouped_rescaled <- TRUE # Enable if any indicators have strata
plot.dists_fitted_grouped_rescaled_nofit <- FALSE
plot.dists_fitted_nofit <- FALSE
plot.dists_fitted_rescaled <- TRUE # Always enable this one
plot.dists_fitted_rescaled_nofit <- FALSE # Enable if q.method=="fixed"

#################################
#################################
# vqa.summary.xl options
#
# These affect format of the final
# Excel VQA summary spreadsheet
#################################
#################################

# Remove area & QH columns from sheet q.overall?
# Use TRUE if no areas provided (i.e., analysis is quality only)
# Otherwise FALSE
VQA.XLS.NO.AREA <- TRUE

# Drop column "notes" from df q.overall
VQA.XLS.DROP.NOTES <- TRUE

# Remove rows from df q.overall where q=NA
VQA.XLS.REMOVE.Q.NA <- TRUE

# Drop rows from q.ei where include==FALSE?
VQA.XLS.DROP.INCLUDE.FALSE <- TRUE

# Output file
FNAME.Q.XLSX.OUT <- "vqa_summary.xlsx"

# Pluralize stratum names?
VQA.SUMMARY.PLURALIZE.STRATA <- FALSE

######################################
######################################
# Application file names
#
# These generally should not change
######################################
######################################

######################################
# Input file names
#
# These are the names of CSV files
# created by the import script and saved
# to directory inputs/, and imported
# by indicator and summary scripts.
# With the exception of the whitelist/
# blacklist files, the base name of
# each file is the also the name of a
# data frame created by the project-
# specific import script.
###############################

# Plot metadata data frame and input file name
DF.PLOTMETADATA <- "plotMetadata"
PLOTMETADATA.FILE <- paste0(DF.PLOTMETADATA, ".csv")

# Land cover data frame and input file name
# Made by prepare.metadata script
DF.VEG <- "vegetation"
VEG.FILE <- paste0(DF.VEG, ".csv")
DF.LANDCOVER <- "landCover"
LANDCOVER.FILE <- paste0(DF.LANDCOVER, ".csv")
METADATA.FILE <- LANDCOVER.FILE # Old parameter, keep for back-compatibility
STRATA.FILE <- LANDCOVER.FILE # Old parameter, CHECK if still needed!

# Stratum classes data frame and input file name
DF.STRATA <- "landCover" # Not sure if still needed; CHECK

# Species master list data frame and input file name
DF.SPECIES <- "species"
SPECIES.FILE <- paste0(DF.SPECIES, ".csv")

# Indicator-stratum include file.
# List of indicators & strata to include/exclude.
# When created, includes all indicators, as set in the
# parameters files and all strata present in the data.
# Edit to exclude certain indicators or indicator-stratum
# combinations on the fly. After editing, you MUST set
# REPLACE.INCLUDE.FILES <- TRUE to preserve your changes.
BLACKLIST.FILE <- "ei.stratum.include.csv"

# Indicator-stratum-bm.veg include file.
# Vegetation-specific exceptions to indicators and
# strata excluded in blacklist.file
# When created, combines all classes of bm vegetation
# present in the data with all indicators (as set in the
# parameters files) and all strata (as extracted from
# the raw data).
# Edit to exclude certain indicators or indicator-stratum
# combinations on the fly. After editing, you MUST set
# REPLACE.INCLUDE.FILES <- TRUE to preserve your changes.
WHITELIST.FILE <- "ei.stratum.veg.include.csv"

###############################
# Intermediate file names
#
# Saved to directory results/ and used
# as inputs for later scripts
###############################

# Land cover sample size summary
LC.SUMMARY.FILE <- "lc.summary.csv"
# LC.SUMMARY.FILE <- "sample.size.summary.csv"

# Details land cover sample size summary
LC.SUMMARY.DETAILED.FILE <- "lc.summary.detailed.csv"

# Summary file of parameters for the current EI
# Used by later scripts that summarize VQA results
FILENAME.EI.SUMMARY <- "ei.summary.csv"

#################################
# vqa.summary file names
#################################

# Combined focal indicator values & quality calculations
SUMMARY.FOCAL.EI.FILE <- "summary_focal_ei.csv"

# Vector of bootstrapped EI quality
SUMMARY.FOCAL.EI.BOOT.FILE <- "summary_q.ei_boot.csv"

# Combined bm indicator values
SUMMARY.BM.EI.FILE <- "summary_bm_ei.csv"

# Functional group quality by land cover class
SUMMARY.FOCAL.FG.FILE <- "summary_focal_fg.csv"

# Functional group quality by benchmark vegetation class
SUMMARY.FOCAL.FG.BM.FILE <- "summary_focal_fg_bm.csv"

# Overall quality and QH by land cover class
# Anthropogenic land cover classes separate
SUMMARY.FOCAL.ALL.FILE <- "summary_focal_q_qh_all.csv"

# Overall quality and QH by land cover class
# Anthropogenic land cover classes combined
SUMMARY.FOCAL.FILE <- "summary_focal_q_qh.csv"

# Overall QH by benchmark vegetation class
SUMMARY.FOCAL.BM.FILE <- "summary_focal_qh_bm.csv"

# Vector of bootstrapped overall quality, by landcover class
SUMMARY.Q.OVERALL.BOOT.LC.FILE <- "summary_q.overall_boot_lc.csv"

# Vector of bootstrapped QH, by benchmark vegetation
SUMMARY.QH.BOOT.BM.FILE <- "summary_qh_boot_bm.csv"

#################################
# vqa.summary.csv file names
#################################

# Output files, in same order
FNAME.Q.BM.OUT <- "vqa_summary_qh_bm.csv"
FNAME.Q.OVERALL.OUT <- "vqa_summary_q.overall.csv"
FNAME.Q.FG.OUT <- "vqa_summary_q.fg.csv"
FNAME.Q.EI.OUT <- "vqa_summary_q.ei.csv"

#####################################
#####################################
# API parameters
#####################################
#####################################

##########################################
# Taxonomic Name Resolution Service (TNRS)
# See: https://tnrs.biendata.org
# For resolving species names and looking
# up standardized family classifications
##########################################

# Base URL for TNRS api
TNRS_URL <- "http://vegbiendev.nceas.ucsb.edu:8975/tnrs_api.php"

# TNRS options
# Other two options (mode, matches) set on the fly depending on
# output desired
TNRS_SOURCES <- "tropicos,tpl,usda" # Taxonomic sources
TNRS_CLASS <- "tropicos" # Family classification source

# Request headers
TNRS_HEADERS <- list("Accept" = "application/json", "Content-Type" = "application/json", "charset" = "UTF-8")

#####################################
#####################################
# Load project/assessment parameters
#
# May over-ride one or more of the
# preceding parameters.
#
# Requires the following parameters
# from script params.pa.R:
#   PROJ
#   ASSESS
#   PARAMS.USE.ASSESS
#####################################
#####################################

# Set project-specific parameters file name
params.proj.filename <- paste0("params.", PROJ, ".R")

if (exists("PARAMS.USE.ASSESS")) {
  if (PARAMS.USE.ASSESS == TRUE) {
    params.proj.filename <- paste0("params.", PROJ, ".", ASSESS, ".R")
  }
}
# params.proj.file <- paste0( "params/", params.proj.filename )
params.proj.file <- paste0(BASEDIR_PSFILES, "params/", params.proj.filename)

# Load project-specific parameters file if exists
if (file.exists(params.proj.file)) {
  sys.source(params.proj.file, envir = legacy.run.env)
} else {
  # Warn file doesn't exist & continue, using default parameters
  cat("\nWARNING: project-specific parameters file '", params.proj.file, "' not found!\n\n", sep = "")
}

legacy.seed <- if (exists("VQA_TEST_SEED")) VQA_TEST_SEED else Sys.getenv("VQA_TEST_SEED", "")
if (nzchar(legacy.seed)) {
  seed <- suppressWarnings(as.integer(legacy.seed))
  if (is.na(seed) || as.character(seed) != legacy.seed) {
    stop_quietly("ERROR: VQA_TEST_SEED must be an integer.\n")
  }
  set.seed <- TRUE
}

#####################################
#####################################
# Load parameter confirmation messages
#####################################
#####################################

sys.source(paste0(SRCDIR, "params.conf.general.R"), envir = legacy.run.env)

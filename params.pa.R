##################################################
##################################################
# Project & assessment parameters
#
# First file loaded for all VQA operations.
# Parameters in this file determine which set of
# project- and assessment-specific parameters
# is loaded and which data input/output folder
# is used. Check carefully!
#
# Parameter definitions:
# PROJ: Short code of project
# * Must match project data directory name: "data/<PROJ>/"
# ASSESS: Assessment code
# * Must match assessment data dir name: "data/<PROJ>/<ASSESS>/"
# PARAMS.USE.ASSESS: Use assessment-specific params file? (TRUE|FALSE*)
# * TRUE:
#   * MUST include assessment code in name of project-specific params file
#   * Name format: params.<PROJ>.<ASSESS>.R
#   * Params are specific to single assessment only
# * FALSE:
#   * All assessments use same project parameter file
# * Default is FALSE; include this parameter only if TRUE
# IMPORT.USE.ASSESS: Use assessment-specific import script? (TRUE|FALSE)
# * TRUE:
#   * MUST include assessment code in name of project-specific import script
#   * Name format: import.<PROJ>.<ASSESS>.R
#   * Import file is specific to single assessment only
# * FALSE:
#   * All assessments use same project import file
# * Default is FALSE; include this parameter only if TRUE
#
# Project and assessment naming conventions:
# * No spaces
# * Numbers and letters OK
# * No punctuation except for "-" and "_"
# * No periods (.) in PROJ or ASSESS. Periods are reserved
#   for separating PROJ & ASSESS in file names.
##################################################
##################################################

##################################################
##################################################
# Saved parameters
#
# This section is for saving sets of project and
# assessment parameters. To run a project, copy
# the parameter set from this section and paste it
# at the end under "Current project".
##################################################
##################################################

###########################################
# VQA demo #1
#
# Demo application which calculates
# current quality for a project site and
# one offset. No areas are included
# and quality hectares are not calculated.
#
# Key features of this demo:
# * Calculation of quality only, no area or
#   quality hectares
# * Import of data from Microsoft Access
# * Division of benchmark vegetation
#   into seral stages
###########################################

# Project site current assessment
PROJ <- "vqa-demo1"
ASSESS <- "main_001_current"

# Offset current assessment
PROJ <- "vqa-demo1"
ASSESS <- "offset_001_current"

##########################################
# VQA demo #2
#
# Full demo with baseline and current
# assessments for the project site
# and one offset. Offset baseline quality
# is assumed to be 0; project baseline
# quality is determined empirically from
# actual data. Areas of sampling units
# (sites) are included, allowing
# calculation of quality hectares for each
# assessment and overall net quality
# hectares (NPI) for the project,
# including offsets.
#
# Key features of this demo:
# * Calculation of quality, quality hectares
#   and overall net quality hectares
# * Calculation of net quality hectares with
#   and without offset
# * Offset baseline with fixed quality=0
#   ("averted-loss offset")
# * Import of data from Microsoft Excel
##########################################

# Project baseline assessment
PROJ <- "vqa-demo2"
ASSESS <- "project_baseline"

# Project current assessment
PROJ <- "vqa-demo2"
ASSESS <- "project_current"

# Offset baseline assessment
PROJ <- "vqa-demo2"
ASSESS <- "offset_baseline"

# Offset current assessment
PROJ <- "vqa-demo2"
ASSESS <- "offset_current"

##################################################
##################################################
# Project & assessment to run
#
# Copy the project to run from 'Saved
# parameters' and paste below at bottom
##################################################
##################################################

# Reset to default values of PARAMS.USE.ASSESS and IMPORT.USE.ASSES
# DO NOT DELETE, CHANGE OR MOVE!
PARAMS.USE.ASSESS <- FALSE
IMPORT.USE.ASSESS <- FALSE

# Unset PROJ & ASSESS to throw error instead of
# accidentally reusing last saved values
# DO NOT DELETE, CHANGE OR MOVE!
rm(PROJ, ASSESS)

# ***********************************
# **** Paste PROJ & ASSESS below ****
# ***********************************

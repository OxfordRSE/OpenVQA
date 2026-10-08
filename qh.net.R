###################################################
# Net Quality Hectares (QH.net)
#
# Calculates QH.net of current assessment(s) by
# comparing Q and QH results for current assessment
# to baseline assessments
#
# **** Required input files ****
# 	  SUMMARY.FOCAL.BM.FILE
#     SUMMARY.QH.BOOT.BM.FILE
###################################################

#########################################
# Set paths, load functions & parameters
#########################################

rm(list = ls()) # clear the workspace

if (!exists("params.loaded", mode = "function")) {
  # Set working directory (base application directory)
  # Must set first to set remaining directory parameters
  wd <- getwd()
  setwd(wd)
  SRCDIR <- paste0(wd, "/")
}

# Set job so params.R know which parameters to load
job <- "qh.net"

source("includes/functions.R", local = TRUE)
# source("params.R")
source(paste0(SRCDIR, "params.R"))

###################################################
# Functions
###################################################

qh.boot.transform <- function(q.boot, q, veg.shared) {
  #############################################
  # Convert df of bootstrap Qs to QHs, shared vegetation only
  #############################################

  q.boot.area.shared <- q[
    q$bm.veg %in% veg.shared,
    c("bm.veg", "area_ha")
  ]
  q.boot.shared <- q.boot[q.boot$bm.veg %in% veg.shared, ]
  qh.boot <- merge(q.boot.area.shared, q.boot.shared, by = c("bm.veg"))
  areas <- qh.boot$area_ha

  for (c in 3:ncol(qh.boot)) {
    qh.boot[, c] <- qh.boot[, c] * areas
  }

  return(qh.boot)
}

get.boot.cls <- function(boot.q, boot.reps) {
  ############################################################
  # Calculate upper and lower bootstrap 95% confidence limits
  # Accepts:
  #   boot.q: df of 1 observation with n=boot.reps+1 columns,
  #     the first column the category (e.g., vegClass) and the
  #     remaining columns the bootstrapped values
  #   boot.reps: number of bootstrap repetitions
  # Returns: the CLs as a 2 element vector
  ############################################################

  # Initialize vector to hold the means of each bootstrap run
  q.vec <- rep(NA, boot.reps)

  # For each bootstrap run, get mean quality across all EIs and
  # save to vector, omitting NAs and Infinity
  for (n in 1:boot.reps) {
    rep.col <- paste0("X", n)
    rep.curr.boot.q <- curr.boot.q[[rep.col]][!(curr.boot.q[[rep.col]] == Inf) &
      !(is.na(curr.boot.q[[rep.col]]))]
    q.vec[n] <- mean(rep.curr.boot.q, na.rm = T) # Avg quality for the current run
  }

  boot.mean <- mean(q.vec, na.rm = T) # grand mean quality across all boot runs
  boot.devs <- boot.mean - q.vec # run deviances from the grand mean

  # Return 95% CLs of the deviances
  # Remember that 95% CLs are quantiles c(0.025, 0.975)!
  boot.quant <- quantile(boot.devs, c(0.025, 0.975), na.rm = T)
  boot.quant <- as.vector(boot.quant)

  return(boot.quant)
}

###############################
# Rename some parameters
###############################

# Baseline assessment code
# MUST be same as name of an existing baseline assessment
# directory containing a full set of baseline VQA results
# This value is set in params.R
assess.baseline <- ASSESS.BASELINE

# Current assessment to be compared to baseline
# MUST be same as name of an existing baseline assessment
# directory containing a full set of baseline VQA results
# This value is set in pa.params.R
assess.current <- ASSESS

# Vector of current assessment code, plus offset codes if applicable
assess.offset <- ASSESS.OFFSET

# Input file names
qh.filename <- SUMMARY.FOCAL.BM.FILE # Area, Q and QH by benchmark vegetation
qh.boot.filename <- SUMMARY.QH.BOOT.BM.FILE # Bootstrap Q, by benchmark vegetation

# Main qh.net directory
qh.net.dir <- QH.NET.DIR

# QH.net results directory
qh.net.resultsdir <- QH.NET.RESULTSDIR

##########################################
# Report parameters & confirm operation
##########################################

# Display confirmation message
cat(paste0("Calculate QH.net for project '", PROJ, "' using the following settings: \n"))
cat(paste0(MSG.CONF.START, MSG.CONF.QH.NET))

if (interactive() == FALSE) {
  yes <- c("y", "Y", "Yes", "yes")
  cat("Continue? (y/n):")
  response <- readLines("stdin", n = 1)
  if (!response %in% yes) stop_quietly("Operation cancelled\n\n")
} else {
  cat("\n\n")
}

cat("\n")
cat("##########################################\n")
cat("Begin operation\n")
cat("\n")
source("libraries.R")

###################################################
# Main
###################################################

cat("\n******************************************\n")
cat("Calculating QH.net\n\n")

###########################
# Create results directory
###########################

qh.net.resultsdir <- paste0(qh.net.dir, "results/")

# Create qh.net data directories if not already exist
dir.create(file.path(qh.net.resultsdir), recursive = TRUE, showWarnings = FALSE)

###########################
# Load data
###########################

cat("******************************************\n")
cat("Importing data\n\n")

# Current assessment
cat("Loading QH results for current assessment '", assess.current, "':\n", sep = "")
data.dir <- QH.NET.INPUTDIR.CURRENT

cat("- Loading file '", qh.filename, "'...", sep = "")
qh.file <- paste0(data.dir, qh.filename)
if (file.exists(qh.file)) {
  qh.current <- read.csv(qh.file, header = T) # QH file
  cat("done\n")
} else {
  stop_quietly("ERROR: file not found!\n")
}
cat("- Loading file '", qh.boot.filename, "'...", sep = "")
qh.boot.file <- paste0(data.dir, qh.boot.filename)
if (file.exists(qh.boot.file)) {
  qh.boot.current <- read.csv(qh.boot.file, header = T) # Boostrap QH file
  cat("done\n")
} else {
  stop_quietly("ERROR: file not found!\n")
}


# Baseline assessment
cat("Loading QH results for baseline assessment '", assess.baseline, "':\n", sep = "")
data.dir <- QH.NET.INPUTDIR.BASELINE

cat("- Loading file '", qh.filename, "'...", sep = "")
qh.file <- paste0(data.dir, qh.filename)

if (file.exists(qh.file)) {
  qh.baseline <- read.csv(qh.file, header = T) # QH file
  cat("done\n")
} else {
  stop_quietly("ERROR: file not found!\n")
}
cat("- Loading file '", qh.boot.filename, "'...", sep = "")
qh.boot.file <- paste0(data.dir, qh.boot.filename)
if (file.exists(qh.boot.file)) {
  qh.boot.baseline <- read.csv(qh.boot.file, header = T) # Boostrap QH file
  cat("done\n")
} else {
  stop_quietly("ERROR: file not found!\n")
}

# Offset
if (INCLUDE.OFFSET) {
  cat("Loading QH.net results for offset assessment '", assess.offset, "':\n", sep = "")
  data.dir <- QH.NET.INPUTDIR.OFFSET

  qh.net.filename <- paste0("qh.net_", assess.offset, ".csv")
  cat("- Loading file '", qh.net.filename, "'...", sep = "")
  qh.net.file <- paste0(data.dir, qh.net.filename)
  if (file.exists(qh.net.file)) {
    qh.net.offset <- read.csv(qh.net.file, header = T) # QH file
    cat("done\n")
  } else {
    stop_quietly("ERROR: file not found!\n")
  }

  qh.net.boot.filename <- paste0("qh.net_", assess.offset, "_boot.csv")
  cat("- Loading file '", qh.net.boot.filename, "'...", sep = "")
  qh.net.boot.file <- paste0(data.dir, qh.net.boot.filename)
  if (file.exists(qh.net.boot.file)) {
    qh.net.boot.offset <- read.csv(qh.net.boot.file, header = T) # Boostrap QH file
    cat("done\n")
  } else {
    stop_quietly("ERROR: file not found!\n")
  }
}

#########################################
# Confirm number of bootstrap replicates
# equal in all assessments
#########################################

cat("Checking bootstrap sample sizes:\n")
cat("- Parameter 'boot.reps': ", boot.reps, "\n", sep = "")

boot.reps.baseline <- ncol(qh.boot.baseline) - 1
cat("- Baseline assessment boot.reps: ", boot.reps.baseline, "\n", sep = "")

boot.reps.current <- ncol(qh.boot.current) - 1
cat("- Current assessment boot.reps: ", boot.reps.current, "\n", sep = "")

if (INCLUDE.OFFSET) {
  boot.reps.offset <- ncol(qh.net.boot.offset) - 1
  cat("- Offset assessment boot.reps: ", boot.reps.offset, "\n", sep = "")
}

# Abort if they don't all match up
if (INCLUDE.OFFSET) {
  boot.reps.all <- c(boot.reps.baseline, boot.reps.current, boot.reps.offset)
} else {
  boot.reps.all <- c(boot.reps.baseline, boot.reps.current)
}

if (all(boot.reps.all == boot.reps)) {
  cat("- All boot.reps equal\n")
} else {
  stop_quietly("ERROR: boot.reps differ!\n")
}

#########################################
# Transform vegetation types, if requested
# (see "QH.net options" in project-specific
# parameters file)
#########################################

if (BM.VEG.TRANSFORM) {
  cat("Transforming vegetation types, as requested...")
  qh.current$bm.veg <- transform_column(qh.current$bm.veg, BM.VEG.TRANSFORM.VECTOR)
  qh.boot.current$bm.veg <- transform_column(qh.boot.current$bm.veg, BM.VEG.TRANSFORM.VECTOR)
  qh.baseline$bm.veg <- transform_column(qh.baseline$bm.veg, BM.VEG.TRANSFORM.VECTOR)
  qh.boot.baseline$bm.veg <- transform_column(qh.boot.baseline$bm.veg, BM.VEG.TRANSFORM.VECTOR)
  qh.net.offset$bm.veg <- transform_column(qh.net.offset$bm.veg, BM.VEG.TRANSFORM.VECTOR)
  qh.net.boot.offset$bm.veg <- transform_column(qh.net.boot.offset$bm.veg, BM.VEG.TRANSFORM.VECTOR)
  cat("done\n")
}

#########################################
# Prepared merged QH.net summary
#########################################

cat("Preparing data frame qh.net (1:1 compensation only):\n")

cat("- Merging baseline and current QH into single df...")

# Prepare qh.baseline
qh.baseline <- qh.baseline[, c("bm.veg", "area_ha", "qh", "qh.lcl", "qh.ucl")]
names(qh.baseline) <- c("bm.veg", "area_ha0", "qh0", "qh.lcl0", "qh.ucl0")
qh.baseline$baseline <- TRUE

# Prepare qh.current
qh.current <- qh.current[, c("bm.veg", "area_ha", "qh", "qh.lcl", "qh.ucl")]
names(qh.current) <- c("bm.veg", "area_ha1", "qh1", "qh.lcl1", "qh.ucl1")
qh.current$current <- TRUE

# Create qh.net by left join of baseline to current
# qh.net <- merge(qh.baseline, qh.current, by=c("bm.veg"), all.x=TRUE)
qh.net <- merge(qh.baseline, qh.current, by = c("bm.veg"), all = TRUE)
qh.net <- df.reorder(qh.net, col.move = "baseline", move.last = TRUE)
qh.net <- df.reorder(qh.net, col.move = "current", move.last = TRUE)

# Replace NAs of non-matching vegetation in baseline assessment with zeros
qh.net$area_ha0[is.na(qh.net$baseline)] <- 0
qh.net$qh0[is.na(qh.net$baseline)] <- 0
qh.net$qh.lcl0[is.na(qh.net$baseline)] <- 0
qh.net$qh.ucl0[is.na(qh.net$baseline)] <- 0

# Replace NAs of non-matching vegetation in current assessment with zeros
qh.net$area_ha1[is.na(qh.net$current)] <- 0
qh.net$qh1[is.na(qh.net$current)] <- 0
qh.net$qh.lcl1[is.na(qh.net$current)] <- 0
qh.net$qh.ucl1[is.na(qh.net$current)] <- 0

cat("done\n")

cat("- Calculating QH.net...")
qh.net$qh.net <- qh.net$qh1 - qh.net$qh0
cat("done\n")

#####################################
# QH.net confidence limits
#
# Bootstrap dfs MUST have same number of columns for this to work!
# Hence earlier check that all boot.reps match
#####################################
cat("- Calculating bootstrapped QH.net...")

# Add empty CL columns to data frame
qh.net$qh.net.lcl <- NA
qh.net$qh.net.ucl <- NA

# Vegetation shared by both bootstrap files
veg.shared <- intersect(
  qh.boot.baseline$bm.veg,
  qh.boot.current$bm.veg
)

# Prune bootstrap QH dfs to shared vegetation only
qh.boot.baseline <- qh.boot.baseline[qh.boot.baseline$bm.veg %in% veg.shared, ]
qh.boot.current <- qh.boot.current[qh.boot.current$bm.veg %in% veg.shared, ]

# Order identically by bm.veg (critical!)
qh.boot.baseline <- qh.boot.baseline[order(qh.boot.baseline$bm.veg), ]
qh.boot.current <- qh.boot.current[order(qh.boot.current$bm.veg), ]

# Initialize qh.boot as copy of qh.baseline filled with NAs
qh.boot <- qh.boot.baseline
NAs <- rep(NA, length(veg.shared))
for (c in 2:ncol(qh.boot)) qh.boot[, c] <- NAs

# Bootstrapped QH.net
for (col in 2:ncol(qh.boot.baseline)) {
  qh.boot[, col] <- qh.boot.current[, col] - qh.boot.baseline[, col]
}

# Unique name for saving later, just in case
qh.net.boot.main <- qh.boot

cat("done\n")

# Empirical bootstrap CLs of QH.net
cat("- Estimating bootstrap confidence limits...")
for (curr.veg in veg.shared) { # START "for (curr.veg in veg.shared)"

  # Get bootstrap results for all EIs for current land cover class
  curr.boot.q <- qh.boot[qh.boot$bm.veg == curr.veg, ]

  # Calculate bootstrap confidence limits
  # Returns 2 element vector boot.cls
  bootstrap.cls <- get.boot.cls(boot.q = curr.boot.q, boot.reps = boot.reps)
  lower <- bootstrap.cls[1]
  upper <- bootstrap.cls[2]

  # Subtract bootstrap deviances from the observed quality
  qh.net$qh.net.lcl[qh.net$bm.veg == curr.veg] <-
    qh.net$qh.net[qh.net$bm.veg == curr.veg] + lower
  qh.net$qh.net.ucl[qh.net$bm.veg == curr.veg] <-
    qh.net$qh.net[qh.net$bm.veg == curr.veg] + upper
} # END "for (curr.veg in veg.shared)"

# Calculate remaining CLs
# These should be vegetation types with one or both assessments all 0 QHs.
# In these cases it is valid to simply use the existing CLs. These will be either
# both zero or one non-zero only.
qh.net$qh.net.lcl[is.na(qh.net$qh.net.lcl)] <-
  qh.net$qh.lcl1[is.na(qh.net$qh.net.lcl)] -
  qh.net$qh.lcl0[is.na(qh.net$qh.net.lcl)]
qh.net$qh.net.ucl[is.na(qh.net$qh.net.ucl)] <-
  qh.net$qh.ucl1[is.na(qh.net$qh.net.ucl)] -
  qh.net$qh.ucl0[is.na(qh.net$qh.net.ucl)]
cat("done\n")

if (INCLUDE.OFFSET) { # BEGIN (INCLUDE.OFFSET)
  cat("Incorporating additional QHs from offset:\n")

  cat("- Calculating actual total QH.net...")
  qh.net.offset.bak <- qh.net.offset
  qh.net.offset <- qh.net.offset[, c("bm.veg", "qh.net", "qh.net.lcl", "qh.net.ucl")]
  colnames(qh.net.offset) <- c("bm.veg", "qh.off", "qh.off.lcl", "qh.off.ucl")
  qh.net.comb <- merge(
    qh.net.offset,
    qh.net[, c("bm.veg", "qh.net", "qh.net.lcl", "qh.net.ucl")],
    by = "bm.veg"
  )
  qh.net.comb$qh.net.final <- rowSums(qh.net.comb[, c("qh.off", "qh.net")], na.rm = TRUE)
  qh.net.comb$qh.net.final.lcl <- NA
  qh.net.comb$qh.net.final.ucl <- NA
  cat("done\n")

  cat("- Calculating bootstrapped total QH.net...")
  qh.boot.pruned <- qh.boot
  veg.shared <- intersect(qh.boot.pruned$bm.veg, qh.net.boot.offset$bm.veg)
  qh.boot.pruned <- qh.boot.pruned[qh.boot.pruned$bm.veg %in% veg.shared, ]
  qh.net.boot.offset <- qh.net.boot.offset[qh.net.boot.offset$bm.veg %in% veg.shared, ]
  qh.boot.pruned <- qh.boot.pruned[order(qh.boot.pruned$bm.veg), ]
  qh.net.boot.offset <- qh.net.boot.offset[order(qh.net.boot.offset$bm.veg), ]

  # Initialize qh.boot as copy of qh.baseline filled with NAs
  qh.boot.comb <- qh.net.boot.offset
  NAs <- rep(NA, length(veg.shared))
  for (c in 2:ncol(qh.boot.comb)) qh.boot.comb[, c] <- NAs

  # Bootstrapped QH.net
  for (col in 2:ncol(qh.net.boot.offset)) {
    qh.boot.comb[, col] <- qh.boot.pruned[, col] + qh.net.boot.offset[, col]
  }
  cat("done\n")

  # Empirical bootstrap CLs of total QH.net
  cat("- Estimating bootstrap confidence limits...")
  for (curr.veg in veg.shared) { # START "for (curr.veg in veg.shared)"

    # Get bootstrap results for all EIs for current land cover class
    curr.boot.q <- qh.boot.comb[qh.boot.comb$bm.veg == curr.veg, ]

    # Calculate bootstrap confidence limits
    # Returns 2 element vector boot.cls
    bootstrap.cls <- get.boot.cls(boot.q = curr.boot.q, boot.reps = boot.reps)
    lower <- bootstrap.cls[1]
    upper <- bootstrap.cls[2]

    # Subtract bootstrap deviances from the observed quality
    qh.net.comb$qh.net.final.lcl[qh.net.comb$bm.veg == curr.veg] <-
      qh.net.comb$qh.net.final[qh.net.comb$bm.veg == curr.veg] + lower
    qh.net.comb$qh.net.final.ucl[qh.net.comb$bm.veg == curr.veg] <-
      qh.net.comb$qh.net.final[qh.net.comb$bm.veg == curr.veg] + upper
  } # END "for (curr.veg in veg.shared)"
  cat("done\n")

  # Merge results back into main data frame
  drop.cols <- c("qh.net", "qh.net.lcl", "qh.net.ucl")
  qh.net.comb <- qh.net.comb[, !names(qh.net.comb) %in% drop.cols]
  qh.net.off <- merge(qh.net, qh.net.comb, by = "bm.veg", all.x = TRUE)

  # Copy over original qh.net for vegetation unmatched in offset
  qh.net.off$qh.net.final[is.na(qh.net.off$qh.net.final)] <-
    qh.net.off$qh.net[is.na(qh.net.off$qh.net.final)]
  qh.net.off$qh.net.final.lcl[is.na(qh.net.off$qh.net.final.lcl)] <-
    qh.net.off$qh.net.lcl[is.na(qh.net.off$qh.net.final.lcl)]
  qh.net.off$qh.net.final.ucl[is.na(qh.net.off$qh.net.final.ucl)] <-
    qh.net.off$qh.net.ucl[is.na(qh.net.off$qh.net.final.ucl)]

  # Set qh.net of unmatched offset vegetation to zero
  qh.net.off[is.na(qh.net.off)] <- 0
} # END (INCLUDE.OFFSET)

##############################
# Final cosmetic fixes
##############################

cat("- Composing final data frame...")
# Remove unwanted columns
drop.cols <- c("assess1", "baseline", "current")
qh.net <- qh.net[, !(names(qh.net) %in% drop.cols)]
qh.net.main <- qh.net

if (INCLUDE.OFFSET) {
  qh.net.off <- qh.net.off[, !(names(qh.net.off) %in% drop.cols)]
}

cat("done\n")

##############################
##############################
# Optional QH.net analyses
##############################
##############################

if (PREPARE.QH.NET.P) { # BEGIN PREPARE.QH.NET.P
  # Prepare df qh.net.p (potential additional quality hectares)
  # Same as qh.net, but uses the 1-Q, the complement of Q.
  # This provides an assessment of potential *additional* QH,
  # available if qh.current is increased to 1 (the maximum possible)
  # for all vegetation

  cat("Preparing data frame of potential additional QHs (qh.net.p):\n")

  cat("- Copying qh.net to qh.net.p...")
  qh.net.p <- qh.net
  cat("done\n")

  cat("- Recalculating q1, qh1 and qh.net and setting CL fields to NA...")
  qh.net.p$q1 <- 1 - qh.net.p$q1
  qh.net.p$qh1 <- qh.net.p$q1 * qh.net.p$area_ha1
  qh.net.p$qh.lcl1 <- NA
  qh.net.p$qh.ucl1 <- NA
  qh.net.p$qh.net <- qh.net.p$qh1 - qh.net.p$qh0
  qh.net.p$qh.net.lcl <- NA
  qh.net.p$qh.net.ucl <- NA

  if (QH.NET.P.ZERO.AREA.RESET) {
    # Reset quality of offset vegetation with area_ha1==0 back to zero,
    # on assumption that absent vegetetion will not be created from scratch.
    # This is basically a restoration management decision
    qh.net.p$q1[qh.net.p$area_ha1 == 0] <- 0
    qh.net.p$qh1[qh.net.p$area_ha1 == 0] <- 0
    qh.net.p$qh.lcl1[qh.net.p$area_ha1 == 0] <- 0
    qh.net.p$qh.ucl1[qh.net.p$area_ha1 == 0] <- 0
    qh.net.p$qh.net[qh.net.p$area_ha1 == 0] <-
      qh.net.p$qh1[qh.net.p$area_ha1 == 0] -
      qh.net.p$qh0[qh.net.p$area_ha1 == 0]
    qh.net.p$qh.net.lcl[qh.net.p$area_ha1 == 0] <- 0
    qh.net.p$qh.net.ucl[qh.net.p$area_ha1 == 0] <- 0
  }
  cat("done\n")

  #####################################
  # QH.net confidence limits
  #####################################

  cat("- Re-calculating bootstrap confidence limits...")

  # Calculate df qh.current.p
  qh.current.copy <- merge(qh.current, qh.net.p[, c("bm.veg"), drop = FALSE], by = "bm.veg")
  qh.current.copy <- qh.current.copy[, !(names(qh.current.copy) == "notes")]
  qh.current.p <- qh.current.copy
  qh.current.p[, c("q", "q.lcl", "q.ucl", "qh", "qh.lcl", "qh.ucl")] <- NA
  qh.current.p$q <- 1 - qh.current.copy$q
  qh.current.p$q.lcl <- qh.current.p$q - (qh.current.copy$q - qh.current.copy$q.lcl)
  qh.current.p$q.ucl <- qh.current.p$q + (qh.current.copy$q.ucl - qh.current.copy$q)
  qh.current.p$qh <- qh.current.p$q * qh.current.p$area_ha
  qh.current.p$qh.lcl <- qh.current.p$q.lcl * qh.current.copy$area_ha
  qh.current.p$qh.ucl <- qh.current.p$q.ucl * qh.current.copy$area_ha
  rm(qh.current.copy)

  # Update qh CLs in qh.net.p while we're at it, reusing veg.shared
  qh.net.p[qh.net.p$bm.veg %in% veg.shared, c("qh.lcl1", "qh.ucl1")] <-
    qh.current.p[qh.current.p$bm.veg %in% veg.shared, c("qh.lcl", "qh.ucl")]

  # Calculate df qh.boot.current.p
  qh.boot.current.p <- qh.boot.current
  qh.boot.current.p[, 2:ncol(qh.boot.current.p)] <- 1 - qh.boot.current.p[, 2:ncol(qh.boot.current.p)]

  # Calculate current bootstrapped QHs, reusing veg.shared
  qh.boot.current.p <- qh.boot.transform(
    q.boot = qh.boot.current.p, q = qh.current.p,
    veg.shared
  )
  qh.boot.current.p <- qh.boot.current.p[, !(names(qh.boot.current.p) %in% c("area_ha"))]

  # Reuse original qh.boot.baseline
  qh.boot.p <- qh.boot.baseline
  NAs <- rep(NA, length(veg.shared))
  for (c in 2:ncol(qh.boot.p)) qh.boot.p[, c] <- NAs

  # Bootstrapped QH.net
  for (c in 2:ncol(qh.boot.baseline)) {
    qh.boot.p[, c] <- qh.boot.current.p[, c] - qh.boot.baseline[, c]
  }

  # Empirical bootstrap CLs of QH.net
  for (curr.veg in veg.shared) { # START "for (curr.veg in veg.shared)"

    # Get bootstrap results for all EIs for current land cover class
    curr.boot.q <- qh.boot.p[qh.boot.p$bm.veg == curr.veg, ]

    # Calculate bootstrap confidence limits
    # Returns 2 element vector boot.cls
    bootstrap.cls <- get.boot.cls(boot.q = curr.boot.q, boot.reps = boot.reps)
    lower <- bootstrap.cls[1]
    upper <- bootstrap.cls[2]

    # Subtract bootstrap deviances from the observed quality
    qh.net.p$qh.net.lcl[qh.net.p$bm.veg == curr.veg] <-
      qh.net.p$qh.net[qh.net.p$bm.veg == curr.veg] + lower
    qh.net.p$qh.net.ucl[qh.net.p$bm.veg == curr.veg] <-
      qh.net.p$qh.net[qh.net.p$bm.veg == curr.veg] + upper
  } # END "for (curr.veg in veg.shared)"

  # Calculate remaining CLs
  # These should be vegetation types with one or both assessments all 0 QHs.
  # In these cases it is valid to simply use the existing CLs. These will be either
  # both zero or one non-zero only.
  qh.net.p$qh.net.lcl[is.na(qh.net.p$qh.net.lcl)] <-
    qh.net.p$qh.lcl1[is.na(qh.net.p$qh.net.lcl)] -
    qh.net.p$qh.lcl0[is.na(qh.net.p$qh.net.lcl)]
  qh.net.p$qh.net.ucl[is.na(qh.net.p$qh.net.ucl)] <-
    qh.net.p$qh.ucl1[is.na(qh.net.p$qh.net.ucl)] -
    qh.net.p$qh.ucl0[is.na(qh.net.p$qh.net.ucl)]

  cat("done\n")
} # END PREPARE.QH.NET.P

if (ALLOW.TRADE.UP) { # BEGIN ALLOW.TRADE.UP
  #####################################
  # Calculate alternate results with
  # trade-ups from vegetation with
  # excess QH to vegetation with
  # negative net QH
  #####################################

  ##############################
  # Set up trade-up QH.net table
  ##############################

  cat("Preparing data frame qh.net.all (trade-up transactions allowed):\n")

  cat("- Creating qh.net.all...")

  # Create qh.all by full outer join on baseline and current
  qh.net.all <- merge(qh.baseline, qh.current, by = c("bm.veg"), all = TRUE)
  qh.net.all <- df.reorder(qh.net.all, col.move = "baseline", move.last = TRUE)
  qh.net.all <- df.reorder(qh.net.all, col.move = "current", move.last = TRUE)

  # Replace NAs of non-matching vegetation with zeros
  qh.net.all$area_ha0[is.na(qh.net.all$area_ha0)] <- 0
  qh.net.all$q0[is.na(qh.net.all$q0)] <- 0
  qh.net.all$qh0[is.na(qh.net.all$qh0)] <- 0
  qh.net.all$qh.lcl0[is.na(qh.net.all$qh.lcl0)] <- 0
  qh.net.all$qh.ucl0[is.na(qh.net.all$qh.ucl0)] <- 0
  qh.net.all$area_ha1[is.na(qh.net.all$area_ha1)] <- 0
  qh.net.all$q1[is.na(qh.net.all$q1)] <- 0
  qh.net.all$qh1[is.na(qh.net.all$qh1)] <- 0
  qh.net.all$qh.lcl1[is.na(qh.net.all$qh.lcl1)] <- 0
  qh.net.all$qh.ucl1[is.na(qh.net.all$qh.ucl1)] <- 0

  # Fix "Anthropogenic/non-vegetated"
  qh.net.all$q0[qh.net.all$bm.veg == "Anthropogenic/non-vegetated"] <- 0
  qh.net.all$qh0[qh.net.all$bm.veg == "Anthropogenic/non-vegetated"] <- 0
  qh.net.all$qh.lcl0[qh.net.all$bm.veg == "Anthropogenic/non-vegetated"] <- 0
  qh.net.all$qh.ucl0[qh.net.all$bm.veg == "Anthropogenic/non-vegetated"] <- 0
  qh.net.all$q1[qh.net.all$bm.veg == "Anthropogenic/non-vegetated"] <- 0
  qh.net.all$qh1[qh.net.all$bm.veg == "Anthropogenic/non-vegetated"] <- 0
  qh.net.all$qh.lcl1[qh.net.all$bm.veg == "Anthropogenic/non-vegetated"] <- 0
  qh.net.all$qh.ucl1[qh.net.all$bm.veg == "Anthropogenic/non-vegetated"] <- 0
  qh.net.all$qh.net[qh.net.all$bm.veg == "Anthropogenic/non-vegetated"] <- 0
  qh.net.all$qh.net.lcl[qh.net.all$bm.veg == "Anthropogenic/non-vegetated"] <- 0
  qh.net.all$qh.net.ucl[qh.net.all$bm.veg == "Anthropogenic/non-vegetated"] <- 0

  # Calculate provisional mean qh.net and add placeholders for CLs of qh.net
  qh.net.all$qh.net <- NA
  qh.net.all$qh.net <- qh.net.all$qh1 - qh.net.all$qh0
  qh.net.all$qh.net.lcl <- NA
  qh.net.all$qh.net.ucl <- NA

  cat("done\n")

  ###########################################################
  # Run model for pairs of donor & recipient vegetation.
  #
  # Donors are outer loop, recipients are middle loop, and
  # donations (transactions) are inner loop.
  # In inner loop, pass 1 QH per iteration from donor to
  # recipient, until qh.lcl.recipient>=0 or qh.lcl.donor<0.
  # If qh.lcl.recipient>=0, exit inner loop and continue
  # with next recipient. If qh.lcl.donor<0, return 1 QH
  # from recipient back to donor, and exit middle loop and
  # continue with next donor. Exit all loops when all
  # recipients have been processed or when no more donors
  # are available.
  ###########################################################


  cat("- Running trade up model...")

  cat("WARNING: Not ready...under construction!\n")


  ##############################
  # Final cosmetic fixes
  ##############################

  cat("- Composing final data frame...")
  # Remove unwanted columns
  drop.cols <- c("assess1", "baseline", "current")
  qh.net.all <- qh.net.all[, !(names(qh.net.all) %in% drop.cols)]

  cat("- done\n")
} # END ALLOW.TRADE.UP

########################
########################
# Save all results
########################
########################

cat("Saving final results to file:\n")

qh.net.file.basename <- paste0("qh.net_", assess.current) # Save for naming Excel file
filename <- paste0(qh.net.file.basename, ".csv")
fileandpath <- paste0(qh.net.resultsdir, filename)
qh.net.main.csv <- fileandpath # Save for later for converting to Excel
cat(paste0("- Saving qh.net.main (1:1 compensation) to file \"", filename, "\"\n"))
suppressMessages(write_excel_csv(qh.net.main, file = fileandpath))

qh.net.boot.main.file.basename <- paste0("qh.net_", assess.current, "_boot") # Save for naming Excel file
filename <- paste0(qh.net.boot.main.file.basename, ".csv")
fileandpath <- paste0(qh.net.resultsdir, filename)
qh.net.main.csv <- fileandpath # Save for later for converting to Excel
cat(paste0("- Saving qh.net.boot.main to file \"", filename, "\"\n"))
suppressMessages(write_excel_csv(qh.net.boot.main, file = fileandpath))

if (INCLUDE.OFFSET) {
  qh.net.off.file.basename <- paste0("qh.net_", assess.current, "_with_offsets") # Save for naming Excel file
  filename <- paste0(qh.net.off.file.basename, ".csv")
  fileandpath <- paste0(qh.net.resultsdir, filename)
  qh.net.off.csv <- fileandpath # Save for later for converting to Excel
  cat(paste0("- Saving qh.net.off (current + offsets) to file \"", filename, "\"\n"))
  suppressMessages(write_excel_csv(qh.net.off, file = fileandpath))
}

if (PREPARE.QH.NET.P) {
  # Save potential QH
  qh.net.p.file.basename <- paste0("qh.net.p_", assess.current)
  filename <- paste0(qh.net.p.file.basename, ".csv")
  fileandpath <- paste0(qh.net.resultsdir, filename)
  cat(paste0("- Saving qh.net.p (potential additional QHa) to file \"", filename, "\"\n"))
  suppressMessages(write_excel_csv(qh.net.p, file = fileandpath))
}

if (ALLOW.TRADE.UP) {
  # Save the trade-up analysis as well
  qh.net.tu.file.basename <- paste0("qh.net_", assess.current, "_tradeup")
  filename <- paste0(qh.net.tu.file.basename, ".csv")
  fileandpath <- paste0(qh.net.resultsdir, filename)
  cat(paste0("- Saving qh.net.all (trade-ups allowed) to file \"", filename, "\"\n"))
  suppressMessages(write_excel_csv(qh.net.all, file = fileandpath))
}

#######################################
# Save results as formatted Excel file
#######################################

source("qh.net/qh.net.xl.R")

cat("\n")
cat("Operation completed", "\n")
cat("##########################################\n")

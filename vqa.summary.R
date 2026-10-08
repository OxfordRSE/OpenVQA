##########################################################
# VQA Overall Quality Calculation
#
# Purpose:
# 	Combine individual indicator quality results and calculate functional group
# 	and overall quality using 3-category geometric mean algorithm
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
###########################################################

cat("******************************************\n")
cat("Summarizing VQA results\n\n")

##########################################################
# Set working directory and load global parameters and
# functions if not already loaded
##########################################################

if (!exists("params.loaded", mode = "function")) {
  # Set working directory (base application directory)
  # Must set first to set remaining directory parameters
  wd <- getwd()
  setwd(wd)
  SRCDIR <- paste0(wd, "/")
  source(paste0(SRCDIR, "params.R"))
  source("includes/functions.R", local = TRUE)
}

###########################
# Input: Quality results for each indicator
# * In results folder
# * EI prefix added programmatically
###########################

results.bm.file.suffix <- "_benchmark.csv"
results.focal.file.suffix <- "_focal.csv"
boot.q.file.suffix <- "_boot.q.csv" # Indicator bootstrap quality file

##########################################################
# Import land cover metadata
##########################################################

cat("Importing metadata files from input directory '", INPUTDIR, "':\r\n")

# Import site-level metadata
# Used to associate focal with benchmark vegetation
cat(paste0("  ", METADATA.FILE, "\r\n"))
input.file <- paste(INPUTDIR, METADATA.FILE, sep = "")
rawData <- read.csv(input.file, header = T)
landcover <- unique(rawData[, c("landCover", "vegClass", "area_ha")])
colnames(landcover) <- c("landcover", "bm.veg", "area_ha")
landcover$landcover <- as.character(landcover$landcover)
landcover$bm.veg <- as.character(landcover$bm.veg)

##########################################################
# Import whitelist, blacklist and functional groups
##########################################################

# Blacklist: EI+stratum combinations to exclude
# All combinations are included; categories to exclude flagged include=FALSE
cat(paste0("  ", BLACKLIST.FILE, "\r\n"))
input.file <- paste(INPUTDIR, BLACKLIST.FILE, sep = "")
dist.eval <- read.csv(input.file, header = T)
dist.eval <- dist.eval[, c("EI", "stratum", "f.group", "include")]

# Whitelist: Exceptions to blacklist, for individual (benchmark) vegetation classes
cat(paste0("  ", WHITELIST.FILE, "\r\n"))
input.file <- paste(INPUTDIR, WHITELIST.FILE, sep = "")
dist.eval.exceptions <- read.csv(input.file, header = T)
dist.eval.exceptions <- dist.eval.exceptions[!is.na(dist.eval.exceptions$include), ]
names(dist.eval.exceptions)[names(dist.eval.exceptions) == "bm.vegetation"] <- "bm.veg"
dist.eval.exceptions <- dist.eval.exceptions[, c("EI", "stratum", "bm.veg", "include")]

# Make df of f.group classifications only
f.groups <- unique(dist.eval[, c("EI", "stratum", "f.group")])

##########################################################
# Merge benchmark indicator results into single data frame
##########################################################

cat("Importing & merging indicator benchmark value files:\r\n")

n.EI <- nrow(EI.list)

for (i in 1:n.EI) {
  curr.EI <- EI.list[i, 1]
  results.bm.file <- paste(curr.EI, results.bm.file.suffix, sep = "")
  cat(paste0("  ", results.bm.file, "\r\n"))
  input.file <- paste(RESULTSDIR, results.bm.file, sep = "")
  results.bm.EI <- read.csv(input.file, header = T)

  if (i == 1) {
    # Make new data frame
    results.bm <- results.bm.EI
  } else {
    # append to existing data frame
    results.bm <- rbind(results.bm, results.bm.EI)
  }
}
names(results.bm)[names(results.bm) == "vegClass"] <- "bm.veg"

# Mark included/excluded EI-landcover-stratum combinations
results.bm <- merge(x = results.bm, y = dist.eval, by = c("EI", "stratum"), all.x = TRUE)
results.bm <- merge(x = results.bm, y = dist.eval.exceptions, by = c("EI", "stratum", "bm.veg"), all.x = TRUE)
results.bm <- transform(results.bm, include = ifelse(is.na(include.y),
  include.x, as.character(include.y)
))

# Drop the extra 'include' columns,  no longer needed
drop.cols <- c("include.x", "include.y")
results.bm <- results.bm[, !(names(results.bm) %in% drop.cols)]

# Reorder the columns one last time
results.bm <- df.reorder(results.bm, col.move = "bm.veg", move.first = TRUE)
results.bm <- df.reorder(results.bm, col.move = "f.group", col.before = "stratum")
results.bm <- df.reorder(results.bm, col.move = "include", col.before = "f.group")

# Save bm indicator summary to file
fileandpath <- paste(RESULTSDIR, SUMMARY.BM.EI.FILE, sep = "")
cat(paste0("  Saving file '", fileandpath, "'..."))
write.csv(results.bm, file = fileandpath, row.names = FALSE)
cat("done\r\n")

##########################################################
# Merge focal indicator results into single data frame
##########################################################

cat("Preparing indicator quality summary:\r\n")

n.EI <- nrow(EI.list)

cat("  Importing & merging indicator quality files to df 'results.focal':\n")
for (i in 1:n.EI) {
  curr.EI <- EI.list[i, 1]
  results.focal.file <- paste(curr.EI, results.focal.file.suffix, sep = "")
  cat(paste0("  - ", results.focal.file, "\r\n"))
  input.file <- paste(RESULTSDIR, results.focal.file, sep = "")
  results.focal.EI <- read.csv(input.file, header = T)

  if (i == 1) {
    # Make new data frame
    results.focal <- results.focal.EI
  } else {
    # Remove non-matching column if exists (in PCESS only)
    if ("MDES" %in% colnames(results.focal.EI)) {
      results.focal.EI <-
        subset(results.focal.EI, select = -c(MDES))
    }

    # append to existing data frame
    results.focal <- rbind(results.focal, results.focal.EI)
  }
}

cat("  Standardizing 'results.focal'...")

# Standardize reclaimed landcover, if any, to match landcover file
results.focal$landcover <- as.character(results.focal$landcover)
for (i in 1:nrow(results.focal)) {
  results.focal$landcover[i] <- reclaimed.make.pretty(results.focal$landcover[i])
}
names(results.focal)[names(results.focal) == "bm.vegetation"] <- "bm.veg"

# Remove excluded EI-landcover-stratum combinations
results.focal <- merge(x = results.focal, y = dist.eval, by = c("EI", "stratum"), all.x = TRUE)
names(dist.eval.exceptions)[names(dist.eval.exceptions) == "include"] <- "include.exceptions"
results.focal <- merge(x = results.focal, y = dist.eval.exceptions, by = c("EI", "stratum", "bm.veg"), all.x = TRUE)
results.focal$include[!is.na(results.focal$include.exceptions)] <-
  results.focal$include.exceptions[!is.na(results.focal$include.exceptions)]
results.focal <- results.focal[results.focal$include == TRUE, ]
# Drop unneeded column
drop.cols <- c("include.exceptions")
results.focal <- results.focal[, !(names(results.focal) %in% drop.cols)]

# Reorder the columns one last time
results.focal <- df.reorder(results.focal, col.move = "landcover", move.first = TRUE)
results.focal <- df.reorder(results.focal, col.move = "bm.veg", col.before = "landcover")
results.focal <- df.reorder(results.focal, col.move = "f.group", col.before = "stratum")
results.focal <- df.reorder(results.focal, col.move = "include", col.before = "f.group")
results.focal <- df.reorder(results.focal, col.move = "q.diff.method", col.before = "q.diff")
results.focal <- df.reorder(results.focal, col.move = "q.wt.method", col.before = "ol.w")

# Add compound grouping columns
results.focal$lc.ig <- paste0(results.focal$landcover, "|", results.focal$EI)
results.focal$lc.fg <- paste0(results.focal$landcover, "|", results.focal$f.group)
results.focal$bm.fg <- paste0(results.focal$bm.veg, "|", results.focal$f.group)
results.focal <- df.reorder(results.focal, col.move = "lc.ig", col.before = "EI")
results.focal <- df.reorder(results.focal, col.move = "lc.fg", col.before = "f.group")
results.focal <- df.reorder(results.focal, col.move = "bm.fg", col.before = "lc.fg")
cat("done\r\n")

cat("  Inferring missing confidence limits...")

# Add 0/1 column to track inferred CLs
results.focal$cls.inferred <- 0

# Fill in missing CLs for indicators with no variance in either indicator
# Typically, all focal and bm values are zero (e.g., PCESS)
# Set both CLs == q
results.focal[is.na(results.focal$q.lcl) & is.na(results.focal$q.ucl) &
  results.focal$f.sd == 0 & results.focal$b.sd == 0, c("cls.inferred")] <- 1
results.focal[is.na(results.focal$q.lcl) & is.na(results.focal$q.ucl) &
  results.focal$f.sd == 0 & results.focal$b.sd == 0, c("q.lcl", "q.ucl")] <-
  results.focal[is.na(results.focal$q.lcl) & is.na(results.focal$q.ucl) &
    results.focal$f.sd == 0 & results.focal$b.sd == 0, c("q", "q")]

if (infer.missing.cls.strict == TRUE) {
  # Workaround until figure out how stop calculation from failing for
  # indicators with CIs close but not equal to zero
  # infer.missing.cls <- FALSE
  results.focal[is.na(results.focal$q.lcl) | is.na(results.focal$q.ucl), c("cls.inferred")] <- 1
  results.focal[is.na(results.focal$q.lcl) | is.na(results.focal$q.ucl), c("q.lcl", "q.ucl")] <-
    results.focal[is.na(results.focal$q.lcl) | is.na(results.focal$q.ucl), c("q", "q")]
}

if (infer.missing.cls == TRUE) {
  # Remainder, if beta-distributed
  # All have low variance and both means close to zero
  for (i in 1:nrow(results.focal)) {
    # Apply to rows with missing CLs where distribution=beta
    if (is.na(results.focal$q.lcl[i]) && is.na(results.focal$q.ucl[i]) && results.focal$dist[i] == "Bet") {
      f.sd <- results.focal$f.sd[i]
      b.sd <- results.focal$b.sd[i]
      q <- results.focal$q[i]
      q.half.ci <- f.sd + b.sd
      results.focal$q.lcl[i] <- results.focal$q[i] - q.half.ci
      results.focal$q.ucl[i] <- results.focal$q[i] + q.half.ci
      results.focal$cls.inferred[i] <- 1
    }
  }
}
cat("done\r\n")

cat("  Saving indicator quality summary to file '", SUMMARY.FOCAL.EI.FILE, "'...")
drop.cols <- c("cls.inferred")
results.focal.saved <- results.focal[, !(names(results.focal) %in% drop.cols)]
fileandpath <- paste(RESULTSDIR, SUMMARY.FOCAL.EI.FILE, sep = "")
write.csv(results.focal.saved, file = fileandpath, row.names = FALSE)
cat("done\r\n")

#################################################
# Merge bootstrapped indicator quality results into single data frame
#
# IMPORTANT: Note that column "q" in the bootstrap files is
# *actual* quality, NOT the mean of the bootstrap qualities!!!
#################################################

cat("Importing & merging bootstrapped indicator quality files:\r\n")

n.EI <- nrow(EI.list)

for (i in 1:n.EI) {
  curr.EI <- EI.list[i, 1]
  boot.q.file <- paste(curr.EI, boot.q.file.suffix, sep = "")
  cat(paste0("  ", boot.q.file, "\r\n"))
  input.file <- paste(RESULTSDIR, boot.q.file, sep = "")
  boot.q.curr.EI <- read.csv(input.file, header = T)
  boot.q.curr.EI <- as.data.frame(boot.q.curr.EI)
  if (nrow(boot.q.curr.EI) > 0) {
    boot.q.curr.EI$EI <- curr.EI
  }

  if (i == 1) {
    # Make new data frame
    boot.q.ei <- boot.q.curr.EI
  } else {
    # append to existing data frame
    boot.q.ei <- rbind(boot.q.ei, boot.q.curr.EI)
  }
}

# Standardize case of 'landcover'
names(boot.q.ei)[names(boot.q.ei) == "landCover"] <- "landcover"

# Standardize reclaimed vegetation codes (if any) to match landcover
boot.q.ei$landcover <- as.character(boot.q.ei$landcover)
for (i in 1:nrow(boot.q.ei)) {
  boot.q.ei$landcover[i] <- reclaimed.make.pretty(boot.q.ei$landcover[i])
}

# Add bm vegetation column
bm.veg.temp <- unique(results.focal[, c("landcover", "bm.veg")]) # Add bm vegetation
boot.q.ei <- merge(boot.q.ei, bm.veg.temp, by = c("landcover"))

# Add include column
df.include <- unique(results.focal[, c("landcover", "bm.veg", "EI", "stratum", "include")])
boot.q.ei <- merge(boot.q.ei, df.include, by = c("landcover", "bm.veg", "EI", "stratum"))

# Add f.group column
boot.q.ei <- merge(boot.q.ei, f.groups, by = c("EI", "stratum"))

# Reorder columns
boot.q.ei <- df.reorder(boot.q.ei, col.move = "landcover", move.first = TRUE)
boot.q.ei <- df.reorder(boot.q.ei, col.move = "bm.veg", col.before = "landcover")
boot.q.ei <- df.reorder(boot.q.ei, col.move = "EI", col.before = "bm.veg")
boot.q.ei <- df.reorder(boot.q.ei, col.move = "stratum", col.before = "EI")
boot.q.ei <- df.reorder(boot.q.ei, col.move = "f.group", col.before = "stratum")

# Drop excluded indicators
boot.q.ei <- boot.q.ei[boot.q.ei$include == TRUE, ]
# Drop 'include' column, no longer needed
drop.cols <- c("include")
boot.q.ei <- boot.q.ei[, !(names(boot.q.ei) %in% drop.cols)]

# Create land cover + function group classes
boot.q.ei$lc.fg <- paste(boot.q.ei$landcover, boot.q.ei$f.group, sep = "|")
boot.q.ei <- df.reorder(boot.q.ei, col.move = "lc.fg", col.before = "f.group")

# Rename quality column to make clear this is *indicator* quality
names(boot.q.ei)[names(boot.q.ei) == "q"] <- "q.ei"

# Now add land cover - indicator group grouping column
boot.q.ei$lc.ig <- paste(boot.q.ei$landcover, boot.q.ei$EI, sep = "|")
boot.q.ei <- df.reorder(boot.q.ei, col.move = "lc.ig", col.before = "EI")

# Save bootstrapped focal indicator summary to file
fileandpath <- paste(RESULTSDIR, SUMMARY.FOCAL.EI.BOOT.FILE, sep = "")
cat(paste0("  Saving bootstrap file '", fileandpath, "'..."))
write.csv(boot.q.ei, file = fileandpath, row.names = FALSE)
cat("done\r\n")

###############################
###############################
# Quality by land cover class
###############################
###############################

##########################################################
# Functional group quality & confidence limits by land cover class
##########################################################

cat("Summarizing quality by functional groups:\r\n")

###############################
# Indicator group quality
# Arithmetic mean of *actual* indicator quality within
# each indicator group + landcover class
# Intermediate step to avoid over-weighting of indicator groups with many strata indicators
################################
cat("  Indicator group quality...")
if (Q.FG.METHOD == "arithmetic") {
  mean_func <- "mean" # Arithmetic mean function
} else if (Q.FG.METHOD == "generalized_mean") {
  mean_func <- "generalized_mean" # Generalized mean function
} else {
  stop_quietly("ERROR: '", Q.FG.METHOD, "': unknown value for Q.FG.METHOD!\n") # [gmean change]
}

q.ig <- aggregate(
  results.focal$q[!results.focal$q == Inf],
  by = list(results.focal$lc.ig[!results.focal$q == Inf]),
  FUN = c(mean_func) # ,  # [gmean change]
  # FUN=c('gmean'),  # [gmean change]
  # na.rm=TRUE # [gmean change]
)
# Fix column names
colnames(q.ig) <- c("lc.ig", "q.ig")

# Restore separate land cover & functional group columns
lc.ig <- unique(results.focal[, c("landcover", "EI", "lc.ig", "f.group", "lc.fg")])
q.ig <- merge(q.ig, lc.ig, by = c("lc.ig"))
q.ig <- df.reorder(q.ig, col.move = "landcover", move.first = TRUE)
q.ig <- df.reorder(q.ig, col.move = "EI", col.before = "landcover")
q.ig <- df.reorder(q.ig, col.move = "q.ig", move.last = TRUE)
cat("done\r\n")

###############################
# Functional group quality
# Arithmetic mean of *actual* indicator quality within
# each functional group + landcover class
################################
cat("  Functional group quality...")

if (Q.FG.GROUP.BY == "indicator") {
  # Use raw indicator quality within each landcover + f.group,
  # keeping stratum indicators separate
  q.fg <- aggregate(
    results.focal$q[!results.focal$q == Inf],
    by = list(results.focal$lc.fg[!results.focal$q == Inf]),
    FUN = c(mean_func), # [gmean change]
    # FUN=c('gmean'),  # [gmean change]
    na.rm = TRUE
  )
} else if (Q.FG.GROUP.BY == "indicator_group") {
  # Use aggregated quality within indicator groups (=EI)
  # for each landcover + f.group
  q.fg <- aggregate(
    q.ig$q.ig[!q.ig$q.ig == Inf],
    by = list(q.ig$lc.fg[!q.ig$q.ig == Inf]),
    FUN = c(mean_func) # ,  # [gmean change]
    # FUN=c('gmean'),  # [gmean change]
    # na.rm=TRUE   # [gmean change]
  )
} else {
  stop_quietly("ERROR: '", Q.FG.GROUP.BY, "' - unknown value of parameter Q.FG.GROUP.BY!")
}

# Fix column names & restore separate land cover & functional group columns
colnames(q.fg) <- c("lc.fg", "q.fg")
lc.fg <- unique(results.focal[, c("landcover", "f.group", "lc.fg")])
q.fg <- merge(q.fg, lc.fg, by = c("lc.fg"))
q.fg <- df.reorder(q.fg, col.move = "landcover", move.first = TRUE)
q.fg <- df.reorder(q.fg, col.move = "f.group", col.before = "landcover")
cat("done\r\n")

#####################################
# CLs of functional group quality
# As calculated from bootstrap CLs
# For each bootstrap run, calculate mean of all bootstrap qualitiess
# across all EIs, then subtract grand mean across all runs from run
# mean to calculate bootrapped deviances of the run means. The
# upper and lower 95% CLs of the bootstrapped deviances are then
# subtracted from the observed quality (q.adj) to provide the final
# bootstrap CLs
#####################################
cat("  Bootstrap CLs...")

# Create new aggregated indicator group bootstrap quality df if aggregating by EIs
if (Q.FG.GROUP.BY == "indicator_group") { # START Q.FG.GROUP.BY=="indicator_group"
  # Vector of unique land cover + indicator group classes

  all.lc.ig <- unique(boot.q.ei$lc.ig)
  boot.q.ig <- boot.q.ei[0, ]
  drop_cols <- c("stratum", "q.ei")
  boot.q.ig <- boot.q.ig[, !names(boot.q.ig) %in% drop_cols]
  boot.q.ig.meta <- unique(boot.q.ei[, c("landcover", "bm.veg", "EI", "lc.ig", "f.group", "lc.fg", "boot.reps")])
  boot.q.ig.data <- boot.q.ig[, !names(boot.q.ig) %in% colnames(boot.q.ig.meta)]
  i <- 1

  for (i in 1:length(all.lc.ig)) {
    curr.lc.ig <- all.lc.ig[i]

    # Get bootstrap results for all EIs for current land cover class
    curr.boot.q <- boot.q.ei[boot.q.ei$lc.ig == curr.lc.ig, ]

    # Initialize vector to hold the means of each bootstrap run
    q.vec <- rep(NA, boot.reps)

    # For each bootstrap run, get mean quality for all EIs in current indicator group
    # and save to vector, omitting NAs and Infinity
    if (Q.FG.METHOD == "arithmetic") { # [gmean change]
      for (n in 1:boot.reps) {
        rep.col <- paste0("X", n)
        rep.curr.boot.q <- curr.boot.q[[rep.col]][!(curr.boot.q[[rep.col]] == Inf) &
          !(is.na(curr.boot.q[[rep.col]]))]
        q.vec[n] <- mean(rep.curr.boot.q, na.rm = T) # Avg quality for the current run
      }
    } else if (Q.FG.METHOD == "generalized_mean") { # [gmean change]
      for (n in 1:boot.reps) { # [gmean change]
        rep.col <- paste0("X", n) # [gmean change]
        rep.curr.boot.q <- curr.boot.q[[rep.col]][!(curr.boot.q[[rep.col]] == Inf) # [gmean change]
        & !(is.na(curr.boot.q[[rep.col]]))] # [gmean change]
        q.vec[n] <- generalized_mean(rep.curr.boot.q) # Generalized mean [gmean change]
      } # [gmean change]
    } else { # [gmean change]
      stop_quietly("ERROR: '", Q.FG.METHOD, "': unknown value for Q.FG.METHOD!\n") # [gmean change]
    } # [gmean change]

    q.vec <- c(curr.lc.ig, q.vec)

    if (i == 1) {
      q.vec.all <- q.vec
    } else {
      q.vec.all <- rbind(q.vec.all, q.vec)
    }
  }

  vec.colnames <- c("lc.ig", colnames(boot.q.ig.data))
  colnames(q.vec.all) <- vec.colnames
  q.vec.all <- as.data.frame(q.vec.all)
  q.vec.all[-1] <- lapply(q.vec.all[-1], as.numeric)
  boot.q.ig <- merge(boot.q.ig.meta, q.vec.all, by = "lc.ig", all.x = TRUE)

  # Set the final bootstrap quality data frame to this one
  boot.q <- boot.q.ig
  boot.q <- df.reorder(boot.q, col.move = "lc.ig", col.before = "EI")
} else {
  boot.q <- boot.q.ei
} # END Q.FG.GROUP.BY=="indicator_group"

# Add CL columns to q.fg
q.fg$q.lcl <- NA
q.fg$q.ucl <- NA

# Vector of unique land cover + f.group classes
all.lc.fg <- unique(boot.q.ei$lc.fg)

# Loop through land cover + f.group classes
for (curr.lc.fg in all.lc.fg) {
  # Get bootstrap results for all EIs for current land cover class
  # curr.boot.q <- boot.q.ei[boot.q.ei $lc.fg == curr.lc.fg, ]
  curr.boot.q <- boot.q[boot.q$lc.fg == curr.lc.fg, ]

  # Initialize vector to hold the means of each bootstrap run
  q.vec <- rep(NA, boot.reps)

  # For each bootstrap run, get mean quality for all EIs in current functional group
  # and save to vector, omitting NAs and Infinity
  if (Q.FG.METHOD == "arithmetic") {
    for (n in 1:boot.reps) {
      rep.col <- paste0("X", n)
      rep.curr.boot.q <- curr.boot.q[[rep.col]][!(curr.boot.q[[rep.col]] == Inf) &
        !(is.na(curr.boot.q[[rep.col]]))]
      q.vec[n] <- mean(rep.curr.boot.q, na.rm = T) # Avg quality for the current run [gmean change]
    }
  } else if (Q.FG.METHOD == "generalized_mean") { # [gmean change]
    for (n in 1:boot.reps) {
      rep.col <- paste0("X", n)
      rep.curr.boot.q <- curr.boot.q[[rep.col]][!(curr.boot.q[[rep.col]] == Inf) &
        !(is.na(curr.boot.q[[rep.col]]))]
      q.vec[n] <- generalized_mean(rep.curr.boot.q) # Generalized mean [gmean change]
    }
  } else { # [gmean change]
    stop_quietly("ERROR: '", Q.FG.METHOD, "': unknown value for Q.FG.METHOD!\n") # [gmean change]
  } # [gmean change]

  boot.mean <- mean(q.vec, na.rm = T) # grand mean quality across all boot runs
  boot.devs <- boot.mean - q.vec # run deviances from the grand mean

  # Get 95% CLs of the deviances
  boot.quant <- quantile(boot.devs, c(0.025, 0.975), na.rm = T)
  lower <- boot.quant[[1]]
  upper <- boot.quant[[2]]

  # Subtract bootstrap deviances from the adjusted observed quality
  q.fg$q.lcl[q.fg$lc.fg == curr.lc.fg] <- q.fg$q.fg[q.fg$lc.fg == curr.lc.fg] + lower
  q.fg$q.ucl[q.fg$lc.fg == curr.lc.fg] <- q.fg$q.fg[q.fg$lc.fg == curr.lc.fg] + upper
}
cat("done\r\n")

# Backup func. group results as landcover df
q.fg.lc <- q.fg
names(q.fg.lc)[names(q.fg.lc) == "q.fg"] <- "q"

# Check for missing cls
q.fg$cls.inferred <- 0
cls.missing <- any(is.na(q.fg$q.lcl) | is.na(q.fg$q.ucl)) # Note single "|"

if (infer.missing.cls == TRUE && cls.missing == TRUE) {
  cl.missing <- q.fg[is.na(q.fg$q.lcl), c("lc.fg")]
  results.focal.missing <- results.focal[
    results.focal$lc.fg %in% cl.missing,
    c("landcover", "EI", "stratum", "f.group", "lc.fg", "q", "q.lcl", "q.ucl")
  ]
  q.lcl.fg.missing <- aggregate(
    q.lcl ~ lc.fg,
    data = results.focal.missing,
    FUN = c("mean"),
    na.rm = TRUE
  )
  colnames(q.lcl.fg.missing) <- c("lc.fg", "q.lcl.missing")
  q.ucl.fg.missing <- aggregate(
    q.ucl ~ lc.fg,
    data = results.focal.missing,
    FUN = c("mean"),
    na.rm = TRUE
  )
  colnames(q.ucl.fg.missing) <- c("lc.fg", "q.ucl.missing")

  q.fg <- merge(q.fg, q.lcl.fg.missing, by = "lc.fg", all.x = TRUE)
  q.fg <- merge(q.fg, q.ucl.fg.missing, by = "lc.fg", all.x = TRUE)
  q.fg[is.na(q.fg$q.lcl) & is.na(q.fg$q.ucl), c("q.lcl", "q.ucl")] <-
    q.fg[is.na(q.fg$q.lcl) & is.na(q.fg$q.ucl), c("q.lcl.missing", "q.ucl.missing")]
  q.fg$cls.inferred[!is.na(q.fg$q.lcl.missing) | !is.na(q.fg$q.ucl.missing)] <- 1
  q.fg <- q.fg[, !names(q.fg) %in% c("q.lcl.missing", "q.ucl.missing")]
}

# Drop composite lc.fg columns and save the functional group quality file
drop.cols <- c("lc.fg", "cls.inferred")
q.fg.saved <- q.fg[, !(names(q.fg) %in% drop.cols)]
fileandpath <- paste(RESULTSDIR, SUMMARY.FOCAL.FG.FILE, sep = "")
cat(paste0("  Saving file '", fileandpath, "'..."))
write.csv(q.fg.saved, file = fileandpath, row.names = FALSE)
cat("done\r\n")

##########################################################
# Overall quality & confidence limits by land cover class
##########################################################

cat("Summarizing quality by land cover class:\r\n")

######################################
# Overall quality
#
# Mean of functional group quality
# for each land cover class
######################################
cat("  Geometric mean of functional group quality...")
if (Q.OVERALL.METHOD == "gmean") {
  # Geometric mean
  q.overall <- aggregate(
    q.fg$q.fg,
    by = list(q.fg$landcover),
    FUN = c("gmean"),
    na.rm = TRUE
  )
} else if (Q.OVERALL.METHOD == "prod") {
  # Product
  q.overall <- aggregate(
    q.fg$q.fg,
    by = list(q.fg$landcover),
    FUN = c("prod"),
    na.rm = TRUE
  )
} else if (Q.OVERALL.METHOD == "mixed") {
  # Product of (a) geometric mean of all non-integrity FGs,
  # multiplied by (b) product of all integrity indicators
  q.overall.ni <- aggregate(
    q.fg ~ landcover,
    data = q.fg[!q.fg$f.group == "integrity", ],
    FUN = c("gmean"),
    na.rm = TRUE
  )
  names(q.overall.ni)[names(q.overall.ni) == "q.fg"] <- "q.fg.ni"
  q.overall.i <- aggregate(
    q.fg ~ landcover,
    data = q.fg[q.fg$f.group == "integrity", ],
    FUN = c("gmean"),
    na.rm = TRUE
  )
  names(q.overall.i)[names(q.overall.i) == "q.fg"] <- "q.fg.i"
  q.overall <- merge(q.overall.ni, q.overall.i, by = "landcover")
  q.overall$q.fg <- q.overall.ni$q.fg.ni * q.overall.i$q.fg.i
  q.overall <- q.overall[, !names(q.overall) %in% c("q.fg.ni", "q.fg.i")]
} else {
  msg.err <- "ERROR: unknown value of Q.OVERALL.METHOD!"
  stop_quietly(msg.err)
}
colnames(q.overall) <- c("landcover", "q")
cat("done\r\n")

############################################
# Bootstrapped overall quality scores
#
# Aggregated from bootstrapped functional
# group quality scores, using means
# function set in general parameters
############################################

cat("  Geometric means of bootstrapped functional group qualities...")

# Vector of bootstrap quality column names
prefix <- "X"
suffix <- seq(1:boot.reps)
cols <- paste0(prefix, suffix)

# Calculate mean bootstrap indicator quality grouped by f.groups
# WARNING: May need to subset to remove values of Inf or NA
boot.q.fg <- aggregate(
  boot.q.ei[, c(cols)],
  by = list(boot.q.ei$lc.fg),
  FUN = c(mean_func) # ,  # [gmean change]
  # FUN=c('gmean'),   # [gmean change]
  # na.rm=TRUE  # [gmean change]
)
# Restore intelligible column names
names(boot.q.fg)[names(boot.q.fg) == "Group.1"] <- "lc.fg"

# Add separate land cover column
# lc.fg2 <- unique(boot.q.ei[, c("landcover", "lc.fg")])
boot.q.fg <- merge(lc.fg, boot.q.fg, by = c("lc.fg"), all.x = TRUE)
boot.q.fg <- df.reorder(boot.q.fg, col.move = "landcover", move.first = TRUE)
boot.q.fg <- df.reorder(boot.q.fg, col.move = "f.group", col.before = "landcover")

if (infer.missing.cls == TRUE) {
  lc.fg.n <- aggregate(
    f.n ~ lc.fg,
    data = results.focal,
    FUN = c("mean"),
    na.rm = TRUE
  )
  colnames(lc.fg.n) <- c("lc.fg", "q.fg.n")
  q.fg2 <- merge(q.fg, lc.fg.n, by = "lc.fg", all.x = TRUE)
  q.fg2$q.fg.sd <- (q.fg2$q.ucl - q.fg2$q.lcl) * sqrt(q.fg2$q.fg.n) / 1.96
  q.fg2 <- q.fg2[, c("lc.fg", "q.fg", "q.fg.n", "q.fg.sd")]
  boot.q.fg <- merge(boot.q.fg, q.fg2, by = "lc.fg", all.x = TRUE)
  boot.q.fg$q.boot <- NA
  boot.q.fg$q.boot.max <- 1
  for (n in 1:boot.reps) {
    rep.col <- paste0("X", n)
    boot.q.fg$q.boot <-
      rnorm(boot.q.fg$q.fg.n, boot.q.fg$q.fg, boot.q.fg$q.fg.sd)
    boot.q.fg$q.boot <- pmin(boot.q.fg$q.boot, boot.q.fg$q.boot.max)
    boot.q.fg[[rep.col]][is.na(boot.q.fg[[rep.col]])] <-
      boot.q.fg$q.boot[is.na(boot.q.fg[[rep.col]])]
  }
  drop.cols <- c("q.fg", "q.fg.n", "q.fg.sd", "q.boot", "q.boot.max")
  boot.q.fg <- boot.q.fg[, !names(boot.q.fg) %in% drop.cols]
}

# Bootstrap overall quality
# Geometric mean of functional group quality per land cover class
# Calculated for each bootstrap run
if (Q.OVERALL.METHOD == "gmean") {
  boot.q.overall <- aggregate(
    boot.q.fg[, c(cols)],
    by = list(boot.q.fg$landcover),
    FUN = c("gmean"),
    na.rm = TRUE
  )
} else if (Q.OVERALL.METHOD == "prod") {
  boot.q.overall <- aggregate(
    boot.q.fg[, c(cols)],
    by = list(boot.q.fg$landcover),
    FUN = c("prod"),
    na.rm = TRUE
  )
} else if (Q.OVERALL.METHOD == "mixed") {
  boot.q.fg.ni <- boot.q.fg[!boot.q.fg$f.group == "integrity", ]
  boot.q.overall.ni <- aggregate(
    boot.q.fg.ni[, c(cols)],
    by = list(boot.q.fg.ni$landcover),
    FUN = c("gmean"),
    na.rm = TRUE
  )
  names(boot.q.overall.ni)[names(boot.q.overall.ni) == "q.fg"] <- "q.fg.ni"
  names(boot.q.overall.ni)[names(boot.q.overall.ni) == "Group.1"] <- "landcover"
  boot.q.overall.ni <- boot.q.overall.ni[order(boot.q.overall.ni$landcover), ]

  boot.q.fg.i <- boot.q.fg[boot.q.fg$f.group == "integrity", ]
  boot.q.overall.i <- aggregate(
    boot.q.fg.i[, c(cols)],
    by = list(boot.q.fg.i$landcover),
    FUN = c("gmean"),
    na.rm = TRUE
  )
  names(boot.q.overall.i)[names(boot.q.overall.i) == "q.fg"] <- "q.fg.i"
  names(boot.q.overall.i)[names(boot.q.overall.i) == "Group.1"] <- "landcover"
  boot.q.overall.i <- boot.q.overall.i[order(boot.q.overall.i$landcover), ]

  # Extract just the numeric columns and get their products
  is_x_col <- grepl("^X[0-9]+", names(boot.q.overall.ni)) # Same for both dfs

  # Extract and multiply the numeric parts as matrices
  numeric_product <- as.matrix(boot.q.overall.ni[, is_x_col]) * as.matrix(boot.q.overall.i[, is_x_col])

  # Combine landcover with the new numeric products and convert back to data frame
  lc_col <- boot.q.overall.ni[, c("landcover"), drop = FALSE]
  boot.q.overall <- data.frame(cbind(lc_col, numeric_product))
}
names(boot.q.overall)[names(boot.q.overall) == "Group.1"] <- "landcover"
cat("done\r\n")

# Save bootstrapped "overall quality by benchmark" to file
fileandpath <- paste(RESULTSDIR, SUMMARY.Q.OVERALL.BOOT.LC.FILE, sep = "")
cat(paste0("  Saving bootstrap file '", fileandpath, "'..."))
write.csv(boot.q.overall, file = fileandpath, row.names = FALSE)
cat("done\r\n")

#####################################################################
# Bootstrapped CLs of overall quality
#
# For each bootstrap run, calculate mean of all bootstrap qualities,
# then subtract grand mean across all runs to calculate bootrap
# deviances of the run means. Upper and lower 95% CLs of the
# bootstrap deviances are then subtracted from the observed quality
# to provide the final empirical bootstrap CLs
#####################################################################

cat("  Calculating bootstrap CLs...")

# Create the CL columns
q.overall$q.lcl <- NA
q.overall$q.ucl <- NA

# Vector of land cover classes
all.lc <- unique(boot.q.ei$landcover)

for (curr.lc in all.lc) {
  # Get bootstrap results for all EIs for current land cover class
  curr.boot.q <- boot.q.overall[boot.q.overall$landcover == curr.lc, ]

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

  # Get 95% CLs of the deviances
  boot.quant <- quantile(boot.devs, c(0.025, 0.975), na.rm = T)
  lower <- boot.quant[[1]]
  upper <- boot.quant[[2]]

  # Subtract bootstrap deviances from the observed quality
  q.overall$q.lcl[q.overall$landcover == curr.lc] <- q.overall$q[q.overall$landcover == curr.lc] + lower
  q.overall$q.ucl[q.overall$landcover == curr.lc] <- q.overall$q[q.overall$landcover == curr.lc] + upper
}

# Backup overall quality results as land cover df
q.overall.lc <- q.overall
cat("done\r\n")

##################################
##################################
# Land cover quality hectares
##################################
##################################

cat("Calculating quality hectares by land cover class:\r\n")
landcover$landcoverFinal <- landcover$landcover
landcover.bak <- landcover

##################################
# Anthropogenic land cover classes separate
##################################
cat("  Anthropogenic landcover classes separate...")

# Merge with overall-quality-by-landcover df
landcover <- merge(landcover, q.overall.lc, by.x = "landcover", by.y = "landcover", all.x = TRUE)

landcover[is.na(landcover$q), c("landcoverFinal")] <- "Anthropogenic/non-vegetated"
landcover[landcover$landcover %in% veg.nodata, c("landcoverFinal")] <-
  landcover[landcover$landcover %in% veg.nodata, c("landcover")]
colnames(landcover) <- c("landcover", "bm.veg", "area_ha", "landcoverFinal", "q", "q.lcl", "q.ucl")

# Add Quality Hectares
landcover$qh <- landcover$area_ha * landcover$q
landcover$qh.lcl <- landcover$area_ha * landcover$q.lcl
landcover$qh.ucl <- landcover$area_ha * landcover$q.ucl
cat("done\r\n")

# Save quality + QH results by land cover class
fileandpath <- paste(RESULTSDIR, SUMMARY.FOCAL.ALL.FILE, sep = "")
cat(paste0("  Saving file '", fileandpath, "'..."))
write.csv(landcover, file = fileandpath, row.names = FALSE)
cat("done\r\n")

##################################
# Anthropogenic landcover classes combined
##################################
cat("  Anthropogenic landcover classes combined...")

lc.area <- aggregate(
  landcover$area_ha,
  by = list(landcover$landcoverFinal),
  FUN = c("sum")
)
colnames(lc.area) <- c("landcoverFinal", "area_ha")
landcover.tmp <- unique(landcover[, c("landcoverFinal", "q", "q.ucl", "q.lcl")])
landcover.final.tmp <- merge(landcover.tmp, lc.area, by = c("landcoverFinal"), all.x = T)
landcover.final <- merge(
  landcover.final.tmp,
  landcover[
    !landcover$landcoverFinal == "Anthropogenic/non-vegetated",
    c("landcoverFinal", "bm.veg")
  ],
  by = c("landcoverFinal"),
  all.x = T
)
landcover.final <- landcover.final[
  ,
  c("landcoverFinal", "bm.veg", "q", "q.lcl", "q.ucl", "area_ha")
]
colnames(landcover.final) <- c("landcover", "bm.veg", "q", "q.lcl", "q.ucl", "area_ha")

# Add Quality Hectares
landcover.final$qh <- landcover.final$area_ha * landcover.final$q
landcover.final$qh.lcl <- landcover.final$area_ha * landcover.final$q.lcl
landcover.final$qh.ucl <- landcover.final$area_ha * landcover.final$q.ucl

landcover.final$notes <- ""
landcover.final[is.na(landcover.final$q) & !landcover.final$landcover == "Anthropogenic/non-vegetated", c("notes")] <- "Insufficient data"
cat("done\r\n")

# Focal results, final
fileandpath <- paste(RESULTSDIR, SUMMARY.FOCAL.FILE, sep = "")
cat(paste0("  Saving file '", fileandpath, "'..."))
write.csv(landcover.final, file = fileandpath, row.names = FALSE)
cat("done\r\n")

##########################################################
##########################################################
# Benchmark quality hectares
#
# Calculate bm QH as sum of land cover QH
##########################################################
##########################################################

cat("Calculating benchmark quality hectares:\n")

cat("  Aggregating landcover QH by benchmark vegetation...")
qh.lc <- landcover.final[, c("bm.veg", "landcover", "area_ha", "qh", "qh.lcl", "qh.ucl", "notes")]
qh.bm.area <- aggregate(
  area_ha ~ bm.veg,
  data = qh.lc,
  FUN = sum,
  na.rm = TRUE
)
qh.bm.qh <- aggregate(
  qh ~ bm.veg,
  data = qh.lc,
  FUN = sum,
  na.rm = TRUE
)
qh.bm <- merge(qh.bm.area, qh.bm.qh, by = "bm.veg", all.x = TRUE)

# Temporarily deactivated: This next section will need updating
# for applications with land cover for which quality and qh cannot
# be calculated (e.g., anthropogenic, non-vegetation natural and missing data)
# # Add missing vegetation from landcover.final, if any
# qh.lc.bm.unmatched <- qh.lc[ !qh.lc$bm.veg %in% unique(qh.bm$bm.veg), ]
#
# if ( nrow(qh.lc.bm.unmatched)>0 ) {
#   qh.bm.unmatched.area <- aggregate(
#     area_ha~bm.veg,
#     data=qh.lc.bm.unmatched,
#     FUN=c('sum')
#   )
#   qh.bm.unmatched.qh <- aggregate(
#     qh~bm.veg,
#     data=qh.lc.bm.unmatched,
#     FUN=c('sum')
#   )
#   qh.bm.unmatched <- merge( qh.bm.area, qh.bm.unmatched.qh, by="bm.veg", all.x=TRUE)
#   qh.bm.unmatched$q <- NA
#   qh.bm.unmatched$q.lcl <- NA
#   qh.bm.unmatched$q.ucl <- NA
#
#   # reorder the columns to match qh.bm, append and sort
#   qh.bm.unmatched <- qh.bm.unmatched[,c("bm.veg", "q", "q.lcl", "q.ucl", "area_ha", "qh", "qh.lcl", "qh.ucl")]
#   qh.bm <- rbind( qh.bm, qh.bm.unmatched )
#   qh.bm <- qh.bm[ order(qh.bm$bm.veg), ]
# }

cat("done\n")

##########################################################
# Bootstrapped benchmark QH
##########################################################

cat("  Aggregating bootstrapped landcover QH by benchmark vegetation...")
# Calculate bootstrapped landcover quality hectares
area.bm.lc <- qh.lc[, c("landcover", "bm.veg", "area_ha")]
boot.qh.lc <- merge(boot.q.overall, area.bm.lc, by = "landcover", all.x = TRUE)
boot.qh.lc <- boot.qh.lc[, c("bm.veg", "landcover", "area_ha", cols)]
boot.qh.lc[, c(cols)] <- boot.qh.lc[, c(cols)] * boot.qh.lc$area_ha

# Sum the bootstrapped landcover QHs with each bm veg class
boot.qh.bm <- aggregate(
  boot.qh.lc[, c(cols)],
  by = list(boot.qh.lc$bm.veg),
  FUN = c("sum"),
  na.rm = TRUE
)
names(boot.qh.bm)[names(boot.qh.bm) == "Group.1"] <- "bm.veg"
cat("done\n")

# Save bootstrap benchmark QH file
fileandpath <- paste(RESULTSDIR, SUMMARY.QH.BOOT.BM.FILE, sep = "")
cat(paste0("  Saving bootstrap benchmark QH file '", fileandpath, "'..."))
write.csv(boot.qh.bm, file = fileandpath, row.names = FALSE)
cat("\n")

##########################################################
# Confidence limits of benchmark quality hectares
##########################################################

cat("  Estimating bootstrapped CLs of benchmark quality hectares...")
# Calculate bootstrapped CLs of benchmark quality hectares
# Create the CL columns
qh.bm$qh.lcl <- NA
qh.bm$qh.ucl <- NA

# Vector of land cover classes
all.bm.veg <- unique(boot.qh.bm$bm.veg)

for (curr.bm.veg in all.bm.veg) {
  # Get bootstrap results for all EIs for current land cover class
  curr.boot.qh <- boot.qh.bm[boot.qh.bm$bm.veg == curr.bm.veg, ]

  # Initialize vector to hold the means of each bootstrap run
  q.vec <- rep(NA, boot.reps)

  # For each bootstrap run, get mean quality across all EIs and
  # save to vector, omitting NAs and Infinity
  for (n in 1:boot.reps) {
    rep.col <- paste0("X", n)
    rep.curr.boot.qh <- curr.boot.qh[[rep.col]][!(curr.boot.qh[[rep.col]] == Inf) &
      !(is.na(curr.boot.qh[[rep.col]]))]
    q.vec[n] <- mean(rep.curr.boot.qh, na.rm = T) # Avg quality for the current run
  }

  boot.mean <- mean(q.vec, na.rm = T) # grand mean quality across all boot runs
  boot.devs <- boot.mean - q.vec # run deviances from the grand mean

  # Get 95% CLs of the deviances
  boot.quant <- quantile(boot.devs, c(0.025, 0.975), na.rm = T)
  lower <- boot.quant[[1]]
  upper <- boot.quant[[2]]

  # Populate confidence limits in main data frame (qh.bm) by
  # subtracting bootstrap deviances from the observed quality hectares
  qh.bm$qh.lcl[qh.bm$bm.veg == curr.bm.veg] <- qh.bm$qh[qh.bm$bm.veg == curr.bm.veg] + lower
  qh.bm$qh.ucl[qh.bm$bm.veg == curr.bm.veg] <- qh.bm$qh[qh.bm$bm.veg == curr.bm.veg] + upper
}
cat("done\r\n")

# Save final benchmark QH file
fileandpath <- paste(RESULTSDIR, SUMMARY.FOCAL.BM.FILE, sep = "")
cat(paste0("  Saving benchmark QH file '", fileandpath, "'..."))
write.csv(qh.bm, file = fileandpath, row.names = FALSE)
cat("\n")

##########################################################
# End script
##########################################################

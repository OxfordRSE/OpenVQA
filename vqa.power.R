###############################################
# Calculates post hoc power vs. sample size curves at range of
# effect sizes for overall quality (all ind) of one or more land
# cover classes.
###############################################

# Latest update: 17 Feb 2023

#######################
# Parameters
#######################

# Critical!
# Determines data directories and params file used
# Params file used=paste0("params.", PROJ, ".R")
# Assessment code and everything else are set in the params file
PROJ <- "teck-ev"
PROJ <- "vqa-pub"
PROJ <- "teck-dpm"
PROJ <- "teck-pom"

########################
# Single vegetation class to process
# Set the land cover you wish to model
# FOR TESTING ONLY!
# NOT USED UNLESS COMMENT OUT LOOP "veg loop"
########################
curr.veg <- "Alpine" # DONE
curr.veg <- "Alpine dwarf shrub" # DONE
curr.veg <- "Avalanche feature" # DONE
curr.veg <- "Brushland/Grassland" # DONE
curr.veg <- "Dry forest, early-mid"
curr.veg <- "Dry forest, mature"
curr.veg <- "Dry forest, old"
curr.veg <- "Intermediate forest, early-mid"
curr.veg <- "Intermediate forest, mature"
curr.veg <- "Intermediate forest, old"
curr.veg <- "Krummholz"
curr.veg <- "Rock/Talus"
curr.veg <- "Wet forest, early-mid"
curr.veg <- "Wet forest, mature"
curr.veg <- "Wet forest, old"
curr.veg <- "Wetland"
curr.veg <- "WBP"
curr.veg <- "Mesic forest, Mature"

########################
# LAND COVER CLASSES TO PROCESS
# Landcover classes, not bm vegetation
# Each version of df.veg will typically
# be specific to one project only
########################

# teck-ev and teck-ev.demo
df.veg <- data.frame(veg = c(
  "Mesic forest, Mature - Disturbed"
))

df.veg <- data.frame(veg = c(
  "Brushland/Grassland (reclaimed)",
  "Dry forest, early-mid (reclaimed)",
  "Intermediate forest, early-mid (reclaimed)",
  "Intermediate forest, mature (reclaimed)",
  "Rock/Talus (reclaimed)",
  "Wet forest, mature (reclaimed)"
))
df.veg <- data.frame(veg = c(
  "Intermediate forest, mature (reclaimed)",
  "Rock/Talus (reclaimed)"
))
df.veg <- data.frame(veg = c(
  "Dry forest, Early-mid - Ecosystem-Reclaimed"
))
df.veg <- data.frame(veg = c(
  "Dry forest, Early-mid - Reclaimed"
))
df.veg <- data.frame(veg = c(
  "Grassland - Reclaimed"
))
df.veg <- data.frame(veg = c(
  "Mesic forest, Early-mid - Natural"
))
df.veg <- data.frame(veg = c(
  "Rock/Talus - Natural"
))

# vqa-pub / baseline
df.veg <- data.frame(veg = c(
  "Alpine",
  "Alpine Dwarf Shrub",
  "Avalanche feature",
  "Brushland/Grassland",
  "Dry forest, Early-mid",
  "Dry forest, Mature",
  "Dry forest, Old",
  "Intermediate forest, Early-mid",
  "Intermediate forest, Mature",
  "Intermediate forest, Old",
  "Krummholz",
  "Rock/Talus",
  "Wet forest, Early-mid", "Wet forest, Mature",
  "Wet forest, Old",
  "Wetland"
))
# vqa-pub / reclamation
df.veg <- data.frame(veg = c(
  "Brushland/Grassland (reclaimed)",
  "Dry forest, Early-mid (reclaimed)",
  "Intermediate forest, Early-mid (reclaimed)",
  "Wet forest, Early-mid (reclaimed)"
))
# # Baseline vegetation which is also in historical reclamation
# # For testing, vqa-pub/000-baseline_test
# df.veg<-data.frame(veg=c(
# "Brushland/Grassland",
# "Dry forest, Early-mid",
# "Intermediate forest, Early-mid",
# "Wet forest, Early-mid"
# ))
df.veg <- data.frame(veg = c(
  "Wet forest, Early-mid (reclaimed)"
))

# teck-dpm
df.veg <- data.frame(veg = c(
  "Balsam Fir/Black Spruce Forest, Early-mid [Logging]",
  "Balsam Fir/Black Spruce Forest, Early-mid [Mining, Natural Regeneration]",
  "Balsam Fir/Black Spruce Forest, Early-mid [Mining, Reclaimed]"
))
df.veg <- data.frame(veg = c(
  "Balsam Fir/Black Spruce Forest, Early-mid [Logging]"
))

# teck-pom
df.veg <- data.frame(veg = c(
  "Western Hemlock - Western Red-cedar Cool-Mesic Central Rocky Mountain Forest & Woodland Alliance, Mature"
))

########################
# Ecological ind (abbreviations) to process
########################
df.ei <- data.frame(ei = c(
  "asc",
  "ba",
  "mccsc",
  "mpck",
  "pbi",
  "pbst",
  "pcnwbp",
  "pinf",
  "sa"
))
# Ecological ind (abbreviations) to process
df.ei <- data.frame(ei = c(
  "GC",
  "PCESS",
  "PCGF",
  "SR",
  "TD"
))
# # Omit GC (functional group = physical environment)
# df.ei<-data.frame(ei=c(
# 'SR',
# 'TD',
# 'PCESS',
# 'PCGF'
# ))

# Overall quality algorithm
q.overall.method <- "simple arith mean" # Simple arith avg of ei qualities
q.overall.method <- "f.group geom mean" # Arith avg of ei qualities within categories,
# then geom mean of category mean

# Effect sizes to test
# Array of values distributed over [0:1].
# Recommend no more than 3, otherwise takes too long and
# graph is too busy.
# e.size determines critical value of quality:
# 		q.crit.upper = q.mean + e.size,
# 		q.crit.lower = q.mean - e.size
# where q.mean is mean quality.
# A significant diff is found when:
# 		q.obs > q.crit.upper --OR--
#  	q.obs < q.crit.lower
# where q.obs = observed quality.
# Note that q.mean and q.obs are distributed over [0:1].
e.sizes <- c(0.1, 0.15, 0.2)
e.sizes <- c(0.05, 0.1)
e.sizes <- c(0.15)
e.sizes <- c(0.1, 0.15)
e.sizes <- c(0.1)
e.sizes <- c(0.05)

# Target power for predicting minimum sample size
# Recommend conventional power of 0.80
pwr.target <- 0.8

wd <- getwd()
setwd(wd)

params.file <- paste0("params.", PROJ, ".R")
cat("Loading parameters file '", params.file, "'...", sep = "")
if (file.copy(from = params.file, to = "params.R", overwrite = TRUE) == FALSE) {
  stop("ERROR: file copy failed!\n")
}
cat("done\n\n")

# load parameters
source(params.file, local = TRUE)

# Get functional groups
f.groups <- unique(EI.F.GROUPS[, 2])
fg.disp <- ""
for (i in 1:length(f.groups)) {
  if (i == 1) {
    fg.disp <- f.groups[i]
  } else {
    fg.disp <- paste0(fg.disp, ", ", f.groups[i])
  }
}

# load functions
source(paste(INCLUDESDIR, "functions.R", sep = ""), local = TRUE)

# Seed for bootstrap repeatability
# NOTE: it matters where you place set.seed!
# See: https://stackoverflow.com/a/64236620/2757825
# Needs changes to be implemented properly
# Should be passed to bootstrap function, but the latter
# currently does not support a set.seed parameter
do.set.seed <- FALSE # Set randomization seed to fix results?
seed <- 123 # Seed value. Only used if do.set.seed ==T

# Main model parameters
boot.reps <- 1000 # Number of bootstrap replications; ideal: 10000; never <1000
sims <- 50 # No of replicates per power test (at each sample size); ideal: 1000
n.start <- 27 # Starting (minimum) sample size
n.final <- 33 # Final (maximum) sample size, up n.max (see below)
# set to very large number to use n.max
n.cap <- "n.final" # Cap max value of n at some value lower than n.final
# Values: "n.obs", "n.max", "n.min", "n.final", "".
# Empty ("") same as n.final.
# Option n.obs caps sample size for each vector separately
# at the observed sample size for that vector.
n.step <- 3 # Units of n to loop over (default=1)
reduce.observed <- TRUE # Rarify observed samples? If FALSE, only
# bootstrap samples are reduced & observed
# curves stay constant.


# Use permutation test of significant difference for means test (slower)?
test.diff.obs <- FALSE # Observed quality
test.diff.boot <- FALSE # Bootstrap quality

##############################################
# Directories, files and save options
##############################################

# Save results to results.file?
save.results <- TRUE

# Save the bootstrap file?
save.boot <- TRUE

# Create power results directory if missing
dir.create(file.path(RESULTSDIR, "power"), showWarnings = F)
DIR.PWR.RESULTS <- paste0(RESULTSDIR, "power/")

# Results file suffix
# Optional suffix to save different results to files with different names
# You will need to alter input file parameters of downstream graphing
# files if you alter the default file names
# Set to empty string "" to use default names
f.results.suffix <- "brushland-grassland.boot-1000"
f.results.suffix <- "reclamation.boot-1000"
f.results.suffix <- "reclamation.boot-10000"
f.results.suffix <- "no.GC"
f.results.suffix <- "no.GC.05"
f.results.suffix <- "with.GC.05.pt2"

# The simulations file
# Individual runs are appended to this file
sims.filename <- "power.sims.csv"

# Final results file
# Individual replicates (power calculations based on multiple
# replicates) are appended to this file
results.filename <- "power.csv"

# Bootstrap results file
boot.filename <- "power.boot.csv"

replace.file <- FALSE # TRUE: replace results files completely
# FALSE: append results if file exists

replace.data <- FALSE # Replace results for this vegetation in results.file
# If false then results are appended
# Only applies if save.results==TRUE

echo.reps <- TRUE # TRUE=screen echo each replicate

#######################
# Prepare remaining settings
#######################

# CHECK THIS! May need to move...
# It matters where you place set.seed!
# See: https://stackoverflow.com/a/64236620/2757825
if (do.set.seed == T) set.seed(seed) # for repeatability

# Message if distribution unknown or not supported
msg_baddist <- "ERROR: Operation not supported for this distribution!"

#################################
# Names and locations of input and output files
#################################

# Raw data input files (in inputs/ folder)
metadata.file <- "focal_summary.csv" #  land cover metadata CHECK CORRECT?
all.ind.list.file <- "dist.eval.csv" # List of EIs & strata to include & exclude
whitelist.file <- "dist.eval.exceptions.csv" # Whitelist of landcover-specific
# exceptions to black list

##############################################
# Gather remaining EI attributes
##############################################

df.ei$dist <- NA
df.ei$ei.name <- NA
df.ei$q.method <- NA
df.ei$has.stratum <- NA
df.ei$test.tail <- NA
df.ei$bm.val <- NA

for (i in 1:nrow(df.ei)) {
  ei <- df.ei[i, c("ei")]
  df.ei[i, c("dist")] <- ei.params(ei)$distn
  df.ei[i, c("ei.name")] <- ei.params(ei)$ei.name
  df.ei[i, c("q.method")] <- ei.params(ei)$q.method
  df.ei[i, c("has.stratum")] <- ei.params(ei)$has.stratum
  df.ei[i, c("test.tail")] <- ei.params(ei)$test.tail
  df.ei[i, c("bm.val")] <- ei.params(ei)$bm.val
}

#################################
# Set final path + name for results files
#################################

# Suffix if applicable
if (!f.results.suffix == "") {
  sims.filename <- paste0(
    tools::file_path_sans_ext(sims.filename), ".", f.results.suffix, ".csv"
  )
  results.filename <- paste0(
    tools::file_path_sans_ext(results.filename), ".", f.results.suffix, ".csv"
  )
  boot.filename <- paste0(
    tools::file_path_sans_ext(boot.filename), ".", f.results.suffix, ".csv"
  )
}

# Add path
sims.file <- paste0(DIR.PWR.RESULTS, sims.filename)
results.file <- paste0(DIR.PWR.RESULTS, results.filename)
boot.file <- paste0(DIR.PWR.RESULTS, boot.filename)

#################################
# Confirm operation
#################################

# Starting message
br <- "\n"
h.line <- "------------------------------------------------"
main.title <- "VQA Overall Power Simulation"

cat(paste0(h.line, br, main.title, br, br))
cat("Settings:\n")
cat("  Project='", PROJ, "'\n", sep = "")
cat("  Assessment='", ASSESS, "'\n", sep = "")
cat("  Vegetation:\n")
cat(paste0("    ", as.character(df.veg$veg)), sep = "\n")
cat("  Indicators:\n")
cat(paste0("    ", as.character(df.ei$ei)), sep = "\n")
cat("  Functional groups: ", fg.disp, "\n", sep = "")
cat("  Input files:\n")
cat(paste0("    ", INPUTDIR, metadata.file, "\n"))
cat(paste0("    ", INPUTDIR, all.ind.list.file, "\n"))
cat(paste0("    ", INPUTDIR, whitelist.file, "\n"))
cat("  Output files:\n")
cat(paste0("    ", results.file, "\n"))
cat(paste0("    ", sims.file, "\n"))
cat(paste0("    ", boot.file, "\n"))
cat("  Replace data: ", replace.data, "\n", sep = "")
cat("  Effect sizes: ", e.sizes, "\n", sep = " ")
cat("  boot.reps: ", boot.reps, "\n", sep = "")
cat("  sims: ", sims, "\n", sep = "")
cat("  n.start: ", n.start, "\n", sep = "")
cat("  n.final : ", n.final, "\n", sep = "")
cat("  n.cap: ", n.cap, "\n", sep = "")
cat("  n.step: ", n.step, "\n", sep = "")
cat("  reduce.observed: ", reduce.observed, "\n", sep = "")
cat("\n")

msg.conf <- "Continue? (y/n):"
yes <- c("y", "Y", "Yes", "yes")

if (interactive()) {
  response <- readline(msg.conf)
} else {
  cat(msg.conf)
  response <- readLines("stdin", n = 1)
}

if (!response %in% yes) stop_quietly("Operation cancelled\n\n")
main.title <- "Begin operation"
cat(paste0(br, h.line, br, main.title, br, br))

cat("Importing metadata:\n")

##############################################
# Import land cover metadata
# Needed for blacklisting/whitelisting EIs and grouping into
# categories for overall quality algorithm
##############################################

# Import site-level metadata
# Used to associate focal with benchmark vegetation
print(paste("Importing  '", metadata.file, "'", sep = ""))
input.file <- paste(INPUTDIR, metadata.file, sep = "")
rawData <- read.csv(input.file, header = T)
landcover <- unique(rawData[, c("landCoverCode", "targetLandCoverCode")])
colnames(landcover) <- c("landcover", "bm.vegetation")

# Convert to human-readable versions of codes
landcover$landcover <- human.readable.landcover(landcover$landcover)
landcover$bm.vegetation <- human.readable.bm.veg(landcover$bm.vegetation)

##############################################
# Import indicator whitelist and blacklist
#
# Indicator: any ei + stratum measured separately
# Blacklist: indicators to exclude
##############################################

print(paste("Importing  '", all.ind.list.file, "'", sep = ""))
input.file <- paste(INPUTDIR, all.ind.list.file, sep = "")
all.ind.list <- read.csv(input.file, header = T)
all.ind.list$ei.strat <- paste0(all.ind.list$EI, "-", all.ind.list$stratum)
all.ind.list$ei.strat <- sub("-nostrata", "", all.ind.list$ei.strat)
all.ind.list <- all.ind.list[, c("EI", "stratum", "ei.strat", "f.group", "include")]
# Set new name f.group back to old name "category" for compatibility
# with rest of script
colnames(all.ind.list)[colnames(all.ind.list) == "f.group"] <- "category"

blacklist <- all.ind.list[all.ind.list$include == FALSE, ]
ind.list <- all.ind.list[all.ind.list$include == TRUE, ]

print(paste("Importing  '", whitelist.file, "'", sep = ""))
input.file <- paste(INPUTDIR, whitelist.file, sep = "")
whitelist <- read.csv(input.file, header = T)
whitelist <- whitelist[!is.na(whitelist$include), ]
whitelist <- whitelist[, c("EI", "stratum", "bm.vegetation", "include")]

veg.num <- 1

for (v in 1:nrow(df.veg)) { ### Begin veg loop
  curr.veg <- toString(df.veg[v, c("veg")])

  ######################################
  # Import raw data and merge into single data frame
  ######################################

  main.title <- "Importing data"
  cat(paste0(br, main.title, br))

  n.EI <- nrow(df.ei)

  for (e in 1:n.EI) {
    curr.ei <- df.ei[e, 1]
    raw.file.name <- paste0(curr.ei, "_raw.csv")
    print(paste0("Importing  '", raw.file.name, "'"))
    raw.file <- paste(RESULTSDIR, raw.file.name, sep = "")
    curr.ei.raw <- read.csv(raw.file, header = T)

    # Special handling to remove any non-matching columns
    curr.ei.raw <- curr.ei.raw[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI")]
    colnames(curr.ei.raw) <- c(
      "plotCode", "focalOrBenchmark", "landcover",
      "bm.vegetation", "stratum", "ei.val"
    )
    curr.ei.raw$EI <- curr.ei

    if (e == 1) {
      # Make new data frame
      dat.raw <- curr.ei.raw
    } else {
      # append to existing data frame
      dat.raw <- rbind(dat.raw, curr.ei.raw)
    }
  }

  ##############################################
  # Flag EI-landcover-stratum combinations to include &
  # remove the rest
  ##############################################

  dat.raw <- merge(x = dat.raw, y = all.ind.list, by = c("EI", "stratum"), all.x = TRUE)
  dat.raw <- merge(x = dat.raw, y = whitelist, by = c("EI", "stratum", "bm.vegetation"), all.x = TRUE)
  dat.raw <- transform(dat.raw, include = ifelse(is.na(include.y),
    include.x, as.character(include.y)
  ))

  # Drop the blacklisted classes and reorder columns
  dat.raw <- dat.raw[dat.raw$include == TRUE, c("EI", "landcover", "bm.vegetation", "stratum", "category", "plotCode", "focalOrBenchmark", "ei.val")]

  ##############################################
  # Select subset of data for current land cover class &
  # determine sample size parameters
  ##############################################

  # Get raw focal data for current land cover class CHANGED
  dat <- dat.raw[dat.raw$landcover == curr.veg, ]

  # Get bm veg for curr.veg
  curr.bm <- unique(dat.raw[dat.raw$landcover == curr.veg, ]$bm.vegetation)

  # Get raw bm data for current land cover class
  dat.bm <- dat.raw[dat.raw$bm.vegetation == curr.bm & dat.raw$focalOrBenchmark == "b", ]

  # Reset bm landcover to landcover being currently assessed
  dat.bm$landcover <- curr.veg

  # Combine the focal and bm data frame
  dat <- rbind(dat, dat.bm)

  # Count number of distinct ei+stratum classes ("ei.full")
  ei.full <- unique(paste(dat$landcover, dat$stratum, dat$EI, sep = "+"))
  n.classes <- length(ei.full)

  ei.all <- with(
    dat,
    aggregate(
      ei.val,
      list(
        EI = EI, landcover = landcover, stratum = stratum,
        category = category, focalOrBenchmark = focalOrBenchmark
      ),
      FUN = function(x) {
        c(n = length(x))
      }
    )
  )
  names(ei.all)[names(ei.all) == "x"] <- "n"

  # Reorder columns and sort
  col_idx <- grep("landcover", names(ei.all))
  ei.all <- ei.all[, c(col_idx, (1:ncol(ei.all))[-col_idx])]
  ei.all <- ei.all[with(ei.all, order(landcover, EI, stratum, focalOrBenchmark)), ]

  n.f.max <- max(ei.all[ei.all$focalOrBenchmark == "f", c("n")])
  n.f.min <- min(ei.all[ei.all$focalOrBenchmark == "f", c("n")])


  n.b.max <- max(ei.all[ei.all$focalOrBenchmark == "b", c("n")])
  n.b.min <- min(ei.all[ei.all$focalOrBenchmark == "b", c("n")])
  n.min <- min(n.f.min, n.b.min)
  n.max <- max(n.f.max, n.b.max)

  if (n.cap == "n.min") {
    n.finish <- min(n.min, n.final)
  } else if (n.cap == "n.max" || n.cap == "n.obs") {
    n.finish <- min(n.max, n.final)
  } else {
    n.finish <- n.final
  }

  ns <- seq(from = 3, to = n.max, by = 1)

  # Restructure ei.all to make it a bit easier to use
  cols <- c("EI", "stratum")
  ei.all$ei.full <- apply(ei.all[, cols], 1, paste, collapse = "-")
  ei.all.f <- ei.all[ei.all$focalOrBenchmark == "f", c("ei.full", "landcover", "EI", "stratum", "category", "n")]
  names(ei.all.f)[names(ei.all.f) == "n"] <- "n.f"
  ei.all.b <- ei.all[ei.all$focalOrBenchmark == "b", c("ei.full", "n")]
  names(ei.all.b)[names(ei.all.b) == "n"] <- "n.b"
  ei.all <- merge(ei.all.f, ei.all.b)

  # Restructure the raw data to make it comparable
  dat$ei.full <- apply(dat[, cols], 1, paste, collapse = "-")
  n.cols <- ncol(dat)
  col.mv <- grep("ei.full", names(dat))
  col.next <- grep("plotCode", names(dat))
  cols.reordered <- move.col(col.mv, col.next, n.cols)
  dat <- dat[, cols.reordered]
  col.mv <- grep("EI", names(dat))
  col.next <- grep("stratum", names(dat))
  cols.reordered <- move.col(col.mv, col.next, n.cols)
  dat <- dat[, cols.reordered]

  #################################
  # Startup message
  #################################

  br <- "\n"
  main.title <- "Running simulations"
  veg.msg <- paste0("Landcover: ", curr.veg)

  # Echo starting message
  cat(paste0(br, main.title, br, br, veg.msg, br, h.line, br))

  #################################
  # Begin replicates
  #################################

  # Set total rows for results matrix
  tot.rows <- n.finish * length(e.sizes)

  # prepare the results matrix
  results <- matrix(NA,
    nrow = tot.rows,
    ncol = sims
  )

  # Prepare data frame for individual replicate results
  df.sims <- data.frame(
    landcover = character(0),
    e.size = character(0),
    boot.reps = character(0),
    sims = character(0),
    n = character(0),
    sim = character(0),
    q = character(0),
    q.lcl = character(0),
    q.ucl = character(0),
    q.crit.lower = character(0),
    q.crit.upper = character(0),
    sig.lower = character(0),
    sig.upper = character(0),
    sig = character(0)
  )

  # Prepare the power results data frame
  df.pwr <- data.frame(
    n = integer(0),
    e.size = numeric(0),
    e.size.adj = numeric(0),
    e.size.min = numeric(0),
    pwr = numeric(0)
  )

  start.time <- proc.time()
  # We want element 3 for elapsed ("clock") time
  start <- format(.POSIXct(start.time[[3]]), "%H:%M:%OS2")
  cat("Start time:", start)

  es.num <- 1

  for (es in 1:length(e.sizes)) { # START effect size loop
    ### for (es in 3:3) {				# For testing with loop
    ### es <- 3							# For testing without loop

    # Set current effect size & echo
    e.size <- e.sizes[es]
    cat(paste0("\n", "Effect size: ", e.size, "\n"))
    flush.console()

    ##################################
    # Initialize the matrices needed to hold
    # intermediate results for current effect size
    ##################################

    # # prepare df of power results
    # pwr <- rep(NA, length(ns))

    # prepare the quality matrix
    qs <- matrix(NA,
      nrow = tot.rows,
      ncol = sims
    )

    # prepare the observed effect sizes matrix
    e.sizes.adj <- matrix(NA,
      nrow = tot.rows,
      ncol = sims
    )

    # prepare the upper CL matrix
    q.ucls <- matrix(NA,
      nrow = tot.rows,
      ncol = sims
    )

    # prepare the lower CL matrix
    q.lcls <- matrix(NA,
      nrow = tot.rows,
      ncol = sims
    )

    n.num <- 1

    for (n in seq(n.start, n.finish, by = n.step)) { # START sample size loop
      ### for (n in 20:20) {			# For testing with loop
      ### n <- 20							# For testing without loop
      # Set vector sample sizes
      if (n.cap == "n.obs") {
        n.f <- min(n, n.f.min)
        n.b <- min(n, n.b.min)
      } else {
        n.f <- n
        n.b <- n
      }

      # Run multiple replicates for this sample size
      for (sim in 1:sims) { # START sim loop
        ### for (sim in 10:10) {			# For testing with loop
        ### sim <- 5 							# For testing without loop

        ####################################
        # Set up EI & stratum loops here to pull separate
        # samples needed for each EI quality calculation
        ####################################

        # df of ei + stratum combinations and categories
        ei.full.all.cats <- unique(ei.all[, c("ei.full", "category")])

        # Vector of ei + stratum combinations, and count of combinations
        ei.full.all <- unique(ei.full.all.cats$ei.full)
        n.ei <- length(ei.full.all)

        # Empty vector to hold quality results for individual EIs
        q.eis <- rep(NA, n.ei) # EI quality

        # Set up matrices to hold f & b subsamples so they can
        # be reused for bootstrap. Set dimensions to largest
        # possible number of samples
        f.samps <- matrix(NA,
          nrow = n.ei,
          ncol = max(n.max, n.final)
        )
        b.samps <- matrix(NA,
          nrow = n.ei,
          ncol = max(n.max, n.final)
        )

        # Calculate and save individual EI quality
        # for ( i in 1:n.ei ) {		# START ei loop
        i <- 1
        while (i <= n.ei) { # START ei WHILE loop
          ### for ( i in 4:4 ) {		# For testing single value with loop
          ### i <- 1						# For testing without loop

          # Get attributes of current EI
          curr.ei.full <- ei.full.all[i]
          curr.ei <- ei.all[ei.all$ei.full == curr.ei.full, c("EI")]
          distn <- df.ei[df.ei$ei == curr.ei, c("dist")]
          test.tail <- df.ei[df.ei$ei == curr.ei, c("test.tail")]
          q.method <- df.ei[df.ei$ei == curr.ei, c("q.method")]

          # Get actual samples for this indicator
          f <- dat[dat$ei.full == curr.ei.full & dat$focalOrBenchmark == "f", c("ei.val")]
          b <- dat[dat$ei.full == curr.ei.full & dat$focalOrBenchmark == "b", c("ei.val")]
          f.samp <- f
          b.samp <- b

          # Subsample if requested
          if (reduce.observed == T) {
            # Reduce samples if n.samp < n.obs
            if (n < length(f)) f.samp <- sample(f, n, replace = F)
            if (n < length(b)) b.samp <- sample(b, n, replace = F)

            # Increase samples if n.samp > n.obs, but only if no cap
            if (!(n.cap == "nobs")) {
              # Append subsample to original vector
              if (n > length(f)) {
                f.samp <-
                  as.vector(c(f, sample(f, n - length(f), replace = T)))
              }
              if (n > length(b)) {
                b.samp <-
                  as.vector(c(b, sample(b, n - length(b), replace = T)))
              }
            }
          }

          # Save the subsamples
          f.samps[i, 1:length(f.samp)] <- f.samp
          b.samps[i, 1:length(b.samp)] <- b.samp

          # Determine domain of x for modeling pdf
          # Use domain of actual data, not subsamples
          if (distn == "Bet") {
            xs <- seq(0, 1, 100)
          } else if (distn == "NBin") {
            xs.max <- ceiling(max(f, b) + (0.05 * (max(f, b) - min(f, b))))
            xs <- seq(0, xs.max + 10)
          } else if (distn == "gamma") {
            xs.max <- max(f, b) + (0.05 * (max(f, b) - min(f, b)))
            xs <- seq(0, xs.max, length.out = 100)
          } else {
            stop(paste0(msg_baddist, "(dist=')", distn, "')"))
          }

          # Get observed quality for current sample
          if (distn == "NBin" || distn == "gamma") {
            qual <- q.overlap.no.cls(distn, test.tail = test.tail, f = f.samp, b = b.samp, xs = xs)
            q.ei <- qual$ol.obs
          } else if (distn == "Bet") {
            if (q.method == "fixed") { # Fixed benchmark method
              bm.val <- df.ei[df.ei$ei == curr.ei, c("bm.val")]
              qual <- qual.beta.fixed(f = f.samp, b.fixed = bm.val, test.diff = test.diff.obs)
            } else { # Empirical overlap-based method
              qual <- qual.beta(f = f.samp, b = b.samp, xs = xs, test.diff = test.diff.obs)
            }
            q.ei <- qual$q
          } else {
            stop(paste0(msg_baddist, "(dist=')", distn, "')"))
          }

          # Save quality for current EI
          # q.eis[i] <- q.ei
          # print(paste0("  q(", curr.ei.full, ")=", q.ei))
          # flush.console()

          # TESTING
          if (is.na(q.ei) || q.ei == "") {
            # # Quality calculation failed; do not increment i
            # print( paste0( curr.ei, ": FAILED! Discarding results for this ei..." ) )
            # flush.console()
          } else {
            # Quality calculation successful; save Q and increment i
            # print( paste0( curr.ei, ": ", q.ei ) )
            # flush.console()
            q.eis[i] <- q.ei
            i <- i + 1
          }
          # END TESTING
        } # END ei WHILE loop

        ###################################
        # Overall quality & critical values
        ###################################

        if (q.overall.method == "simple arith mean") {
          q <- mean(q.eis)
        } else if (q.overall.method == "f.group geom mean") {
          q.eis.cat <- data.frame(ei.full.all.cats, q.eis)
          q.cat <- with(
            q.eis.cat,
            aggregate(
              q.eis,
              list(category = category),
              FUN = function(x) {
                c(q.ei.mean = mean(x))
              }
            )
          )
          names(q.cat)[names(q.cat) == "x"] <- "cat.mean"
          q <- gmean(q.cat$cat.mean)
        } else {
          msg <- paste0("ERROR: q.overall.method '", q.overall.method, "' not valid!")
          stop(msg)
        }

        q.crit.lower <- q - e.size
        q.crit.upper <- q + e.size
        q.crit.adj <- min(1, q.crit.upper)

        ###################################
        # Bootstrap CLs of overall quality
        ###################################

        # Generate bootstrapped quality of current samples
        boot.q <- rep(NA, boot.reps)

        for (j in 1:boot.reps) { # START bootstrap loop
          ### j <- 1 # for testing without loop

          # Calculate individual EI qualities
          # for ( i in 1:n.ei ) {		# START ei loop
          i <- 1
          while (i <= n.ei) { # START ei WHILE loop
            ### for ( i in 4:4 ) {		# For testing single value with loop
            ### i <- 1		# For testing without loop

            # Get attributes of current EI
            curr.ei.full <- ei.full.all[i]
            curr.ei <- ei.all[ei.all$ei.full == curr.ei.full, c("EI")]
            distn <- df.ei[df.ei$ei == curr.ei, c("dist")]
            test.tail <- df.ei[df.ei$ei == curr.ei, c("test.tail")]
            q.method <- df.ei[df.ei$ei == curr.ei, c("q.method")]

            # # Get actual samples for this indicator
            # f <- dat[ dat$ei.full==curr.ei.full & dat$focalOrBenchmark=='f', c("ei.val")]
            # b <- dat[ dat$ei.full==curr.ei.full & dat$focalOrBenchmark=='b', c("ei.val")]

            # Reuse the saved subsamples
            f.samp <- f.samps[i, which(!(is.na(f.samps[i, ])))]
            b.samp <- b.samps[i, which(!(is.na(b.samps[i, ])))]

            # Take random sample of the saved subsample
            boot.samp.f <- sample(f.samp, n.f, replace = T)
            boot.samp.b <- sample(b.samp, n.b, replace = T)

            # Determine domain of x for modeling pdf
            # Use domain of actual data, not subsamples
            if (distn == "Bet") {
              xs <- seq(0, 1, length.out = 50)
            } else if (distn == "NBin") {
              xs.max <- ceiling(max(f, b) + (0.05 * (max(f, b) - min(f, b))))
              xs <- seq(0, xs.max + 10)
            } else if (distn == "gamma") {
              xs.max <- max(f, b) + (0.05 * (max(f, b) - min(f, b)))
              xs <- seq(0, xs.max, length.out = 100)
            }

            # Get quality for current sample
            if (distn == "NBin" || distn == "gamma") {
              qual <- q.overlap.no.cls(distn, test.tail = test.tail, f = boot.samp.f, b = boot.samp.b, xs = xs)
              q.ei <- qual$ol.obs
            } else if (distn == "Bet") {
              if (q.method == "fixed") {
                # Fixed benchmark method
                bm.val <- df.ei[df.ei$ei == curr.ei, c("bm.val")]
                qual <- qual.beta.fixed(f = boot.samp.f, b.fixed = bm.val, test.diff = test.diff.boot)
              } else {
                # Empirical overlap-based method
                qual <- qual.beta(f = boot.samp.f, b = boot.samp.b, xs = xs, test.diff = test.diff.boot)
              }
              q.ei <- qual$q
            }

            # q.eis[i] <- q.ei
            # print(paste0("  q(", curr.ei.full, ")=", q.ei))
            # flush.console()

            # TESTING
            if (is.na(q.ei) || q.ei == "") {
              # # Error; do not increment i
              # print( paste0( "Boot q.ei for ", curr.ei, ": FAILED! Discarding results for this ei..." ) )
              # flush.console()
            } else {
              # print( paste0( "Boot q.ei for ", curr.ei, ": ", q.ei ) )
              # flush.console()
              q.eis[i] <- q.ei
              i <- i + 1
            }
            # END TESTING
          } # END ei WHILE loop

          ################################
          # Overall quality for this bootstrap run
          ################################

          if (q.overall.method == "simple arith mean") {
            boot.q.curr <- mean(q.eis)
          } else if (q.overall.method == "f.group geom mean") {
            q.eis.cat <- data.frame(ei.full.all.cats, q.eis)
            q.cat <- with(
              q.eis.cat,
              aggregate(
                q.eis,
                list(category = category),
                FUN = function(x) {
                  c(q.ei.mean = mean(x))
                }
              )
            )
            names(q.cat)[names(q.cat) == "x"] <- "cat.mean"
            boot.q.curr <- gmean(q.cat$cat.mean)
          }

          if (is.null(boot.q.curr) || is.na(boot.q.curr)) {
            boot.q[j] <- NA
          } else {
            boot.q[j] <- boot.q.curr
          }
        } # END bootstrap loop

        # Convert boot.q to data frame and all column for veg, e.size, n and sims
        df.boot.q <- as.data.frame(t(boot.q))
        for (col.no in 1:boot.reps) colnames(df.boot.q)[col.no] <- paste0("q.boot.", col.no)

        df.boot.q$veg <- curr.veg
        df.boot.q$e.size <- e.size
        df.boot.q$n <- n
        df.boot.q$sim <- sim
        df.boot.q$q.obs <- q

        df.boot.q <- df.reorder(df.boot.q, col.move = "veg", move.first = TRUE)
        df.boot.q <- df.reorder(df.boot.q, col.move = "e.size", col.before = "veg")
        df.boot.q <- df.reorder(df.boot.q, col.move = "n", col.before = "e.size")
        df.boot.q <- df.reorder(df.boot.q, col.move = "sim", col.before = "n")
        df.boot.q <- df.reorder(df.boot.q, col.move = "q.obs", col.before = "sim")

        # Accumulate bootstrap results for this simulation
        if (sim == 1) {
          boot.q.sim <- df.boot.q
        } else {
          boot.q.sim <- rbind(boot.q.sim, df.boot.q)
        }

        ########################################
        # Two-tailed significance test:
        # bootstrap upper 95% CL < q.crit.upper  AND
        # bootstrap lower 95% CL < q.crit.lower
        ########################################

        boot.q.mean <- mean(boot.q, na.rm = T) # mean bootstrap quality
        boot.q.dev <- boot.q - boot.q.mean # vector of bootstrap deviances

        # Upper and lower 95% CLs of bootstrap deviance
        boot.q.dev.cls <- quantile(boot.q.dev, c(0.025, 0.975), na.rm = T)

        # Upper and lower 95% CLs of bootstrap quality
        boot.q.cls <- q + boot.q.dev.cls # Note use observed quality (q), not boot quality
        q.lcl <- boot.q.cls[1]
        q.ucl <- boot.q.cls[2]

        # The significance test
        sig.lower <- q.crit.lower < q.lcl # Sig different than lower CL?
        sig.upper <- q.crit.upper > q.ucl # Sig diff than upper CL?
        sig <- sig.upper && sig.lower # Both must be true

        #################################
        # Save the results of the current simulation
        #################################

        # Save to results matrices
        results[n, sim] <- sig
        qs[n, sim] <- q
        e.sizes.adj[n, sim] <- abs(q.crit.adj - q)
        q.lcls[n, sim] <- q.lcl
        q.ucls[n, sim] <- q.ucl

        # Append individual replicate to replicates data frame
        df.sims <- rbind(df.sims, as.character(c(
          curr.veg,
          e.size,
          boot.reps,
          sims,
          n,
          sim,
          q,
          q.lcl,
          q.ucl,
          q.crit.lower,
          q.crit.upper,
          sig.lower,
          sig.upper,
          sig
        )))

        # Reset df to character if this is the first round
        # Stupid R automatically converts to factors when
        # appending to empty data frame
        if (sim == 1) df.sims[] <- lapply(df.sims, as.character)

        ########################
        # Echo results for this sim
        ########################

        q.disp <- specify_decimal(q, 2)
        q.lcl.disp <- specify_decimal(q.lcl, 2)
        q.ucl.disp <- specify_decimal(q.ucl, 2)
        q.crit.lower.disp <- specify_decimal(q.crit.lower, 2)
        q.crit.upper.disp <- specify_decimal(q.crit.upper, 2)

        if (echo.reps == TRUE) {
          cat(paste0(
            "    ",
            "n=", n,
            ", sim=", sim,
            ", q=", q.disp,
            ", q.lcl=", q.lcl.disp,
            ", q.ucl=", q.ucl.disp,
            ", q.crit.lower=", q.crit.lower.disp,
            ", q.crit.upper=", q.crit.upper.disp,
            ", sig.lower=", sig.lower,
            ", sig.upper=", sig.upper,
            ", sig=", sig,
            "\n"
          ))
          flush.console()
        }
      } # End replicate (sim) loop

      ###################################################
      # Calculate results of this power experiment (=set of replicates)
      ###################################################

      sigdiffs <- length(which(results[n, ] == TRUE))
      curr.pwr <- sigdiffs / sims
      curr.pwr.disp <- specify_decimal(curr.pwr, 2)
      q.disp <- specify_decimal(mean(qs[n, ], na.rm = T), 2)
      q.ucl.disp <- specify_decimal(mean(q.ucls[n, ], na.rm = T), 2)
      q.lcl.disp <- specify_decimal(mean(q.lcls[n, ], na.rm = T), 2)

      # Adjusted mean effect size for this replicate
      e.size.adj <- mean(e.sizes.adj, na.rm = T)
      e.size.adj.disp <- specify_decimal(e.size.adj, 2)

      # Theoretical effect size
      e.size.disp <- specify_decimal(e.size, 2)

      # Mean minimum detectable effect size
      e.size.min <- min(
        mean(q.ucls[n, ], na.rm = T) - mean(qs[n, ], na.rm = T),
        mean(mean(qs[n, ] - q.lcls[n, ], na.rm = T), na.rm = T)
      )
      e.size.min.disp <- specify_decimal(e.size.min, 2)

      # Echo results for this replicate
      cat(paste0(
        "nf=", n.f, ", nb=", n.b, ", q.mean=", q.disp,
        ", ucl.mean=", q.ucl.disp,
        ", lcl.mean=", q.lcl.disp,
        ", e.size=", e.size.disp,
        ", e.size.adj=", e.size.adj.disp,
        ", e.size.min=", e.size.min.disp,
        ", boot.reps=", boot.reps,
        ", sims=", sims,
        ", H0 rej=", sigdiffs,
        ", power=", curr.pwr.disp,
        "\n"
      ))
      flush.console()

      # Save power results for plotting
      df.pwr <- rbind(df.pwr, as.numeric(c(n, e.size, e.size.adj, e.size.min, curr.pwr)))

      # Accumulate bootstrap results for this n
      if (n.num == 1) {
        boot.q.n <- boot.q.sim
      } else {
        boot.q.n <- rbind(boot.q.n, boot.q.sim)
      }

      n.num <- n.num + 1
    } # End sample size loop

    # Accumulate bootstrap results for this es
    if (es.num == 1) {
      boot.q.es <- boot.q.n
    } else {
      boot.q.es <- rbind(boot.q.es, boot.q.n)
    }

    es.num <- es.num + 1
  } # End effect size loop

  ###################################
  # Save the individual simulation results
  ###################################

  # Name columns again because stupid R loses names
  # when append  to empty data frame
  colnames(df.sims) <- c(
    "landcover", "e.size", "boot.reps", "sims", "n", "sim",
    "q", "q.lcl", "q.ucl", "q.crit.lower", "q.crit.upper",
    "sig.lower", "sig.upper", "sig"
  )

  # write.csv(df.sims, file=sims.file, row.names=FALSE)
  # the following if...then replaces the line above
  if (save.results == TRUE) { ### 	BEGIN if-then: save sim results
    if (file.exists(sims.file) && replace.file == FALSE) {
      # Open existing results file if it exists and append new results
      sims.prev <- read.csv(sims.file, header = T, stringsAsFactors = FALSE)

      # Delete previous results for this land cover, if applicable
      if (replace.data == T) {
        rows <- which(sims.prev$landcover == curr.veg)
        # Delete rows pertaining to current combination
        if (length(rows) > 0) sims.prev <- sims.prev[-c(rows), ]
      }
      df.sims.all <- rbind(sims.prev, df.sims)
    } else {
      df.sims.all <- df.sims
    }

    # write.csv(pwr.results.all, file=results.file, row.names=FALSE)
    write.csv(df.sims.all, file = sims.file, row.names = FALSE)
  } ### END if-then: save sim results

  ###################################
  # Fit power curves and determine target
  # sample sizes for each e.size
  ###################################

  # Rename the final data frame
  colnames(df.pwr) <- c("n", "e.size", "e.size.adj", "e.size.min", "pwr")
  df.pwr$n.min <- NA
  df.pwr$fit <- NA

  for (es in 1:length(e.sizes)) { # Start second effect size loop

    # Set current effect size & echo
    e.size <- e.sizes[es]

    curr.df.pwr <- df.pwr[df.pwr$e.size == e.size, ]

    fit <- power.logistic(df.pwr = curr.df.pwr, pwr.target = pwr.target)

    if (fit[1] == "fail") {
      df.pwr$n.min[df.pwr$e.size == e.size] <- 99999
      df.pwr$fit[df.pwr$e.size == e.size] <- NA
    } else {
      n.min.target <- fit$n.min

      # Hack to control for bug that returns maximum value of x
      # over submitted x values (x.obs) if predicted value of x (x.pred)
      # is outside domain of x.obs
      if (n.min.target >= n.finish) n.min.target <- 99999

      df.pwr$n.min[df.pwr$e.size == e.size] <- n.min.target
      df.pwr$fit[df.pwr$e.size == e.size] <- fit$pwr.fit
    }
  } # End second effect size loop

  #######################
  # Add remaining columns
  #######################

  # Make data frame of complete results
  pwr.results <- df.pwr
  pwr.results$landcover <- curr.veg
  pwr.results$n.b <- length(b)
  pwr.results$n.f <- length(f)
  pwr.results$sims <- sims
  pwr.results$boot.reps <- boot.reps
  pwr.results$pwr.target <- pwr.target

  # Reorder some columns
  col_idx <- grep("n.f", names(pwr.results))
  pwr.results <- pwr.results[, c(col_idx, (1:ncol(pwr.results))[-col_idx])]
  col_idx <- grep("n.b", names(pwr.results))
  pwr.results <- pwr.results[, c(col_idx, (1:ncol(pwr.results))[-col_idx])]
  col_idx <- grep("landcover", names(pwr.results))
  pwr.results <- pwr.results[, c(col_idx, (1:ncol(pwr.results))[-col_idx])]

  #######################
  # Save results to file
  #######################

  if (save.results == TRUE) { ### 	BEGIN if-then: save results
    if (file.exists(results.file) && replace.file == FALSE) {
      # Open existing results file if it exists and append new results
      dat.prev <- read.csv(results.file, header = T, stringsAsFactors = FALSE)

      # Delete previous results for this combination, if applicable
      if (replace.data == TRUE) {
        rows <- which(dat.prev$landcover == curr.veg)
        # Delete rows pertaining to current combination
        if (length(rows) > 0) dat.prev <- dat.prev[-c(rows), ]
      }
      pwr.results.all <- rbind(dat.prev, pwr.results)
    } else {
      pwr.results.all <- pwr.results
    }

    write.csv(pwr.results.all, file = results.file, row.names = FALSE)
  } ### END if-then: save results

  # Accumulate bootstrap results for all veg
  if (veg.num == 1) {
    boot.q.all <- boot.q.es
  } else {
    boot.q.all <- rbind(boot.q.all, boot.q.es)
  }

  veg.num <- veg.num + 1
} ### END veg loop

# Save the accumulated bootstrap file
if (save.boot == TRUE) {
  write.csv(boot.q.all, file = boot.file, row.names = FALSE)
}

end.time <- proc.time() - start.time
# Use element 3 for elapsed ("clock") time
elapsed <- format(.POSIXct(end.time[[3]]), "%H:%M:%OS2")
cat(br, "Run completed. Time elapsed:", elapsed)
cat(paste0(br, h.line, br))

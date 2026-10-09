###############################################
# Calculates overall quality and 95% conf limits for two equal
# size samples drawn randomly from benchmark distribution
# only. Does this multiple times, as set by parater "runs".
# In theory should have 100% overlap each time.
# This version also saves file of individual ei qualities
# and CLs as well
###############################################

#######################
# Parameters
#######################

# Data frame of landcover classes to process
df.veg <- data.frame(veg = c(
  "Alpine",
  "Alpine dwarf shrub",
  "Avalanche feature",
  "Brushland/Grassland",
  "Dry forest, early-mid",
  "Dry forest, mature",
  "Dry forest, old",
  "Intermediate forest, early-mid",
  "Intermediate forest, mature",
  "Intermediate forest, old",
  "Krummholz",
  "Rock/Talus",
  "Wet forest, early-mid",
  "Wet forest, mature",
  "Wet forest, old",
  "Wetland"
))

df.veg <- data.frame(veg = c(
  "Dry forest, early-mid"
))

# Ecological ind (abbreviations) to process
df.ei <- data.frame(ei = c(
  "GC",
  "PCESS",
  "PCGF",
  "SR",
  "TD"
))

# Overall quality algorithm
# q.overall.method <- "simple arith mean"	# Simple arith avg of ei qualities
q.overall.method <- "3 cat geom mean" # Arith avg of ei qualities within categories,
# then geom mean of category mean

##############################
# Load global parameters
# May be reset in the next section
# if desired
##############################

wd <- getwd()
setwd(wd)

# load parameters
source("params.R", local = TRUE)

# Load functions
source(paste(INCLUDESDIR, "functions.R", sep = ""), local = TRUE)

##############################
# Reset global parameters
# Comment out to use defaults
##############################

do.set.seed <- FALSE # Set randomization seed to fix results?
seed <- 123 # Seed value. Only used if do.set.seed ==T
boot.reps <- 10 # Number of bootstrap replications
perm.reps <- 100 # Number of reps for permutation means test (q.method=fixed only)

##############################
# Local parameters
##############################

reps <- 5 # Number of simulated calculations of quality to run
# using random re-draws of same data

echo.reps <- TRUE # TRUE=echo replicate	to terminal

# Use permutation test of significant difference for means test (slower)?
test.diff.obs <- TRUE # Observed quality
test.diff.boot <- TRUE # Bootstrap quality

# Simulation method
# sim.method <- "split"					# Split bm sample randomly in two
# sim.method <- "identical"			# Assign full bm sample to f and b
# sim.method <- "subsample.nr"		# Two random subsamples of identical size
# w/o replacement. MUST specify subsample.n.prop
# if choose this option
# sim.method<-"subsample.nr.range" # Randomly sample f from b over range values
# sim.method<-"subsample.nr.range.repl" # Randomly sample f from b over range values, with replacement
sim.method <- "subsample.nr.range.n.bm.fixed" # Randomly sample f from b over range values
# f with replacement, b fixed at actual sample


subsample.n.prop <- 0.5 # Proportion of b to sample if using option sim.method=
# "subsample.nr". Should be <<1.0

# Set quality to 100% for indicators whose upper 95% CLs overlap 1?
# Must also set threshold effect size
adjust.q <- TRUE
e.size.max <- 0.05 # Only adjust q if effect size (=largest CL) at or below this value
# CI will be as much as double this value

#########################
# Files & paths and save options
#########################

# Overall quality file
# q.sims.file <- "results/bm.sim/bm.sims.csv"
q.sims.file <- "results/bias/bias.sims.csv"

# Indicator quality file
# q.ei.sims.file <- "results/bm.sim/bm.sims.ei.csv"
q.ei.sims.file <- "results/bias/bias.sims.ei.csv"

# Append results or replace results files
# FALSE =replace original files
# TRUE=append to existing files
append.results <- TRUE

#######################
# Prepare remaining settings
#######################

# Set working directory
# Must set first to set directory parameters
wd <- getwd()
setwd(wd)
wd

if (do.set.seed == T) set.seed(seed) # for repeatability

# Message if distribution unknown or not supported
msg_baddist <- "ERROR: Operation not supported for this distribution!"

#################################
# Names and locations of input files
#################################

# Raw data input files (in inputs/ folder)
metadata.file <- "focal_summary.txt" #  land cover metadata
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

##############################################
# Import land cover metadata
# Needed for blacklisting/whitelisting EIs and grouping into
# categories for overall quality algorithm
##############################################

# Import site-level metadata
# Used to associate focal with benchmark vegetation
print(paste("Importing  '", metadata.file, "'", sep = ""))
input.file <- paste(INPUTDIR, metadata.file, sep = "")
rawData <- read.table(input.file, header = T, sep = "\t")
landcover <- unique(rawData[, c("landCoverCode", "targetLandCoverCode", "area_ha")])
colnames(landcover) <- c("landcover", "bm.vegetation", "area_ha")

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
all.ind.list <- all.ind.list[, c("EI", "stratum", "ei.strat", "category", "include")]
blacklist <- all.ind.list[all.ind.list$include == FALSE, ]
ind.list <- all.ind.list[all.ind.list$include == TRUE, ]

print(paste("Importing  '", whitelist.file, "'", sep = ""))
input.file <- paste(INPUTDIR, whitelist.file, sep = "")
whitelist <- read.csv(input.file, header = T)
whitelist <- whitelist[!is.na(whitelist$include), ]
whitelist <- whitelist[, c("EI", "stratum", "bm.vegetation", "include")]

for (v in 1:nrow(df.veg)) { ### Begin veg loop
  #### v=1 # For testing without veg loop
  curr.veg <- toString(df.veg[v, c("veg")])

  # Starting message
  br <- "\n"
  h.line <- "------------------------------------------------"
  main.title <- "VQA Benchmark Subsampling Simulation"
  veg.title <- curr.veg
  ind.title <- unique(all.ind.list$ei.strat[all.ind.list$include == TRUE])

  cat(paste0(h.line, br, main.title, br))
  cat(paste0("Vegetation: ", veg.title, br, "Indicators: "))
  for (ind in ind.title) cat(paste0("  ", ind, br))

  ######################################
  # Import raw data and merge into single data frame
  ######################################

  n.EI <- nrow(df.ei)

  # Loop through all EI files, import and merge
  for (i in 1:n.EI) {
    curr.ei <- df.ei[i, 1]
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

    if (i == 1) {
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

  # Get raw data for current land cover class
  dat <- dat.raw[dat.raw$landcover == curr.veg, ]

  # Count number of distinct ei+stratum classes ("ei.full")
  ei.full <- unique(paste(dat$landcover, dat$stratum, dat$EI, sep = "+"))
  n.classes <- length(ei.full)

  ei.all <- with(dat, aggregate(ei.val, list(EI = EI, landcover = landcover, stratum = stratum, category = category, focalOrBenchmark = focalOrBenchmark),
    FUN = function(x) {
      c(n = length(x))
    }
  ))
  names(ei.all)[names(ei.all) == "x"] <- "n"

  # Reorder columns and sort
  col_idx <- grep("landcover", names(ei.all))
  ei.all <- ei.all[, c(col_idx, (1:ncol(ei.all))[-col_idx])]
  ei.all <- ei.all[with(ei.all, order(landcover, EI, stratum, focalOrBenchmark)), ]

  n.max <- max(ei.all[ei.all$focalOrBenchmark == "b", c("n")])
  n.min <- min(ei.all[ei.all$focalOrBenchmark == "b", c("n")])

  ns <- seq(from = 3, to = n.max, by = 1)

  # Restructure ei.all to make it a bit easier to use
  cols <- c("EI", "stratum")
  ei.all$ei.full <- apply(ei.all[, cols], 1, paste, collapse = "-")
  ei.all <- ei.all[ei.all$focalOrBenchmark == "b", c("ei.full", "category", "EI", "n")]
  names(ei.all)[names(ei.all) == "n"] <- "n.b"

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
  h.line <- "------------------------------------------------"
  main.title <- "VQA Benchmark Subsampling Simulation"
  veg.msg <- paste0("Landcover: ", curr.veg)

  # Echo starting message
  cat(paste0(h.line, br, main.title, br, br, veg.msg, br, h.line, br))

  #################################
  # Begin replicates
  #################################

  # Get maximum possible bm sample size per ei for this veg
  max.n <- max(table(dat[dat$focalOrBenchmark == "b", c("ei.full")]))

  # Prepare data frame for overall quality results
  df.sims <- data.frame(
    method = character(0),
    landcover = character(0),
    boot.reps = character(0),
    reps = character(0),
    n.b.actual = character(0),
    n.b = character(0),
    n = character(0),
    rep = character(0),
    q = character(0),
    q.lcl = character(0),
    q.ucl = character(0),
    e.size = character(0),
    diff.min = character(0),
    sig = character(0)
  )

  # Prepare data frame for ei quality results
  df.ei.sims <- data.frame(
    method = character(0),
    landcover = character(0),
    ei = character(0),
    boot.reps = character(0),
    reps = character(0),
    n.b.actual = character(0),
    n.b = character(0),
    n = character(0),
    rep = character(0),
    q = character(0),
    q.lcl = character(0),
    q.ucl = character(0),
    q.ei.adj = character(0),
    e.size = character(0),
    diff.min = character(0),
    sig = character(0)
  )

  start.time <- proc.time()
  start <- format(.POSIXct(start.time[[1]]), "%H:%M:%OS2")
  cat("Start time:", start, "\n")

  # Replicates loop
  for (rep in 1:reps) { # START rep loop
    ### for (rep in 10:10) {			# For testing with loop
    ### rep <- 1 							# For testing without loop

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
    # possible number of samples (max.n)
    f.samps <- matrix(NA,
      nrow = max.n,
      ncol = n.ei
    )
    b.samps <- matrix(NA,
      nrow = max.n,
      ncol = n.ei
    )

    # EI quality loop
    # Calculate and save quality of individual EIs
    for (j in 1:n.ei) { # START ei loop
      ### for ( j in 4:4 ) {		# For testing single value with loop
      ### j <- 1						# For testing without loop

      # Get attributes of current EI
      curr.ei.full <- ei.full.all[j]
      curr.ei <- ei.all[ei.all$ei.full == curr.ei.full, c("EI")]
      distn <- df.ei[df.ei$ei == curr.ei, c("dist")]
      test.tail <- df.ei[df.ei$ei == curr.ei, c("test.tail")]
      q.method <- df.ei[df.ei$ei == curr.ei, c("q.method")]

      # Get actual samples for this indicator
      # focal comes from benchmark as well!
      b <- dat[dat$ei.full == curr.ei.full & dat$focalOrBenchmark == "b" & !is.na(dat$ei.val), c("ei.val")]
      n.b.actual <- length(b)

      # Pull the simulation samples using the chosen method
      if (sim.method == "split") {
        # reorder the vector randomly
        b.rand <- sample(b)

        # Get subsample size, n, and set for benchmark vector b and pseudo-focal vector b
        n <- as.integer(length(b) / 2)
        n.f <- n
        n.b <- n

        # split into chunks of size n and assign first two chunks to f and b
        b.split <- split(b.rand, ceiling(seq_along(b.rand) / n))
        f.samp <- b.split$`1`
        b.samp <- b.split$`2`
      } else if (sim.method == "identical") {
        # f.samp & b.samp are identical copies of the original b sample
        n <- length(b)
        n.f <- n
        n.b <- n

        f.samp <- b
        b.samp <- b
      } else if (sim.method == "subsample.nr") {
        n <- as.integer(length(b) * subsample.n.prop)
        n.f <- n
        n.b <- n

        f.samp <- sample(b, n, replace = F)
        b.samp <- sample(b, n, replace = F)
      } else if (sim.method == "subsample.nr.range") {
        if (reps >= n.b.actual) stop("ERROR: reps too large for this method, choose smaller value")
        # if (rep==1) {
        increment <- as.integer(n.b.actual / reps)
        if (increment <= 2) stop("ERROR: reps too large for this method, choose smaller value")
        # }

        # Get current value of n
        n <- increment + ((rep - 1) * increment)

        n.f <- n
        n.b <- n

        f.samp <- sample(b, n, replace = F)
        b.samp <- sample(b, n, replace = F)
      } else if (sim.method == "subsample.nr.range.repl") {
        if (reps >= n.b.actual) stop("ERROR: reps too large for this method, choose smaller value")
        # if (rep==1) {
        increment <- as.integer(n.b.actual / reps)
        if (increment <= 2) stop("ERROR: reps too large for this method, choose smaller value")
        # }

        # Get current value of n
        n <- increment + ((rep - 1) * increment)

        n.f <- n
        n.b <- n

        f.samp <- sample(b, n, replace = T)
        b.samp <- sample(b, n, replace = T)
      } else if (sim.method == "subsample.nr.range.n.bm.fixed") {
        if (reps >= n.b.actual) stop("ERROR: reps too large for this method, choose smaller value")
        # if (rep==1) {
        increment <- as.integer(n.b.actual / reps)
        if (increment <= 2) stop("ERROR: reps too large for this method, choose smaller value")
        # }

        # Get current value of n
        n <- increment + ((rep - 1) * increment)

        n.f <- n
        n.b <- n.b.actual

        f.samp <- sample(b, n, replace = T)
        b.samp <- b
      } else {
        stop(paste0("ERROR: wrong or missing method: '", sim.method, "'"))
      }

      # Save the subsamples
      f.samps[1:length(f.samp), j] <- f.samp
      b.samps[1:length(b.samp), j] <- b.samp

      # Determine domain of x for modeling pdf
      # Use domain of actual data, not subsamples
      if (distn == "Bet") {
        lowlim <- 0.0000001
        shape1 <- 0.1
        shape2 <- 0.1
        xs <- seq(0, 1, 100)
      } else if (distn == "NBin") {
        xs.max <- ceiling(max(b) + (0.05 * (max(b) - min(b))))
        xs <- seq(0, xs.max + 10)
      } else if (distn == "gamma") {
        lowlim <- 0.0000001
        uplim <- max(b) + (0.1 * max(b))
        shape <- 1
        rate <- 1
        xs.max <- max(b) + (0.05 * (max(b) - min(b)))
        xs <- seq(0, xs.max, length.out = 100)
      } else {
        stop(paste0(msg_baddist, "(dist=')", distn, "')"))
      }

      # Get observed quality for current sample
      if (distn == "NBin" || distn == "gamma") {
        # qual <- q.overlap.no.cls(distn, test.tail= test.tail, f=f.samp, b=b.samp, xs=xs)
        qual <- boot.overlap.diff(distn,
          test.tail = test.tail, f = f.samp, b = b.samp, xs = xs,
          boot.reps = boot.reps, seed = seed, set.seed = set.seed
        )
        q.ei <- qual$ol.obs
        q.ei.lcl <- qual$ol.lcl
        q.ei.ucl <- qual$ol.ucl
      } else if (distn == "Bet") {
        if (q.method == "fixed") { # Fixed benchmark method
          bm.val <- df.ei[df.ei$ei == curr.ei, c("bm.val")]
          qual <- boot.qual.beta.fixed(
            f = f.samp, b = bm.val, boot.reps = boot.reps,
            perm.reps = perm.reps
          )
        } else { # Empirical overlap-based method
          qual <- boot.qual.beta(
            f = f.samp, b = b.samp, xs = xs, boot.reps = boot.reps,
            seed = seed, set.seed = set.seed, shape1 = shape1, shape2 = shape2,
            lowlim = lowlim
          )
        }
        q.ei <- qual$q
        q.ei.ucl <- qual$q.ucl
        q.ei.lcl <- qual$q.lcl
      } else {
        stop(paste0(msg_baddist, "(dist=')", distn, "')"))
      }

      # Effect size & minimum difference from 100% quality
      e.size <- max(q.ei - q.ei.lcl, q.ei.ucl - q.ei)
      sig <- as.numeric(q.ei.ucl) < 1
      diff.min <- max(0, 1 - as.numeric(q.ei.ucl))

      if (adjust.q == TRUE && q.ei.ucl >= 1 && e.size <= e.size.max) {
        # Set q to 1 if upper CL overlaps 1 and effect size is at or below
        # threshold sensitivity (MDES, min. detectable effect size)
        q.ei.adj <- 1
      } else {
        q.ei.adj <- q.ei
      }

      # Store quality for current EI for later
      # calculation of overall quality
      q.eis[j] <- q.ei.adj

      #################################
      # Save the results of the current ei for this
      # replicate
      #################################

      # Append individual replicate to replicates data frame
      df.ei.sims <- rbind(df.ei.sims, as.character(c(
        sim.method,
        curr.veg,
        curr.ei.full,
        boot.reps,
        reps,
        n.b.actual,
        n.b,
        n.f,
        rep,
        q.ei,
        q.ei.lcl,
        q.ei.ucl,
        q.ei.adj,
        e.size,
        diff.min,
        sig
      )))

      # Reset df to character if this is the first round
      # Stupid R automatically converts to factors when
      # appending to empty data frame
      if (rep == 1) df.ei.sims[] <- lapply(df.ei.sims, as.character)

      # Name columns again because stupid R loses names when append
      # to empty data frame
      colnames(df.ei.sims) <- c(
        "method", "veg", "ei", "boot.reps", "reps", "n.b.actual", "n.b", "n.f", "rep", "q", "q.lcl", "q.ucl", "q.adj", "e.size", "diff.min", "sig.diff"
      )
      # write.csv(df.ei.sims, file=q.ei.sims.file, row.names=FALSE)

      #######################
      # Save overal quality results to file
      #######################

      if (file.exists(q.ei.sims.file) && append.results == TRUE) {
        # Open existing results file if it exists and append new results
        dat.prev <- read.csv(q.ei.sims.file, header = T, stringsAsFactors = FALSE)

        df.ei.sims.all <- rbind(dat.prev, df.ei.sims)
      } else {
        df.ei.sims.all <- df.ei.sims
      }

      write.csv(df.ei.sims.all, file = q.ei.sims.file, row.names = FALSE)
    } # END ei loop

    ###################################
    # Overall quality
    ###################################

    if (q.overall.method == "simple arith mean") {
      q <- mean(q.eis)
    } else if (q.overall.method == "3 cat geom mean") {
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

    ###################################
    # Bootstrap CLs of overall quality
    ###################################

    # Generate bootstrapped quality of current samples
    boot.q <- rep(NA, boot.reps)

    for (j in 1:boot.reps) { # START bootstrap loop
      ### j <- 1 # for testing without loop

      # Calculate individual EI qualities
      ### for ( i in 1:n.ei ) {		# START ei loop
      ### for ( i in 4:4 ) {		# For testing single value with loop
      i <- 1 # For testing without loop

      # Get attributes of current EI
      curr.ei.full <- ei.full.all[i]
      curr.ei <- ei.all[ei.all$ei.full == curr.ei.full, c("EI")]
      distn <- df.ei[df.ei$ei == curr.ei, c("dist")]
      test.tail <- df.ei[df.ei$ei == curr.ei, c("test.tail")]
      q.method <- df.ei[df.ei$ei == curr.ei, c("q.method")]

      # Get actual samples for this indicator
      # Subsitute b for f!
      f <- dat[dat$ei.full == curr.ei.full & dat$focalOrBenchmark == "b", c("ei.val")]
      b <- dat[dat$ei.full == curr.ei.full & dat$focalOrBenchmark == "b", c("ei.val")]

      # Reuse the saved subsamples
      f.samp <- f.samps[which(!(is.na(f.samps[i, ]))), i]
      b.samp <- b.samps[which(!(is.na(b.samps[i, ]))), i]

      # Take random sample of the save subsample
      boot.samp.f <- sample(f.samp, n, replace = T)
      boot.samp.b <- sample(b.samp, n, replace = T)

      # Determine domain of x for modeling pdf
      # Use domain of actual data, not subsamples
      if (distn == "Bet") {
        xs <- seq(0, 1, length.out = 50)
      } else if (distn == "NBin") {
        xs.max <- ceiling(max(b) + (0.05 * (max(b) - min(b))))
        xs <- seq(0, xs.max + 10)
      } else if (distn == "gamma") {
        xs.max <- max(b) + (0.05 * (max(b) - min(b)))
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

      q.eis[i] <- q.ei
      # print(paste0("  q(", curr.ei.full, ")=", q.ei))
      # flush.console()

      ### }		# END ei loop

      ################################
      # Overall quality for this bootstrap run
      ################################

      if (q.overall.method == "simple arith mean") {
        boot.q.curr <- mean(q.eis)
      } else if (q.overall.method == "3 cat geom mean") {
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

    ########################################
    # Significance test: bootstrap upper 95% CL < q.crit
    ########################################

    boot.q.mean <- mean(boot.q, na.rm = T) # mean bootstrap quality
    boot.q.dev <- boot.q - boot.q.mean # vector of bootstrap deviances

    # Upper and lower 95% CLs of bootstrap deviance
    boot.q.dev.cls <- quantile(boot.q.dev, c(0.025, 0.975), na.rm = T)

    # Upper and lower 95% CLs of bootstrap quality
    boot.q.cls <- q + boot.q.dev.cls
    q.lcl <- boot.q.cls[1]
    q.ucl <- boot.q.cls[2]

    # Effect size & minimum difference from 100% quality
    e.size <- max(q - q.lcl, q.ucl - q)
    sig <- as.numeric(q.ucl) < 1
    diff.min <- max(0, 1 - as.numeric(q.ucl))

    #################################
    # Save the results of the current replicate
    #################################

    # Append individual replicate to replicates data frame
    df.sims <- rbind(df.sims, as.character(c(
      sim.method,
      curr.veg,
      boot.reps,
      reps,
      n.b.actual,
      n.b,
      n.f,
      rep,
      q,
      q.lcl,
      q.ucl,
      e.size,
      diff.min,
      sig
    )))

    # Reset df to character if this is the first round
    # Stupid R automatically converts to factors when
    # appending to empty data frame
    if (rep == 1) df.sims[] <- lapply(df.sims, as.character)

    ########################
    # Echo results for this rep
    ########################

    q.disp <- specify_decimal(q, 2)
    q.lcl.disp <- specify_decimal(q.lcl, 2)
    q.ucl.disp <- specify_decimal(q.ucl, 2)
    e.size.disp <- specify_decimal(e.size, 2)
    diff.min.disp <- specify_decimal(diff.min, 2)

    if (echo.reps == TRUE) {
      cat(paste0(
        "    ",
        "method=", sim.method,
        " veg=", curr.veg,
        " n.b.actual=", n.b.actual,
        " n.b=", n.b,
        " n.f=", n.f,
        ", rep=", rep,
        ", q=", q.disp,
        ", q.lcl=", q.lcl.disp,
        ", q.ucl=", q.ucl.disp,
        ", e.size =", e.size.disp,
        ", diff.min =", diff.min.disp,
        ", sig=", sig,
        "\n"
      ))
      flush.console()
    }
  } # End replicate (rep) loop

  ###################################
  # Save the individual replicate results
  ###################################

  # Name columns again because stupid R loses names when append
  # to empty data frame
  colnames(df.sims) <- c(
    "method", "veg", "boot.reps", "reps", "n.b.actual", "n.b", "n.f", "rep", "q", "q.lcl", "q.ucl", "e.size", "diff.min", "sig.diff"
  )
  # write.csv(df.sims, file=q.sims.file, row.names=FALSE)

  #######################
  # Save overal quality results to file
  #######################

  if (file.exists(q.sims.file) && append.results == TRUE) {
    # Open existing results file if it exists and append new results
    dat.prev <- read.csv(q.sims.file, header = T, stringsAsFactors = FALSE)

    df.sims.all <- rbind(dat.prev, df.sims)
  } else {
    df.sims.all <- df.sims
  }

  write.csv(df.sims.all, file = q.sims.file, row.names = FALSE)
} ### END veg loop

end.time <- proc.time() - start.time
elapsed <- format(.POSIXct(end.time[[1]]), "%H:%M:%OS2")
cat("Run completed. Time elapsed:", elapsed)

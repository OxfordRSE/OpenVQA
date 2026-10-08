######################################################
# VQA posthoc power analysis, single veg, EI, & stratum, at multiple
# effect sizes
#
# Author: Brad Boyle (bboyle@email.arizona.edu)
# First release: 20 Jul 2017
#
# Calculates power versus sample size, over a range of effect sizes,
# for detecting change in quality of a single ecological indicator (EI).
# Significance assessed using the upper and lower (asymmetrical)
# bootstrapped 95% confidence limits (CLs). Values of quality outside
# the CLs are significant. Minimum detectable ES is the smallest of the
# two 95% margins of error (ME), where ME is the difference between
# the upper or lower CL and the mean.
# For EI with multiple strata, only single stratum (set as parameter)
# analyzed.
# NOTE: will run multiple effect sizes sequentially if mutliple es  values
# 	provided as vector, appending all to file
# WARNING: Graph feature only prints result for single ei. Be sure to submit
# only single es, not vector of es values, if using this script to produce
# graph. If use vector, all es results will be (falsely) combined in single
# graph as single ei.
######################################################

# Set working directory
wd <- getwd()
setwd(wd)
wd

#######################
# Load libraries, functions,
# external parameters & paths
######################

# Load required packages
library(drc) # For fitting Michaelis-Menten curve
library(ggplot2)

###################################
# Parameter options
# Select from these lists
# Not actually set; that comes in next section
###################################

# Critical!
# Determines data directories and params file used
# Params file used=paste0("params.", proj, ".R")
# Assessment code and everything else are set in the params file
proj <- "vqa-pub"

# Vegetation you wish to plot
curr.veg <- "Alpine"
curr.veg <- "Alpine dwarf shrub"
curr.veg <- "Avalanche feature"
curr.veg <- "Brushland/Grassland"
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

# proj=="vqa-pub", assess=="baseline"
curr.veg <- "Intermediate forest, Early-mid"
curr.veg <- "Avalanche feature"

# Ecological indicator
ei <- "asc"
ei <- "ba"
ei <- "mccsc"
ei <- "mpck"
ei <- "pbi"
ei <- "pbst"
ei <- "pcnwbp"
ei <- "pinf"
ei <- "sa"
ei <- "GC"
ei <- "PCESS"
ei <- "SR"

# Stratum (set to empty string if this EI has no strata)
# ei <- 'asc'	:
curr.stratum <- "Mature Trees"
curr.stratum <- "Medium Trees"
curr.stratum <- "Saplings"
curr.stratum <- "Small Trees"

# ei <- 'mccsc':
curr.stratum <- "Trees >=20 in DBH"
curr.stratum <- "Trees 10-15 in DBH"
curr.stratum <- "Trees 15-20 in DBH"
curr.stratum <- "Trees 5-10 in DBH"

# ei <- 'pcnwbp':
curr.stratum <- "Shrubs"
curr.stratum <- "Trees"

# ei <- "GC"
curr.stratum <- "SubstrateOrganicMatter"
curr.stratum <- "SubstrateDecWood"

# ei <- "PCESS"
curr.stratum <- "Herb"

# ei <- "PCGF"
curr.stratum <- "D_Soil"
curr.stratum <- "Dominant_Trees"
curr.stratum <- "Dw_Wood" # No variance; don't bother
curr.stratum <- "Epiphyte" # Almost no variance; don't bother
curr.stratum <- "Herb_Dwarf"
curr.stratum <- "Low_Shrub"
curr.stratum <- "Main_Canopy"
curr.stratum <- "Subcanopy_Trees"
curr.stratum <- "Tall_Shrub"

# ei <- "SR"
# ei <- 'TD'
curr.stratum <- ""

######################################
# Load global parameters
######################################

params.file <- paste0("params.", proj, ".R")
cat("Loading parameters file '", params.file, "'...", sep = "")
if (file.copy(from = params.file, to = "params.R", overwrite = TRUE) == FALSE) {
  stop("ERROR: file copy failed!\n")
}
cat("done\n\n")

# load parameters
source(params.file, local = TRUE)

# load functions
source(paste(INCLUDESDIR, "functions.R", sep = ""), local = TRUE)

# Create power figures directory if missing
dir.create(file.path(FIGDIR, "power"), showWarnings = F)
pwr.figs.dir <- paste0(FIGDIR, "power/")

# Create power results directory if missing
dir.create(file.path(RESULTSDIR, "power"), showWarnings = F)
pwr.results.dir <- paste0(RESULTSDIR, "power/")

#####################################
# Set local parameters
# Over-ride any global parameters of same name
#####################################
curr.veg <- "Avalanche feature"
curr.veg <- "Wet forest, Old"
curr.veg <- "Brushland/Grassland"

ei <- "SR"
curr.stratum <- ""

# Set exact zeros to 1 (Nbin) or small value >0 (all other distns)
adjust.zeros <- TRUE

# Make unix-friendly versions of veg, ei and stratum, for file names
veg.uf <- unix.friendly(curr.veg)
ei.uf <- unix.friendly(ei)
stratum.uf <- ""
if (trim(curr.stratum) != "") stratum.uf <- paste0(".", unix.friendly(curr.stratum))

# One tailed effect sizes to test
# Array, recommend no more than 3, otherwise takes too long and
# graph is too busy.
# ES determines critical value:
# 		q.crit.upper = q.mean + e.size,
# 		q.crit.lower = q.mean - e.size
# where q.mean is mean quality.
# A significant diff is found when:
# 		q.obs > q.crit.upper --OR--
#  	q.obs < q.crit.lower
# where q.obs = observed quality.
# Note that q.mean and q.obs distributed over [0:1].
# e.sizes <- c(0.1)
e.sizes <- c(0.2)
e.sizes <- c(0.15, 0.2)
e.sizes <- c(0.1, 0.15, 0.2)

# Target power for predicting minimum sample size
# Recommend conventional power of 0.80
pwr.target <- 0.8

# Get remaining parameters for this ei
ei.pars <- ei.params(ei) # Get attributes of this EI (global function)
distn <- ei.pars$distn # Distribution of the EI
test.tail <- ei.pars$test.tail # Test tail for this EI
has.strata <- ei.pars$has.stratum # True if this EI has strata, otherwise F
if (has.strata == F) curr.stratum <- ""
q.method <- ei.pars$q.method
bm.val <- ei.pars$bm.val
ei.name <- ei.pars$ei.name

# Seed for bootstrap repeatability
# Not currently implemented, would need to be passed as
# parameter to bootstrap function and implemented inside function
# NOTE: it matters where you place set.seed!
# See: https://stackoverflow.com/a/64236620/2757825
do.set.seed <- FALSE # Set randomization seed to fix results?
seed <- 123 # Seed value. Only used if do.set.seed ==T

# Main model parameters
save.data <- T # Save data to file, appending to existing file if exists
replace.data <- T # Replace existing records for this ei+veg+stratum+e.size
save.fig <- FALSE # Save figure
replace.fig <- T # Replace existing figure for this ei+veg+stratum+e.size
boot.reps <- 100 # Number of bootstrap replications
# Keep low for testing; blows up run time
sims <- 100 # No. of simulations (assessments of significance by
# comparing Q.crit.lower & q.crit.upper to Q.ucl).
# Power calculated as:  sig. sims / total sims
# Ideally, should be >=1000, but this will take
# many hours
# Keep low for testing
n.start <- 3 # Minimum sample size
n.cap <- "n.final" # Cap max value of n at some value lower than n.final
# Options: "n.obs", "n.max", "n.min", "n.final"
# Option n.obs caps separate sample sizes for each vector
# separately at the observed sample size for that vector
n.final <- 57 # Final (maximum) sample size. Only used if
# n.cap=="n.final"
n.step <- 3 # Units of n to loop over (default=1)
reduce.observed <- T # Rarify observed samples? If F, only bootstrap samples
# are reduced & observed curves stay constant.
plot.results <- FALSE # Set to false to turn off graph section
fit.method <- "logistic" # Method for fitting power curve
# Options: "logistic", "mm" (=Michaelis Menten)

# The simulations file
# Individual runs are appended to this file
# sims.file <- paste0("results/power/power.ei.sims.", veg.uf, ".", ei.uf, stratum.uf, ".csv")
sims.file.name <- "power.single.ei.sims.csv"
sims.file <- paste0(pwr.results.dir, sims.file.name)

# The final results file
# Individual experiments are appended to this file
# (power calculations based on multiple simulations)
# results.file <- "results/power/power.single.ei.results.csv"
# results.file <- paste0("results/power/power.ei.", veg.uf, ".", ei.uf, stratum.uf, ".csv")
results.file.name <- paste0("power.ei.", veg.uf, ".", ei.uf, stratum.uf, ".csv")
results.file <- paste0(pwr.results.dir, results.file.name)

#################################
# Confirm operation
#################################

# Starting message
br <- "\n"
h.line <- "------------------------------------------------"
main.title <- "VQA Indicator Power Simulation"
curr.veg

cat(paste0(h.line, br, main.title, br, br))
cat("Settings:\n")
cat("  Project='", proj, "'\n", sep = "")
cat("  Assessment='", assess, "'\n", sep = "")
cat(paste0("Vegetation: ", curr.veg, br))
cat(paste0("Indicator: ", ei, br))
cat(paste0("Stratum: ", curr.stratum, br))

cat("Settings:\n")
cat("  boot.reps: ", boot.reps, "\n")
cat("  sims: ", sims, "\n")
cat("  n.start: ", n.start, "\n")
cat("  n.final : ", n.final, "\n")
cat("  n.cap: ", n.cap, "\n")
cat("  n.step: ", n.step, "\n")
cat("  reduce.observed: ", reduce.observed, "\n")
cat("\n")

if (interactive() == FALSE) {
  yes <- c("y", "Y", "Yes", "yes")
  cat("Continue? (y/n):")
  response <- readLines("stdin", n = 1)
  if (!response %in% yes) stop_quietly("Operation cancelled\n\n")
}

#################################
# Load the data
#################################

# Load raw data and order by stratum
rawFileName <- paste0(ei, "_raw.csv")
rawFile <- paste0(RESULTSDIR, rawFileName)
dat.raw <- read.csv(rawFile, header = T, stringsAsFactors = FALSE)

if (has.strata) {
  dat <- dat.raw[dat.raw$vegClass == curr.veg & dat.raw$stratum == curr.stratum, ]
} else {
  dat <- dat.raw[dat.raw$vegClass == curr.veg, ]
}

b <- dat[dat$focalOrBenchmark == "b", c("EI")]
f <- dat[dat$focalOrBenchmark == "f", c("EI")]

# Adjust for zeros
if (adjust.zeros == TRUE) {
  if (distn == "NBin") {
    f[f == 0] <- 1
  } else {
    f[f == 0] <- 0.0001
  }
}

# Determine domain of x for modeling pdf
# Use domain of actual data, not subsamples
# xs <- seq(0,max(max(f), max(b)) + 10)
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

# Rarified power curve tests power of equal sample sizes up to
# the minimum of the two sample size
n.min <- min(length(f), length(b))
n.max <- max(length(f), length(b))

if (n.cap == "n.min") {
  n.finish <- min(n.min, n.final)
} else if (n.cap == "n.max") {
  n.finish <- n.max
} else if (n.cap == "n.obs") {
  n.finish <- min(n.max, n.final)
} else {
  n.finish <- n.final
}

ns <- seq(from = 3, to = n.max, by = 1)

# Runtime message settings
br <- "\n"
h.line <- "------------------------------------------------"
main.title <- "VQA Power Simulation"
ei.msg <- paste0("Indicator: ", ei)
veg.msg <- paste0("Landcover: ", curr.veg)
stratum.msg <- ""
if (has.strata) stratum.msg <- paste0(br, "Stratum: ", curr.stratum)

# Echo starting message
cat(paste0(h.line, br, main.title, br, br, ei.msg, br, veg.msg, stratum.msg, br, h.line, br))

#################################
# Begin simulations
#################################

# # Get observed quality
# if (distn=="NBin" || distn=="gamma" ) {
# qual <- q.overlap.no.cls(distn, test.tail= test.tail, f=f, b=b, xs=xs)
# q.obs <- qual$ol.obs			# overlap used directly as quality
# } else {
# stop(paste0("ERROR: Operation not supported for distribution=", distn))
# }

# Set total rows for results matrix
tot.rows <- n.finish * length(e.sizes)

# prepare the results matrix
results <- matrix(NA,
  nrow = tot.rows,
  ncol = sims
)

# Prepare data frame for inidividual simulation results
# df.sims <- data.frame(
# landcover=character(0),
# ei=character(0),
# stratum=character(0),
# e.size= character(0),
# boot.reps= character(0),
# sims= character(0),
# n= character(0),
# q.obs= character(0),
# q.ucl= character(0),
# q.lcl=character(0),
# q.crit= character(0),
# q.crit.lower =character(0),
# q.crit.upper =character(0),
# sig= character(0)
# )
df.sims <- data.frame(
  landcover = character(0),
  ei = character(0),
  stratum = character(0),
  e.size = character(0),
  boot.reps = character(0),
  sims = character(0),
  n = character(0),
  q.obs = character(0),
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

for (es in 1:length(e.sizes)) { # Start effect size loop
  # Set current effect size & echo
  e.size <- e.sizes[es]
  cat(paste0("\n", "Effect size: ", e.size, "\n"))

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

  # prepare the adjust (actual) effect sizes matrix
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

  for (n in seq(n.start, n.finish, by = n.step)) { # Start sample size loop (increasing)
    # for (n in n.start : n.finish ) {		# Start sample size loop (increasing)
    # for (n in n.finish : n.start ) {		# Start sample size loop (decreasing)

    # Set vector sample sizes
    if (n.cap == "n.obs") {
      n.f <- min(n, length(f))
      n.b <- min(n, length(b))
    } else {
      n.f <- n
      n.b <- n
    }

    # Run multiple simulations for this sample size
    for (sim in 1:sims) {
      # Get actual samples for this sample size (n)
      f.samp <- f
      b.samp <- b

      # reduce actual samples if requested
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

      # Get observed quality for current sample
      if (distn == "NBin" || distn == "gamma") {
        qual <- q.overlap.no.cls(distn, test.tail = test.tail, f = f.samp, b = b.samp, xs = xs)
        # Observed quality (=overlap)
        q <- qual$ol.obs
      } else if (distn == "Bet") {
        if (q.method == "fixed") { # Fixed benchmark method
          qual <- qual.beta.fixed(f = f.samp, b.fixed = bm.val, test.diff = FALSE)
        } else { # Empirical overlap-based method
          qual <- qual.beta(f = f.samp, b = b.samp, xs = xs, test.diff = FALSE)
        }
        q <- qual$q
      } else {
        stop(paste0(msg_baddist, "(dist=')", distn, "')"))
      }
      q.crit.lower <- q - e.size
      q.crit.upper <- q + e.size
      q.crit.adj <- min(1, q.crit.upper)

      # Generate bootstrapped quality of current samples
      boot.q <- rep(NA, boot.reps)
      for (i in 1:boot.reps) {
        boot.samp.f <- sample(f.samp, n.f, replace = T)
        boot.samp.b <- sample(b.samp, n.b, replace = T)

        if (distn == "NBin" || distn == "gamma") {
          qual <- q.overlap.no.cls(distn, test.tail = test.tail, f = boot.samp.f, b = boot.samp.b, xs = xs)
          boot.q.curr <- qual$ol.obs # overlap used directly as quality
        } else if (distn == "Bet") {
          if (q.method == "fixed") {
            # Fixed benchmark method
            qual <- qual.beta.fixed(f = boot.samp.f, b.fixed = bm.val, test.diff = FALSE, boot.reps = boot.reps)
          } else {
            # Empirical overlap-based method
            qual <- qual.beta(f = boot.samp.f, b = boot.samp.b, xs = xs, test.diff = FALSE)
          }
          boot.q.curr <- qual$q
        }

        if (is.null(boot.q.curr) || is.na(boot.q.curr)) {
          boot.q[i] <- NA
        } else {
          boot.q[i] <- boot.q.curr
        }
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
      boot.q.cls <- q + boot.q.dev.cls
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
      # q.ucls[n, sim] <- boot.q.ucl.onetailed
      q.lcls[n, sim] <- q.lcl
      q.ucls[n, sim] <- q.ucl

      # Append individual simulation to simulations data frame
      df.sims <- rbind(df.sims, as.character(c(
        curr.veg,
        ei,
        curr.stratum,
        e.size,
        boot.reps,
        sims,
        n,
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
    } # End simulation (sim) loop

    ###################################################
    # Calculate results of this experiment (=set of simulations)
    ###################################################

    sigdiffs <- length(which(results[n, ] == TRUE))
    curr.pwr <- sigdiffs / sims
    curr.pwr.disp <- specify_decimal(curr.pwr, 2)
    q.disp <- specify_decimal(mean(qs[n, ], na.rm = T), 2)
    q.lcl.disp <- specify_decimal(mean(q.lcls[n, ], na.rm = T), 2)
    q.ucl.disp <- specify_decimal(mean(q.ucls[n, ], na.rm = T), 2)

    # Adjusted mean effect size for this simulation
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

    # Echo results for this experiment
    cat(paste0(
      "nf=", n.f, ", nb=", n.b, ", q.mean=", q.disp,
      ", lcl.mean=", q.lcl.disp,
      ", ucl.mean=", q.ucl.disp,
      ", e.size=", e.size.disp,
      ", e.size.adj=", e.size.adj.disp,
      ", e.size.min=", e.size.min.disp,
      ", boot.reps=", boot.reps,
      ", sims=", sims,
      ", sig.diffs=", sigdiffs,
      ", power=", curr.pwr.disp,
      "\n"
    ))
    flush.console()

    # Save power results for plotting
    df.pwr <- rbind(df.pwr, as.numeric(c(n, e.size, e.size.adj, e.size.min, curr.pwr)))
  } # End sample size (experiment) loop
} # End effect size loop

###################################
# Save the individual simulation results
###################################

# Name columns again because stupid R loses names when append
# to empty data frame
colnames(df.sims) <- c(
  "landcover",
  "ei",
  "stratum",
  "e.size",
  "boot.reps",
  "sims",
  "n",
  "q.obs",
  "q.lcl",
  "q.ucl",
  "q.crit.lower",
  "q.crit.upper",
  "sig.lower",
  "sig.upper",
  "sig"
)

write.csv(df.sims, file = sims.file, row.names = FALSE)

###################################
# Fit power curves and determine target
# sample sizes for each e.size
###################################

# Rename the final data frame
colnames(df.pwr) <- c("n", "e.size", "e.size.adj", "e.size.min", "pwr")
df.pwr$n.min <- NA
df.pwr$fit <- NA

# es <- 1	# for testing without loop
for (es in 1:length(e.sizes)) { # Start second effect size loop
  # Set current effect size & echo
  e.size <- e.sizes[es]

  curr.df.pwr <- df.pwr[df.pwr$e.size == e.size, ]

  if (fit.method == "mm") {
    fit <- mm.power(curr.df.pwr, pwr.target = pwr.target)
    df.pwr$n.min[df.pwr$e.size == e.size] <- fit$n.min
    df.pwr$fit[df.pwr$e.size == e.size] <- fit$pwr.fit
  } else if (fit.method == "logistic") {
    fit <- power.logistic(df.pwr = curr.df.pwr, pwr.target = pwr.target)

    if (!fit[1][1] == "fail") {
      n.min.target <- fit$n.min

      # Hack to control for bug that returns maximum value of x
      # over submitted x values (x.obs) if predicted value of x (x.pred)
      # is outside domain of x.obs
      if (n.min.target >= n.finish) n.min.target <- 99999

      df.pwr$n.min[df.pwr$e.size == e.size] <- n.min.target
      df.pwr$fit[df.pwr$e.size == e.size] <- fit$pwr.fit
    }
  } else {
    stop(paste0("ERROR: Fit method ", fit.method, "not recognized!"))
  }
} # End second effect size loop

#######################
# Add remaining columns
#######################

# Make data frame of complete results
pwr.results <- df.pwr
pwr.results$EI <- ei
pwr.results$landcover <- curr.veg
if (has.strata == T) {
  pwr.results$stratum <- curr.stratum
} else {
  pwr.results$stratum <- "nostrata"
}
pwr.results$n.b <- length(b)
pwr.results$n.f <- length(f)
pwr.results$expts <- sims
pwr.results$boot.reps <- boot.reps
pwr.results$pwr.target <- pwr.target

# Reorder some columns
col_idx <- grep("n.f", names(pwr.results))
pwr.results <- pwr.results[, c(col_idx, (1:ncol(pwr.results))[-col_idx])]
col_idx <- grep("n.b", names(pwr.results))
pwr.results <- pwr.results[, c(col_idx, (1:ncol(pwr.results))[-col_idx])]
col_idx <- grep("stratum", names(pwr.results))
pwr.results <- pwr.results[, c(col_idx, (1:ncol(pwr.results))[-col_idx])]
col_idx <- grep("EI", names(pwr.results))
pwr.results <- pwr.results[, c(col_idx, (1:ncol(pwr.results))[-col_idx])]
col_idx <- grep("landcover", names(pwr.results))
pwr.results <- pwr.results[, c(col_idx, (1:ncol(pwr.results))[-col_idx])]

#######################
# Save results to file
#######################
if (save.data == T) {
  if (file.exists(results.file)) {
    # Open existing results file if it exists and append new results
    dat <- read.csv(results.file, header = T, stringsAsFactors = FALSE)

    # Delete previous results for this combination, if applicable
    if (replace.data == T) {
      strat <- curr.stratum
      if (strat == "") strat <- "nostrata"
      rows <- which(dat$EI == ei & dat$landcover == curr.veg & dat$stratum == strat)
      # Delete rows pertaining to current combination
      if (length(rows) > 0) dat <- dat[-c(rows), ]
    }
    pwr.results.all <- rbind(dat, pwr.results)
  } else {
    pwr.results.all <- pwr.results
  }

  write.csv(pwr.results.all, file = results.file, row.names = FALSE)
}

cat("Run completed\n")

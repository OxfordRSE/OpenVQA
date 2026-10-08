###############################################
# Plots f and b distributions with shaded overlap and bootstrap
# bootstrap confidence limits
#
# Requires vectors b & f already exist.
# Run any EI script first.
# Or load previous data! (load(file = "data.Rdata"))
###################################################


source("../includes/functions.R")
source("../params.R")
# load(file = "overlap.demo.data.Rdata")
load("overlap.demo.ws.Rdata")


#############################
# Parameters
#############################

# Set the vegetation to analyze
curr.veg <- allveg[16] # Wetland
curr.veg <- allveg[13] # Wet forest early-mid
curr.veg <- allveg[9] # Intermediate forest, mature
curr.veg <- allveg[6] # Dry forest, mature
curr.veg <- allveg[5]
print(curr.veg)

# Set stratum if applicable
curr.stratum <- ""
print(curr.stratum)

# Boot replicates
boot.reps <- 100

# Set seed? If TRUE, sets randomization seed to fix results
set.seed <- F

# Rarify observed samples?
# If false, will reduce bootstrap samples only, observed curves
# will stay constant.
reduce.observed <- F

# Sample sizes
# Can be any number >3, but if > actual sample size will be
# automatically set to actual sample size
# set to >=1000 to use actual sample size
n.f <- 50
n.b <- 50

# Set to TRUE to plot bootstrap pdfs and CLs
plot.boot <- T

# Set to TRUE to plot overlap
plot.overlap <- T

# Set to TRUE to plot one-tailed overlap
# Applies only if one-tailed overlap (test.tail="lower" or test.tail="upper")
# Only overlap in the relevant tail is shaded
plot.overlap.onetailed <- F

# Set to TRUE to plot focal non-overlap
# Applies only if one-tailed overlap (test.tail="lower" or test.tail="upper")
# Only non-overlap in the relevant tail is shaded
plot.nonoverlap.onetailed <- F

# Set to TRUE to plot all bootstrapped overlaps with progressive shading
# Set to FALSE to plot observed overlap only
# No effect unless plot.overlap als=T
plot.all.overlap <- T

# Display f/b color legend?  (T/F)
show.fb.legend <- T

# Include sample sizes in f/b color legend? (T/F)
display.n <- F

# Set to FALSE to omit overlap CLs from overlap legend
display.overlap.cls <- T

# Set to FALSE to use varying fill colours for bootstrap overlap polygons
boot.ol.color.fixed <- T

# Set EI
ei <- curr.EI # Set by calling script

# Set text for x axis
x.text <- EI.name # Set by calling script

# Tail of bm distn to test
# Values: lower, upper, both
# If one-tailed (lower or upper) will shade only overlap values
# < or > bm mean
test.tail <- "lower"
test.tail <- "both"

# Include vertical line at bm mean?
# Ignored if test.tail==both
show.bm.mean <- FALSE

# Use custom position for legend?
# MUST supply custom values if TRUE
leg.pos.custom <- T
x.pos.custom <- 30
y.pos.custom <- y.max + 0.09 * y.max

# Display 2-tailed and 1-tailed quality
compare.overlap <- F

#############################
# Calculate observered and bootstrap
# pdfs and overalp
#############################

print(ei)

# Select the vegetation and stratum to plot
EI.veg <- subset(dat.EI, dat.EI$vegClass == curr.veg)

if ("stratum" %in% colnames(EI.veg) && !(curr.stratum == "" || curr.stratum == "no.strata")) {
  EI.strat <- subset(EI.veg, EI.veg$stratum == curr.stratum)
  b <- EI.strat[EI.strat$focalOrBenchmark == "b", c("EI")]
  f <- EI.strat[EI.strat$focalOrBenchmark == "f", c("EI")]
} else {
  b <- EI.veg[EI.veg$focalOrBenchmark == "b", c("EI")]
  f <- EI.veg[EI.veg$focalOrBenchmark == "f", c("EI")]
}

# start timer
ptm <- proc.time()

x.max <- max(max(f), max(b))

xs <- seq(0, x.max + 10)

# Set final sample sizes
n.f <- min(n.f, length(f))
n.b <- min(n.b, length(b))

# reduce actual samples if requested
if (reduce.observed == T) {
  if (n.f < length(f)) f <- sample(f, n.f, replace = F)
  if (n.b < length(b)) b <- sample(b, n.b, replace = F)
}

# Get actual pdfs & their overlap
mod <- suppressWarnings(fitdistr(f, densfun = "negative binomial"))
pdf.f <- dnbinom(xs, mu = mod$estimate[2], size = mod$estimate[1])

mod <- suppressWarnings(fitdistr(b, densfun = "negative binomial"))
pdf.b <- dnbinom(xs, mu = mod$estimate[2], size = mod$estimate[1])

# If one-tailed test, set 1-tailed params for plotting
if (test.tail == "lower") {
  b.mean <- mean(b)
  xs.lt <- xs[xs <= b.mean] # x < b.mean
  b.mean.approx <- max(xs.lt)
  pdf.b.lt <- head(pdf.b, length(xs.lt)) # pdf.b over b<=b.mean
  pdf.f.lt <- head(pdf.f, length(xs.lt)) # pdf.f over f <=b.mean

  # Focal non-overlap, lower tail
  xs.f.greater.lt <- xs[pdf.f > pdf.b & xs <= b.mean]
  pdf.b.f.greater.lt <- pdf.b[pdf.f > pdf.b & xs <= b.mean]
  pdf.f.f.greater.lt <- pdf.f[pdf.f > pdf.b & xs <= b.mean]
} else if (test.tail == "upper") {
  b.mean <- mean(b)
  xs.ut <- xs[xs >= b.mean]
  b.mean.approx <- min(xs.ut)
  pdf.b.ut <- tail(pdf.b, length(xs.ut))
  pdf.f.ut <- tail(pdf.f, length(xs.ut))

  # Focal non-overlap, upper tail
  xs.f.greater.ut <- xs[pdf.f > pdf.b & xs >= b.mean]
  pdf.b.f.greater.ut <- pdf.b[pdf.f > pdf.b & xs >= b.mean]
  pdf.f.f.greater.ut <- pdf.f[pdf.f > pdf.b & xs >= b.mean]
}

# Calculate overlap using function q.overlap
ol <- q.overlap(pdf.f, pdf.b, xs, b.mean, "both") # Two-tailed overlap quality
ol.ut <- q.overlap(pdf.f, pdf.b, xs, b.mean, "upper") # one-tailed overlap (upper)
ol.lt <- q.overlap(pdf.f, pdf.b, xs, b.mean, "lower") # one-tailed overlap (lower)

# Run the bootstrap
if (set.seed == TRUE) set.seed(123)

boot.pdf.f <- sapply(1:boot.reps, function(i) {
  boot.samp <- sample(f, n.f, replace = T)
  mod <- suppressWarnings(fitdistr(boot.samp, densfun = "negative binomial"))
  dnbinom(xs, mu = mod$estimate[2], size = mod$estimate[1])
})

boot.pdf.b <- sapply(1:boot.reps, function(i) {
  boot.samp <- sample(b, n.b, replace = T)
  mod <- suppressWarnings(fitdistr(boot.samp, densfun = "negative binomial"))
  dnbinom(xs, mu = mod$estimate[2], size = mod$estimate[1])
})

# Create vector of bootstrapped overlap values
boot.ol <- rep(NA, boot.reps)
for (i in 1:boot.reps) {
  boot.ol[i] <- sum(pmin(boot.pdf.f[, i], boot.pdf.b[, i])) * mean(diff(xs))
}

# sapply method. SLOWER!
# boot.ol <- sapply(1:boot.reps, function(i) {
# sum(pmin(boot.pdf.f[ ,i], boot.pdf.b[ ,i]))*mean(diff(xs))
# } )

# Get 95% CLs of the overlaps of the bootstrapped distributions
# using empirical bootstrap method
boot.ol.dev <- boot.ol - mean(boot.ol, na.rm = T)
boot.ol.dev.cls <- quantile(boot.ol.dev, c(0.025, 0.975), na.rm = T)
boot.ol.cls <- ol + boot.ol.dev.cls
boot.ol.cls

boot.ol.dev.ucl.onetailed <- quantile(boot.ol.dev, c(0.95), na.rm = T)
boot.ol.ucl.onetailed <- ol + boot.ol.dev.ucl.onetailed

# Make data frame of actual and bootstrapped overlaps for later reuse when calculating
# overall overlap (among all indicators) and it's CLs
boot.ol.all <- c(ei, ol, as.numeric(boot.ol.cls[1]), as.numeric(boot.ol.cls[2]), boot.ol)
ol.df <- as.data.frame(t(boot.ol.all))
ol.boot.colnames <- paste("ol.boot", seq(1, boot.reps), sep = "")
colnames(ol.df) <- c("EI", "ol.obs", "ol.lcl", "ol.ucl", ol.boot.colnames)

# get time elapsed
proc.time() - ptm

#-----------------------------------------------------------------------------
# Plot PDF
#-----------------------------------------------------------------------------

# Main title
ti.text <- curr.veg
if (!curr.stratum == "") ti.text <- paste0(ti.text, " (", curr.stratum, ")")

# Get y limits
y.max <- max(max(boot.pdf.f), max(boot.pdf.b))

# Overlap legend text
ol.txt <- specify_decimal(ol * 100, 1)
q.txt <- specify_decimal(ol, 2)
ol.ut.txt <- specify_decimal(ol.ut * 100, 1)
q.ut.txt <- specify_decimal(ol.ut, 2)
ol.lt.txt <- specify_decimal(ol.lt * 100, 1)
q.lt.txt <- specify_decimal(ol.lt, 2)
ol.ucl.txt <- specify_decimal(boot.ol.cls[2] * 100, 1)
ol.lcl.txt <- specify_decimal(boot.ol.cls[1] * 100, 1)
legendText <- paste0("Overlap: \n", ol.txt, "%")
if (display.overlap.cls == T) legendText <- paste0(legendText, " (", ol.lcl.txt, "-", ol.ucl.txt, "%)")
if (compare.overlap) {
  if (test.tail == "lower") {
    legendText <- paste0(
      "Overlap: ", ol.txt, "%", "\n",
      "Q (two-tailed): ", q.txt, "\n",
      "Q (one-tailed, lower): ", q.lt.txt
    )
  } else if (test.tail == "upper") {
    legendText <- paste0(
      "Overlap: ", ol.txt, "%", "\n",
      "Q (two-tailed): ", q.txt, "\n",
      "Q (one-tailed, upper): ", q.ut.txt
    )
  }
}

# Focal, benchmark colour legend (plus optional sample sizes)
bm.leg <- "Benchmark"
f.leg <- "Focal"
if (display.n == T) {
  bm.leg <- paste0(bm.leg, " (n=", n.b, ")")
  f.leg <- paste0(f.leg, " (n=", n.f, ")")
}
group.text <- c(bm.leg, f.leg)

# Set transparency for overlaps
# Must be from 0 to 1
alpha.val <- 0.50 # transparency for lines
alpha.val.leg <- 0.25 # transparency for legend

# Group colors
v1.col <- rgb(0, 0, 1, alpha.val) # bm
v2.col <- rgb(1, 0, 0, alpha.val) # focal

# Focal
par(
  bg = "white", las = 1, cex = 1.2,
  mar = c(5, 5, 5, 5)
)
plot(xs, boot.pdf.f[, 1],
  type = "l",
  main = ti.text,
  col = rgb(.6, .6, .6, .1),
  ylim = c(0, y.max),
  xlab = x.text,
  ylab = "Probability density",
  cex.lab = 1.5,
  cex.axis = 1,
  cex.main = 1.5,
  cex.sub = 1.5
)

# add the observed pdfs
lines(xs, pdf.f, col = rgb(1, 0, 0, alpha.val), lwd = 2)
lines(xs, pdf.b, col = rgb(0, 0, 1, alpha.val), lwd = 2)

if (plot.boot == T) {
  # Focal bootstrap pdfs
  for (i in 2:ncol(boot.pdf.f)) lines(xs, boot.pdf.f[, i], col = rgb(.6, .6, .6, .1))

  # Highlight confidence bands
  quants <- apply(boot.pdf.f, 1, quantile, c(0.025, 0.5, 0.975))
  lines(xs, quants[1, ], col = rgb(1, 0, 0, alpha.val), lwd = 1.5, lty = 2)
  lines(xs, quants[3, ], col = rgb(1, 0, 0, alpha.val), lwd = 1.5, lty = 2)
  # lines(xs, quants[2, ], col=rgb(1,0,0,alpha.val), lwd=1.5, lty=2)

  # Benchmark bootstrap pdfs
  for (i in 2:ncol(boot.pdf.b)) lines(xs, boot.pdf.b[, i], col = rgb(.6, .6, .6, .1))

  # Highlight confidence bands
  quants <- apply(boot.pdf.b, 1, quantile, c(0.025, 0.5, 0.975))
  lines(xs, quants[1, ], col = rgb(0, 0, 1, alpha.val), lwd = 1.5, lty = 2)
  lines(xs, quants[3, ], col = rgb(0, 0, 1, alpha.val), lwd = 1.5, lty = 2)
  # lines(xs, quants[2, ], col=rgb(0,0,1,alpha.val), lwd=1.5, lty=2)
}

if (plot.overlap == T) {
  if (plot.all.overlap == TRUE) {
    for (i in 2:boot.reps) { # START boot.reps (plot.all.overlap=T)
      # Plot all bootstrapped overlaps, with progressive shading
      alpha <- 1 - (i / boot.reps)
      rd <- runif(boot.reps, min = 0, max = 1)
      gn <- i / boot.reps
      bl <- 1 - runif(boot.reps, min = 0, max = 1)
      if (boot.ol.color.fixed == F) {
        # use varying colours for overlap polygns
        polygon(
          x = c(xs, rev(xs)), y = c(
            rep(0, length(xs)),
            rev(pmin(boot.pdf.f[, i], boot.pdf.b[, i]))
          ),
          col = rgb(rd, gn, bl, alpha)
        )
      } else {
        # Use single colour for overlap polygons
        polygon(
          x = c(xs, rev(xs)), y = c(
            rep(0, length(xs)),
            rev(pmin(boot.pdf.f[, i], boot.pdf.b[, i]))
          ),
          col = rgb(1, 1, 0, 0.2)
        )
      }
      print(paste0("alpha =", alpha))
    } # END boot.reps (plot.all.overlap=T)
  } else {
    # Plot observed overlap only

    if (test.tail == "both") {
      # Plot full (2-tailed) overlap region
      polygon(
        x = c(xs, rev(xs)),
        y = c(rep(0, length(xs)), rev(pmin(pdf.f, pdf.b))),
        border = NA,
        col = rgb(1, 1, 0, 0.2)
      )
    } else if (test.tail == "lower") {
      if (plot.overlap.onetailed == TRUE) {
        # Shade lower overlap region over f<b.mean
        polygon(
          x = c(xs.lt, rev(xs.lt)),
          y = c(rep(0, length(xs.lt)), rev(pmin(pdf.f.lt, pdf.b.lt))),
          border = NA,
          col = rgb(1, 1, 0, 0.2)
        )
      }

      if (plot.nonoverlap.onetailed == TRUE) {
        # Shade region where p.f>p.b over f<b.mean
        polygon(
          x = c(xs.f.greater.lt, rev(xs.f.greater.lt)),
          y = c(pdf.b.f.greater.lt, rev(pdf.f.f.greater.lt)),
          border = NA,
          col = rgb(1, 1, 0, 0.2)
        )
      }
    } else if (test.tail == "upper") {
      # Plot upper overlap region > bm mean
      polygon(
        x = c(xs.ut, rev(xs.ut)),
        y = c(rep(0, length(xs.ut)), rev(pmin(pdf.f.ut, pdf.b.ut))),
        border = NA,
        col = rgb(1, 1, 0, 0.2)
      )
    }
  }

  # Add overlap legend
  if (leg.pos.custom == TRUE) {
    x.pos <- x.pos.custom
    y.pos <- y.pos.custom
  } else {
    x.pos <- x.max - 0.5 * x.max
    y.pos <- y.max + 0.09 * y.max
  }

  legend(
    legend = legendText,
    x = x.pos, y = y.pos, # position
    title = "",
    cex = 1.0,
    bty = "n"
  ) # border
}

if (show.bm.mean == TRUE && !test.tail == "both") {
  # Use max of modified x vector instead of exact b.mean
  # line and polygon correspond
  abline(v = b.mean.approx, col = "black", lwd = 1, lty = 2)
}

if (show.fb.legend == TRUE) {
  # Position of focal/benchmark color legend below overlap legend
  x.pos2 <- x.pos + 0.05 * x.max
  y.pos2 <- y.pos - 0.23 * y.max
  # y.pos2 <- y.pos-0.4*y.max
  # if (plot.overlap==F) {
  # #x.pos <- x.max - 0.35*x.max
  # x.pos <- x.max-0.465*x.max
  # y.pos <- y.max
  # }
  legend(
    x = x.pos2,
    y = y.pos2,
    legend = group.text,
    fill = c(rgb(0, 0, 1, alpha.val.leg), rgb(1, 0, 0, alpha.val.leg)),
    box.lty = 0
  )
}


# save(file = "overlap.demo.data.Rdata")

save.image(file = "overlap.demo.ws.Rdata")

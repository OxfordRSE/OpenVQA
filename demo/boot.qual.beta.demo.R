#################################################
# Demo of function boot.qual.beta
# Based on earlier "boot.qual.beta.test.R"
#################################################

# Set name of the graph file
graphFileName <- "beta.demo_c_nofit_rescaled"

require(MASS)
require(simpleboot)
require(betareg)
require(car)

# Distribution fitting parameters
lowlim <- 0.0001
uplim <- 1 - lowlim
shape1 <- .6
shape2 <- 0.1
xs <- seq(0, 1, length.out = 1000)
breaks <- seq(0, 1, l = 10)

# Bootstrap and permutation test parameters
perm.reps <- 1000
boot.reps <- 100
seed <- 123
set.seed <- F

#########################
# Graphing  parameters
#########################

# Scale y axis?
# Do not rescale if plotting only pdf curves
rescale <- TRUE

# Plot histograms?
plot.hists <- TRUE

# Plot pdf curves?
plot.fit <- FALSE

# Normally leave set to TRUE
# only used if plot.fit==TRUE
plot.bm <- TRUE
plot.f <- TRUE

# Plot overlap between the curves
# Only applies if plot.fit==TRUE
plot.overlap <- TRUE

# show.p.means
# Include equation and p-value with means test?
show.p.means <- FALSE

# incude confidence limits in legend?
show.cls <- FALSE

# Color or greyscale?
colour <- TRUE # Set to FALSE to use greyscale

alpha.colour <- 0.2 # Same transparency for f & b if using colour

# Force manual upper limit to y axis
# set to NA to turn off
ylim.max.manual <- 0.35 # Use for Fig a
ylim.max.manual <- NA

# rgb parameters
if (colour == TRUE) {
  # focal
  r.f <- 1
  g.f <- 0
  b.f <- 0
  alpha.f <- alpha.colour

  # benchmark
  r.b <- 0
  g.b <- 0
  b.b <- 1
  alpha.b <- alpha.colour
} else {
  # focal
  r.f <- 0.1
  g.f <- 0.1
  b.f <- 0.1
  alpha.f <- 0.1

  # benchmark
  r.b <- 0.1
  g.b <- 0.1
  b.b <- 0.1
  alpha.b <- 0.3
}

#########################################################
# Set path to save output figures
#
# This is done after initally loading and inspecting the data in case parameter
# group.strata gets reset
##########################################################

# Set working directory
# Must set first to set directory parameters
wd <- getwd()
setwd(wd)

figs.dir <- "figs/"
figdir.abs <- paste(wd, figs.dir, sep = "/")

# Base directory name for ungroup figure of combine focal and bm dists
figs.beta.dir.name <- "beta.demo"

# Create directory if missing
dir.create(file.path(figdir.abs, figs.beta.dir.name), showWarnings = F)
figs.beta.dir <- paste(figs.dir, figs.beta.dir.name, sep = "")

########################################
# Save vectors and associated file names
########################################

# Fig.7a
# "beta.demo_a"
f <- pbeta(xs, 0.1, 0.1) # leptokurtic, mean near 0.5
b <- pbeta(xs, 0.3, 0.3) # slightly platykurtic, mean near 0.5

# Fig.7b
# Beta=0, f close to but not equal to zero
# "beta.demo_b"
f <- pbeta(xs, 0.5, 0.1) # right skewed, mean near 0
b <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # b all zeros

# Fig. 7b alt
# Beta mostly 0, plus few non-zeros to allow fit; f close to but not equal to zero
# "beta.demo_b.alt"
f <- pbeta(xs, 0.5, 0.1) # right skewed, mean near 0
b <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.001, 0.001, 0.005, 0.012, 0.015, 0.055)

# Fig.7c
# "beta.demo_c"
b <- pbeta(xs, 0.5, 0.1) # right skewed, mean near 0
f <- pbeta(xs, 0.1, 0.5) # left-skewed, mean near 1

# Fig.7c alt
# "beta.demo_c.alt"
b <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.001, 0.001, 0.005, 0.012, 0.015, 0.055)
f <- pbeta(xs, 0.1, 0.5) # left-skewed, mean near 1

# Fig.7d
# "beta.demo_d"
f <- pbeta(xs, 0.3, 0.3) # slightly leptokurtic, mean near 0.5
b <- pbeta(xs, 3, 3) # Bimodal, modes @ 0 & 1

#################################################
# Vector pair to test, paste below:
#################################################

# Fig.7c
# "beta.demo_c"
b <- pbeta(xs, 0.5, 0.1) # right skewed, mean near 0
f <- pbeta(xs, 0.1, 0.5) # left-skewed, mean near 1

#################################################
# Main
#################################################

source(paste0("includes/", "functions.R"), local = TRUE)

qual <- boot.qual.beta(f = f, b = b, xs = xs, boot.reps = boot.reps, seed = seed, set.seed = set.seed, shape1 = shape1, shape2 = shape2, lowlim = lowlim)

qual[1:10]
qual$method

# Get the model results
f.mean <- qual$f.mean
b.mean <- qual$b.mean
prop.diff <- f.mean / b.mean
diff <- qual$q.diff
p <- qual$p.diff
pdf.f <- qual$pdf.f # Fitted focal pdf
pdf.b <- qual$pdf.b # Fitted benchmark pdf
pdf.xs <- qual$xs # x values used to plot pdfs
q.diff <- qual$q.diff # Means-test quality
q.ol <- qual$q.ol # Overlap quality
wt <- qual$ol.wt # Overlap-means weighting factor
q <- qual$q # Overall quality and confidence limits
q.lcl <- qual$q.lcl
q.ucl <- qual$q.ucl

# get overlap polygon
ol <- sum(pmin(pdf.f, pdf.b) * mean(diff(xs)))

# graph title, goes below maring title (=title, in m.text)
text.main <- ""

# Axis labels
text.x <- "X"
text.y <- "Density"

# Axis limits
xlim.min <- 0
xlim.max <- 1
ylim.min <- 0
# ylim.max <- xxx

# Means test legend
if (is.na(p) || is.null(p)) {
  means.leg <- ""
} else if (p < 0.001) {
  means.leg <- "(f = b, p<0.001)"
} else if (p >= 0.05) {
  means.leg <- "(f = b, p>0.05)"
} else {
  p.txt <- specify_decimal(p, 3)
  diff.txt <- specify_decimal(diff, 3)
  means.leg <- paste0("(f = ", diff.txt, "*b, p=", p.txt, ")")
}

# Difference-based qualty legend
if (is.na(q.diff) || is.null(q.diff)) {
  q.diff.leg <- "Q.means=<NA>"
} else {
  q.diff.txt <- specify_decimal(q.diff, 3)
  q.diff.leg <- paste0("Q.means =", q.diff.txt)
}

# Overlap legend
if (is.na(q.ol) || is.null(q.ol)) {
  q.ol.leg <- "Q.means =<NA>"
} else {
  q.ol.txt <- specify_decimal(q.ol, 3)
  q.ol.leg <- paste0("Q.overlap=", q.ol.txt)
}

# Quality equation legend
ol.wt.txt <- specify_decimal(wt, 2)
diff.wt.txt <- specify_decimal(1 - wt, 2)
q.eqn <- paste0("Q = ", ol.wt.txt, "*Q.overlap + ", diff.wt.txt, "*Q.means")

# Overall quality legend
if (is.na(q.lcl) || is.null(q.lcl) || is.na(q.ucl) || is.null(q.ucl)) {
  q.cl.leg <- ""
} else {
  q.lcl.txt <- specify_decimal(q.lcl, 3)
  q.ucl.txt <- specify_decimal(q.ucl, 3)
  q.cl.leg <- paste0(" (", q.lcl.txt, "-", q.ucl.txt, ")")
}
if (is.na(q) || is.null(q)) {
  q.leg <- "Q=<NA>"
} else {
  q.txt <- specify_decimal(q, 3)

  if (show.cls == TRUE) {
    q.leg <- paste0("Q=", q.txt, q.cl.leg)
  } else {
    q.leg <- paste0("Q=", q.txt)
  }
}
comma <- ", "


if (show.p.means == TRUE) {
  title <- paste0(q.ol.leg, "\n", q.diff.leg, " ", means.leg, "\n", q.eqn, "\n", q.leg)
} else {
  title <- paste0(q.ol.leg, "\n", q.diff.leg, "\n", q.eqn, "\n", q.leg)
}

######################################
# Get axis parameters & rescale if requested
######################################

h.f <- hist(f, breaks = seq(0, 1, l = 20), plot = FALSE)
h.b <- hist(b, breaks = seq(0, 1, l = 20), plot = FALSE)

# y upper limit is bin with highest count
highestCount <- max(h.b$counts, h.f$counts)
ylim.max <- highestCount + (highestCount * 0.4)

# Rescale if requested
freq.val <- T
if (rescale == T) {
  h.b$density <- h.b$counts / sum(h.b$counts)
  h.f$density <- h.f$counts / sum(h.f$counts)
  freq.val <- F
  text.y <- "Frequency"

  # y upper limit is bin with highest density
  highestDensity <- max(h.b$density, h.f$density)
  if (plot.f == F) {
    highestDensity <- max(h.b$density) # Use bm values only
  } else if (plot.bm == F) {
    highestDensity <- max(h.f$density) # Use focal values only
  }
  ylim.max <- min(1, highestDensity + 0.1)

  if (plot.fit == TRUE) {
    # Rescale fitted distribution density
    if (plot.bm == TRUE && !(is.na(pdf.b))) pdf.b <- pdf.b / max(pdf.b[pdf.b < Inf]) * max(h.b$density) * 0.90
    if (plot.f == TRUE && !(is.na(pdf.f))) pdf.f <- pdf.f / max(pdf.f[pdf.f < Inf]) * max(h.f$density) * 0.90
  }
} else {
  if (plot.fit == TRUE) {
    if (plot.hists == TRUE) {
      # Adjust fitted curve to scale of raw data
      if (plot.bm == TRUE && !(is.na(pdf.b))) pdf.b <- pdf.b / max(pdf.b[pdf.b < Inf]) * max(h.b$counts) * 0.90
      if (plot.f == TRUE && !(is.na(pdf.f))) pdf.f <- pdf.f / max(pdf.f[pdf.f < Inf]) * max(h.f$counts) * 0.90
    } else {
      # Reset y axis limit
      if (is.na(pdf.b)) {
        max.pdf.b <- 0
      } else {
        max.pdf.b <- max(pdf.b)
      }
      if (is.na(pdf.f)) {
        max.pdf.f <- 0
      } else {
        max.pdf.f <- max(pdf.f)
      }

      y.max.pdf <- max(max.pdf.f, max.pdf.b)
      ylim.max <- y.max.pdf + (0.05 * y.max.pdf)
    }
  }
}

if (!is.na(ylim.max.manual)) ylim.max <- ylim.max.manual

######################################
# Plot the graph
######################################

par(bg = "white", mfrow = c(1, 1), oma = c(0, 0, 4, 0))

# Plot the focal histogram
if (plot.hists == TRUE) {
  plot(h.f,
    main = text.main,
    xlab = text.x,
    ylab = text.y,
    col = rgb(r.f, g.f, b.f, alpha.f),
    xlim = c(xlim.min, xlim.max),
    ylim = c(ylim.min, ylim.max),
    freq = freq.val
  )

  # Plot the benchmark histogram
  plot(h.b,
    main = text.main,
    xlab = text.x,
    ylab = text.y,
    col = rgb(r.b, g.b, b.b, alpha.b),
    xlim = c(xlim.min, xlim.max),
    ylim = c(ylim.min, ylim.max),
    freq = freq.val,
    add = TRUE
  )
} else {
  # Make empty plot to hold the pdf curves
  y.max.pdf <- max(max(pdf.f), max(pdf.b))
  ylim.max <- y.max.pdf + (0.05 * max(max(pdf.f), max(pdf.b)))

  plot(h.f,
    main = text.main,
    xlab = text.x,
    ylab = text.y,
    col = rgb(0, 0, 0, 0),
    xlim = c(xlim.min, xlim.max),
    ylim = c(ylim.min, ylim.max),
    lty = "blank",
    freq = freq.val
  )
}

# Plot pdfs if requested
if (plot.fit == TRUE) { # START Add fit lines

  if (plot.f == TRUE) {
    # Focal pdf
    curve2 <- lines(pdf.xs, pdf.f,
      lwd = 2,
      col = rgb(r.f, g.f, b.f, alpha.f)
    )
  }

  if (plot.bm == TRUE) {
    # Bm pdf
    curve1 <- lines(pdf.xs, pdf.b,
      lwd = 2,
      col = rgb(r.b, g.b, b.b, alpha.b)
    )
  }
} # END The pdf fit lines

if (plot.overlap == T && plot.fit == TRUE && plot.f == TRUE && plot.bm == TRUE) {
  polygon(
    x = c(xs, rev(xs)), y = c(
      rep(0, length(xs)),
      rev(pmin(pdf.f, pdf.b))
    ),
    col = rgb(1, 1, 0, 0.2)
  )
}

mtext(title, outer = TRUE, cex = 1)

##############################
# Save the graph
##############################

# Make file name
graphFile <- paste(figs.beta.dir, "/", graphFileName, ".png", sep = "")

# Save the graph
print(paste("Printing graph: ", graphFileName, sep = ""))
dev.copy(png, graphFile)
dev.off()

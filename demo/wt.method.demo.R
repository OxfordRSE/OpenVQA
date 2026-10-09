# Tinkering with beta function for weighting overlap vs means

# Clear workspace
rm(list = ls())

# Load functions
BASEDIR <- "/Users/bboyle/Documents/hga/teck/vqa_publication/analysis/scripts/"
INCLUDESDIR <- paste0(BASEDIR, "scripts/includes/")
source(paste(INCLUDESDIR, "functions.R", sep = ""), local = TRUE)

#################################
# Parameters & functions
# Each line is a set of parameters for one
# weighting function
#################################
w1.s1 <- 0.2
w1.s2 <- 2 # steep
w1.beta <- 1

w2.s1 <- 0.8
w2.s2 <- 3
w2.s1 <- 0.5
w2.s2 <- 3
w2.s1 <- 0.5
w2.s2 <- 1 # shallow
w2.beta <- 2

w3.s1 <- 0.2
w3.s2 <- 1
w3.beta <- 3


# U-shaped weighting function
u.beta <- function(x, sh1 = 0.2, sh2 = 1) {
  ######################################
  # Transforms value on [0,1] to new values on [0,1]
  # which equals 1 at 0 or 1 and drops rapidly to zero
  # in middle of range. Use for weighting means-based
  # quality, which should only be used when one or
  # both means are close to the edge of the distribution
  ######################################


  if (x >= 0.5 && x <= 1) {
    x.tr <- x - (1 - x)
  } else if (x < 0.5 && x >= 0) {
    x.tr <- (1 - x) - (1 - (1 - x))
  } else {
    stop("input value outside domain [0,1]!")
  }
  u.beta <- qbeta(x.tr, sh1, sh2)
  return(u.beta)
}

u.beta.inverse.logit <- function(x, beta = 3) {
  ######################################
  # Transforms value on [0,1] to new values on [0,1]
  # which equals 1 at 0 or 1 and drops rapidly to zero
  # in middle of range. Use for weighting means-based
  # quality, which should only be used when one or
  # both means are close to the edge of the distribution
  ######################################

  if (x >= 0.5 && x <= 1) {
    x.tr <- x - (1 - x)
  } else if (x < 0.5 && x >= 0) {
    x.tr <- (1 - x) - (1 - (1 - x))
  } else {
    stop("input value outside domain [0,1]!")
  }
  u.beta <- 1 / (1 + (x.tr / (1 - x.tr))^(beta * -1))
  return(u.beta)
}

inv.logit <- function(x, beta = 3) {
  ######################################
  # Transforms value on [0,1] to logistic monotically
  # increasing sigmoid-logistic curve
  ######################################

  if (x < 0 || x > 1) stop("input value outside domain [0,1]!")

  # Fit logistic curve
  inv.logit <- 1 / (1 + (x / (1 - x))^(beta * -1))

  return(inv.logit)
}

u.inv.logit <- function(x, min.upper = null, beta = null) {
  ######################################
  # xxxx
  ######################################

  if (is.null(beta)) beta <- 3

  if (is.null(min.upper)) {
    min.upper <- 0.75
  } else if (min.upper < 0.5 || min.upper > 1) {
    stop("Upper minimum outside domain [0.5,1]!")
  }
  max.lower <- 1 - min.upper

  if (x >= min.upper && x <= 1) {
    x.tr <- (x - min.upper) / (1 - min.upper)
  } else if (x <= max.lower && x >= 0) {
    x.tr <- -1 * (x - max.lower) / max.lower
  } else if (x > max.lower && x < min.upper) {
    x.tr <- 0
  } else {
    stop("input value outside domain [0,1]!")
  }

  # msg <- paste0("x=", x, "; x.tr=", x.tr)
  # message(msg)
  # flush.console

  u.inv.logit <- inv.logit(x.tr, beta = beta)
  return(u.inv.logit)
}

u.u.beta <- function(x, lowlim = null, sh1 = 0.2, sh2 = 1) {
  ######################################
  # xxxx
  ######################################

  if (lowlim < 0 || lowlim > 0.1) stop("Limit 'lowlim' outside [0,0.1]!")

  if (is.null(lowlim)) lowlim <- 0.05
  uplim <- 1 - lowlim

  if (x >= 0 && x < lowlim) {
    x.tr <- 0
  } else if (x >= lowlim && x <= uplim) {
    x.tr <- 0.5 + (((x - 0.5) / (uplim - 0.5)) * 0.5)

    # Fix rare cases where transformation goes
    # slightly out of bounds
    if (x.tr < 0) {
      x.tr <- 0
    } else if (x.tr > 1) {
      x.tr <- 1
    }
  } else if (x > uplim && x <= 1) {
    x.tr <- 1
  } else {
    stop("input value outside domain [0,1]!")
  }

  # msg <- paste0("x=", x, "; x.tr=", x.tr)
  # message(msg)
  # flush.console

  u.u.beta <- u.beta(x.tr, sh1 = sh1, sh2 = sh2)
  # u.u.beta<-1
  return(u.u.beta)
}

#################################
# Generate the curves
#################################
xs.step <- 0.005
# xs.step <- 0.05
xs <- seq(0, 1, xs.step)
n.xs <- length(xs)

# Original (no weight, all overlap)
w0 <- rep(1, n.xs)

# Linear
w1a <- seq(1, -1, -(xs.step * 2))
w1a <- head(w1a, (length(w1a) / 2) + 1)
w1b <- seq(-1, 1, xs.step * 2)
w1b <- tail(w1b, (length(w1b) / 2) - 1)
w.linear <- c(w1a, w1b)

# # u.beta transform a
w1 <- unlist(as.vector(lapply(xs, u.beta, sh1 = 0.2, sh2 = 1)))
# w2<- unlist(as.vector(lapply(xs, u.beta, sh1=w2.s1, sh2=w2.s2)))
# w3<- unlist(as.vector(lapply(xs, u.beta, sh1=w3.s1, sh2=w3.s2)))

# u.beta.inverse.logit transform
# w1<- unlist(as.vector(lapply(xs, u.beta.inverse.logit, beta=w1.beta)))

# # inv.logit transform
# w1<- unlist(as.vector(lapply(xs, inv.logit, beta=w1.beta)))

# u.inv.logit transform
# w2<- unlist(as.vector(lapply(xs, u.inv.logit, min.upper=0.8, beta=3)))
# w3<- unlist(as.vector(lapply(xs, u.inv.logit, min.upper=0.9, beta=3)))
w2 <- unlist(as.vector(lapply(xs, u.inv.logit, min.upper = 0.8, beta = 3)))

# u.u.beta transform
w2 <- unlist(as.vector(lapply(xs, u.u.beta, lowlim = 0.02, sh1 = 0.2, sh2 = 1.25)))
w2 <- unlist(as.vector(lapply(xs, u.u.beta, lowlim = 0.01, sh1 = 0.2, sh2 = 3)))
w3 <- unlist(as.vector(lapply(xs, u.u.beta, lowlim = 0.01, sh1 = 0.2, sh2 = 2)))

# # #################################
# # Plot the transform
# #################################

# op <- par(mar = c(5,5,4,2) + 0.1) 	# Increase left & bottom margins

# # 1:1 line
# plot(xs, w1,
# type='l', col='green',
# main="Weighting functions",
# ylab="Overlap half-weight",
# xlab="Indicator mean",
# cex.lab=1.3,
# xlim=c(0,1),
# ylim=c(0,1)
# )

# # Transformations
# #lines(xs, w1, col='green')
# lines(xs, w2, col='blue')
# lines(xs, w3, col='orange')

# # Legend
# legend("topleft", inset=.2,
# legend=c("w1", "w2", "w3"),
# col=c("green", "blue", "orange"),
# , lty=1, cex=1
# )

par(op) # Reset the margins


# # #################################
# Plot the transform for single algorithm only
##################################

op <- par(mar = c(5, 5, 4, 2) + 0.1) # Increase left & bottom margins

title <- paste0("Weighting function (", "uu.beta", ")")
# 1:1 line
plot(xs, w3,
  type = "l", col = "blue",
  main = "Means-based quality weighting function",
  ylab = "Means-based quality weight (W)",
  xlab = "Indicator mean",
  cex.lab = 1.3,
  xlim = c(0, 1),
  ylim = c(0, 1)
)

par(op) # Reset the margins

#################################################
# Tests function boot.qual.beta
#################################################

require(MASS)
require(simpleboot)
require(betareg)
require(car)

lowlim <- 0.0001
uplim <- 1 - lowlim
shape1 <- .6
shape2 <- 0.1
xs <- seq(0, 1, length.out = 100)
breaks <- seq(0, 1, l = 10)
perm.reps <- 1000
boot.reps <- 100
seed <- 123
set.seed <- F

########################################
# Save beta distribution examples
f <- pbeta(xs, 0.5, 0.1) # right skewed, mean near 0
f <- pbeta(xs, 0.1, 0.5) # left-skewed, mean near 1
f <- pbeta(xs, 0.1, 0.1) # leptokurtic, mean near 0.5
b <- pbeta(xs, 0.3, 0.3) # slightly leptokurtic, mean near 0.5
b <- pbeta(xs, 1, 1) # platykurtic, almost uniform, mean near 0.5
b <- pbeta(xs, 0.5, 0.5) # slightly platykurtic, mean near 0.5
f <- pbeta(xs, 0.5, 2) # left-skewed, mean=1
f <- pbeta(xs, 3, 3) # Bimodal, modes @ 0 & 1
b <- pbeta(xs, 6, 6) # Bimodal, extreme modes @ 0 & 1

#################################################
# Candidate vector pairs
# Paste the one you want to test at the end
#################################################

# Slight overlap, similar mean near zero
# b almost all zero, f.mean near zero but not exact zeros
b <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.001, 0.0015, 0.0015, 0.0015)
f <- c(0.001, 0.0015, 0.0015, 0.0015, 0.001, 0.0015, 0.0015, 0.0015, 0.001, 0.01, 0.001, 0.045, 0.05, 0.015, 0.015)

# No overlap, similar mean near zero
# b all zeros, f.mean near zero but not exact zeros
b <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
f <- c(0.001, 0.0015, 0.0015, 0.0015, 0.001, 0.0015, 0.0015, 0.0015, 0.001, 0.01, 0.001, 0.045, 0.05, 0.015, 0.015)

# Slight overlap, similar mean near one
# b almost all ones, f.mean near one but not exact ones
b <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.99, 0.99, 0.99, 0.97, 0.97)
f <- c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.97, 0.97, .97, .96, 0.96, 0.95)


# Beta vector has only two similar values
b <- c(0.01, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.012, 0.012, 0.012, 0.012, 0.012, 0.012, 0.012, 0.012, 0.012, 0.012)
f <- c(0.3, 0.35, 0.35, 0.37, 0.37, 0.37, 0.37, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.43, 0.43, 0.43, 0.43, 0.43, 0.45, 0.45, 0.45, 0.46)

# Beta vector has only two dissimilar values
b <- c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3)
f <- c(0.3, 0.35, 0.35, 0.37, 0.37, 0.37, 0.37, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.43, 0.43, 0.43, 0.43, 0.43, 0.45, 0.45, 0.45, 0.46)


# Totally different distributions with same mean
b <- c(0.4, 0.45, 0.45, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.55, 0.55, 0.6)
f <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)

b <- c(0.01, 0.01, 0.01, 0.015, 0.35, 0.045, 0.07, 0.12)
f <- c(0.01, 0.01, 0.01, 0.015, 0.35, 0.045, 0.07, 0.12)

f <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
b <- c(0, 0, 0, 0, 0, 0.001, 0.01, 0.045, 0.05, 0.015, 0.015)

f <- pbeta(xs, shape2, shape1)
b <- pbeta(xs, shape1, shape2)

# b <- c(0,0,0,0,0,0,00,0,0,0,0.0)
# f <- c(0.001, 0.01,0.001, 0.01,0.01,0.04,0.12,0.4,0.01,0.01,0.01,0.01)

# Beta vector has only two dissimilar values
b <- c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3)
f <- c(0.3, 0.35, 0.35, 0.37, 0.37, 0.37, 0.37, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.43, 0.43, 0.43, 0.43, 0.43, 0.45, 0.45, 0.45, 0.46)

# No overlap, similar mean near one
# b all ones, f.mean near one but not exact ones
b <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
f <- c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.97, 0.97, .97, .96, 0.96, 0.95)

f <- pbeta(xs, 0.1, 0.1) # leptokurtic, mean near 0.5
b <- pbeta(xs, 0.3, 0.3) # slightly platykurtic, mean near 0.5

f <- pbeta(xs, 0.5, 0.1) # right skewed, mean near 0
b <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # b all zeros

b <- pbeta(xs, 0.5, 0.1) # right skewed, mean near 0
f <- pbeta(xs, 0.1, 0.5) # left-skewed, mean near 1

f <- pbeta(xs, 0.3, 0.3) # slightly leptokurtic, mean near 0.5
b <- pbeta(xs, 3, 3) # Bimodal, modes @ 0 & 1

#################################################
# Main
#################################################

source(paste0("includes/", "functions.R"), local = TRUE)

qual <- boot.qual.beta(f = f, b = b, xs = xs, boot.reps = boot.reps, seed = seed, set.seed = set.seed, shape1 = shape1, shape2 = shape2, lowlim = lowlim)

qual[1:10]
qual$method

# Means test legend
f.mean <- qual$f.mean
b.mean <- qual$b.mean
prop.diff <- f.mean / b.mean
diff <- qual$q.diff
p <- qual$p.diff
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
q.diff <- qual$q.diff
if (is.na(q.diff) || is.null(q.diff)) {
  q.diff.leg <- "Q.means=<NA>"
} else {
  q.diff.txt <- specify_decimal(q.diff, 3)
  q.diff.leg <- paste0("Q.means =", q.diff.txt)
}

# Overlap legend
q.ol <- qual$q.ol
if (is.na(q.ol) || is.null(q.ol)) {
  q.ol.leg <- "Q.means =<NA>"
} else {
  q.ol.txt <- specify_decimal(q.ol, 3)
  q.ol.leg <- paste0("Q.overlap=", q.ol.txt)
}

# Overall quality legend
q <- qual$q
q.lcl <- qual$q.lcl
q.ucl <- qual$q.ucl
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
  q.leg <- paste0("Q=", q.txt, q.cl.leg)
}
comma <- ", "

title <- paste0(q.ol.leg, "\n", q.diff.leg, " ", means.leg, "\n", q.leg)

par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))
hist(f, breaks = seq(0, 1, l = 20))
hist(b, breaks = seq(0, 1, l = 20))
mtext(title, outer = TRUE, cex = 1)

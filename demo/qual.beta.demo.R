#################################################
# Tests function boot.qual.beta
#################################################

BASEDIR <- "/Users/bboyle/Documents/hga/teck/vqa_publication/analysis/scripts/"
INCLUDESDIR <- paste0(BASEDIR, "includes/")
source(paste(INCLUDESDIR, "functions.R", sep = ""), local = TRUE)


require(MASS)
require(simpleboot)
require(betareg)
require(car)

lowlim <- 0.0001
uplim <- 1 - lowlim
shape1 <- .6
shape2 <- 0.1
xs <- seq(0, 1, length.out = 100)
xs[1] <- 0.0001
xs[100] <- 0.9999
breaks <- seq(0, 1, l = 10)
perm.reps <- 1000
boot.reps <- 100
seed <- 123
set.seed <- F
discount.method <- "linear"
test.diff.boot <- FALSE

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


# Some handy vectors for testing
zeros <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
ones <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
small <- c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)
big <- c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99)
smaller <- c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001)
bigger <- c(0.999, 0.999, 0.999, 0.999, 0.999, 0.999, 0.999, 0.999, 0.999, 0.999, 0.999, 0.999)
all0.1 <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
all0.9 <- c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9)
all0.5 <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
all0.2 <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
all0.7 <- c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7)

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

demo <- function(f, b, xs, boot.reps, seed, set.seed, shape1, shape2, lowlim) {
  qual <- boot.overlap.beta(
    f = f, b = b, xs = xs,
    boot.reps = boot.reps, seed = seed, set.seed = set.seed,
    shape1 = shape1, shape2 = shape2, lowlim = lowlim,
    discount.method = discount.method, test.diff.boot = test.diff.boot
  )
  # qual <- boot.qual.beta(f=f, b=b , xs=xs, boot.reps= boot.reps, seed= seed, set.seed= set.seed, shape1= shape1, shape2= shape2, lowlim= lowlim )

  qual[1:10]
  qual$method

  # Transformed vectors
  f.tr <- qual$f.tr
  b.tr <- qual$b.tr

  # Print the vectors
  print(paste0("b=", paste(b, collapse = " ")))
  print(paste0("b.tr=", paste(b.tr, collapse = " ")))
  print(paste0("f=", paste(f, collapse = " ")))
  print(paste0("f.tr=", paste(f.tr, collapse = " ")))


  # Fitted pdf
  pdf.b <- qual$pdf.b
  print.pdf.b <- FALSE
  if (length(xs) == length(pdf.b)) print.pdf.b <- TRUE
  pdf.f <- qual$pdf.f
  print.pdf.f <- FALSE
  if (length(xs) == length(pdf.f)) print.pdf.f <- TRUE

  # Print the pdf
  print(paste0("pdf.b=", paste(pdf.b, collapse = " ")))
  print(paste0("pdf.f=", paste(pdf.f, collapse = " ")))
  print(paste0("xs=", paste(xs, collapse = " ")))

  # Means test legend
  f.mean <- qual$f.mean
  b.mean <- qual$b.mean
  f.mean.txt <- specify_decimal(f.mean, 3)
  b.mean.txt <- specify_decimal(b.mean, 3)
  means.leg <- paste0("(b.mean=", b.mean.txt, ", f.mean=", f.mean.txt, ")")
  tr.method.leg <- qual$beta.tr.method

  # Overlap legend
  q.ol <- qual$q.ol
  if (is.na(q.ol) || is.null(q.ol)) {
    q.ol.leg <- "Q.overlap =<NA>"
  } else {
    q.ol.txt <- specify_decimal(q.ol, 3)
    q.ol.leg <- paste0("Q.overlap=", q.ol.txt)
  }


  title <- paste0(q.ol.leg, "\n", means.leg, "\n", tr.method.leg)

  par(mfrow = c(2, 2), oma = c(0, 1, 5, 0))
  hist(b, prob = TRUE, breaks = seq(0, 1, l = 20))
  hist(f, prob = TRUE, breaks = seq(0, 1, l = 20))
  hist(b.tr, prob = TRUE, breaks = seq(0, 1, l = 20))
  if (print.pdf.b == TRUE) lines(xs, pdf.b, col = "red", lwd = 1)
  hist(f.tr, prob = TRUE, breaks = seq(0, 1, l = 20))
  if (print.pdf.f == TRUE) lines(xs, pdf.f, col = "red", lwd = 1)
  mtext(title, outer = TRUE, cex = 1)
}

# reloiad functions
# BASEDIR <- "/Users/bboyle/Documents/hga/teck/vqa/scripts/analysis/"
INCLUDESDIR <- paste0(BASEDIR, "includes/")
source(paste(INCLUDESDIR, "functions.R", sep = ""), local = TRUE)

# Render the graph
demo(f, b, xs, boot.reps, seed, set.seed, shape1, shape2, lowlim)

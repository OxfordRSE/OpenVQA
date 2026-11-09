# For testing performance of beta quality function at handling uniform vectors
# of extreme values at edge of distribution

# Generic beta fitting parameters
xs <- seq(0, 1, length.out = 100)
lowlim <- 0.0000001
shape1 <- 0.1
shape2 <- 0.1

BASEDIR <- "/Users/bboyle/Documents/hga/teck/vqa/scripts/analysis/"
INCLUDESDIR <- paste0(BASEDIR, "includes/")
for (helper_module in c(
  "quality", "distribution", "overlap", "bootstrap", "utility",
  "formatting", "spreadsheet", "plotting"
)) {
  source(paste0(INCLUDESDIR, helper_module, "-functions.R"), local = TRUE)
}

# Transfomation functions
logit <- function(x) {
  log(x / (1 - x))
}
noise.beta.norm <- function(x, n, sd) {
  1 / (1 + exp(-rnorm(n, mean = logit(x), sd = sd)))
}
noise.beta <- function(x, n, sd) {
  rbeta(n, mean = logit(x), sd = sd)
}
sd <- 0.5

# plotting functions
hist.beta <- function(x) {
  hist(x,
    xlim = c(0, 1)
  )
}
hist.beta.br <- function(x) {
  hist(x,
    xlim = c(0, 1),
    breaks = c(
      0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8,
      0.85, 0.9, 0.95, 1
    )
  )
}
hist.beta.br2 <- function(x) {
  hist(x,
    xlim = c(0, 1),
    breaks = c(
      0, 0.025, 0.050, 0.075, 0.100, 0.125, 0.150, 0.175, 0.200,
      0.225, 0.250, 0.275, 0.300, 0.325, 0.350, 0.375, 0.400, 0.425, 0.450,
      0.475, 0.500, 0.525, 0.550, 0.575, 0.600, 0.625, 0.650, 0.675, 0.700,
      0.725, 0.750, 0.775, 0.800, 0.825, 0.850, 0.875, 0.900, 0.925, 0.950, 0.975, 1.000
    )
  )
}
histfit <- function(x, fit) {
  hist(x, xlim = c(0, 1))
  lines(density(fit))
}


zeros <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
ones <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
small <- c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)
big <- c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99)
smaller <- c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001)
bigger <- c(0.999, 0.999, 0.999, 0.999, 0.999, 0.999, 0.999, 0.999, 0.999, 0.999, 0.999, 0.999)

f <- zeros
b <- ones

n.f <- length(f)
n.b <- length(b)

# Test effects of transformations
f
f.tr <- beta.transform(f)
f.tr
f.tr2 <- jitter.beta(f)
f.tr2
f.tr3 <- jitter.beta(f.tr)
f.constr <- beta.constrain(f)
f.constr
f.tr4 <- noise.beta(f.constr, n, sd)
f.tr4

hist.beta(f)
hist.beta(f.tr)
hist.beta(f.tr2)
hist.beta(f.tr3)
hist.beta(f.tr4)


fit <- fit.beta(f.tr, xs = xs, shape1 = shape1, shape2 = shape2, lowlim = lowlim, boot.cls = F)

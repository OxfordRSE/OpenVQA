####################################################
# Demonstration: Monte carlo estimation of overlap coefficient for
# two normal distributions
# (AKA numerical integration using monte carlo methods)
#
# Source:
# https://rpsychologist.com/calculating-the-overlap-of-two-normal-distributions-using-monte-carlo-integration
####################################################

# Numerical integration using monte carlo methods
set.seed(456456)
n <- 100000 # Number of random points to use

# Uses two standard normal distributions whose means differ
# by 0.8 (ie., Cohen's d = 0.8)

# Mean and SD of the two distributions
mu1 <- 0
sd1 <- 1
mu2 <- 0.8 # i.e. cohen's d = 0.8
sd2 <- 1

# The x-axis value
# +/- 3 SDs
xs <- seq(min(mu1 - 3 * sd1, mu2 - 3 * sd2), max(mu1 + 3 * sd1, mu2 + 3 * sd2), length.out = n)

# Exact y-axis values
# Used for plotting the distribution lines
# and setting upper limit when pulling the random values
f1 <- dnorm(xs, mean = mu1, sd = sd1) # dist1
f2 <- dnorm(xs, mean = mu2, sd = sd2) # dist2

# Random y-axis values
# Generate n uniform-random points within the domain defined
# by the range of x-values and y-value of both distributions
# I.e., sample x,y from uniform dist
ps <- matrix(c(runif(n, min(xs), max(xs)), runif(n, min = 0, max = max(f1, f2))), ncol = 2)

# Generate vectors indicating whether the random points
# fall under each distribution
z1 <- ps[, 2] <= dnorm(ps[, 1], mu1, sd1) # Falls under dist1
z2 <- ps[, 2] <= dnorm(ps[, 1], mu2, sd2) # Falls under dist2
z12 <- z1 | z2 # Falls under either dist1 or dist; i.e., union

# Fall under both dist1 & dist2; i.e., overlap
z3 <- ps[, 2] <= pmin(dnorm(ps[, 1], mu1, sd1), dnorm(ps[, 1], mu2, sd2))
z3 <- z1 & z2 # Simpler method

# Calculate OVL
# This is the generic solution, uses average of overlap with
# dist1 & dist2
ovl <- (sum(z3) / sum(z1) + sum(z3) / sum(z2)) / 2

##################################
# Make the plot
##################################

# All random points (for comparison only)
plot(ps, col = "#137072", pch = 20, ylim = c(0, max(f1, f2)), xlim = range(xs), xlab = "", ylab = "")

# All points minus the joint distributions
plot(ps[!z12, 1], ps[!z12, 2], col = "#137072", pch = 20, ylim = c(0, max(f1, f2)), xlim = range(xs), xlab = "", ylab = "")

points(ps[z1, 1], ps[z1, 2], col = "#FBFFC0") # Dist1 points
points(ps[z2, 1], ps[z2, 2], col = "#56B292") # Dist2 points

# Plot lines of the actual distributions
lines(xs, f1, lwd = 2)
lines(xs, f2, lty = "dotted", lwd = 2)

# Overlap points
points(ps[z3, 1], ps[z3, 2], col = "#BF223D") # Overlap points


# Add overlap legend
ovl.text <- format(round(ovl, 2), nsmall = 2)
leg.x <- min(xs) + 0.5 * (max(xs) - min(xs))
leg.y <- 0.12 * max(f1, f2)
text(leg.x, leg.y, paste0("OVL=", ovl.text), col = "#FBFFC0", cex = 1.5)

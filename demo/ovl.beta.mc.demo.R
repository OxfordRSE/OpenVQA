####################################################
# Demonstration: Monte carlo estimation of overlap coefficient for
# two beta distributions
#
# Based on:
# https://rpsychologist.com/calculating-the-overlap-of-two-normal-distributions-using-monte-carlo-integration
####################################################

# Randomization parameters
set.seed(456456)
n <- 100000 # Number of random points to use

# Position of legend text on standardized grid: x=[0,1] & y=[0,1]
leg.x.std <- 0.5
leg.y.std <- 0.8

# Alpha (a, shape1) and beta (b, shape2) parameters of the two distributions
a1 <- 2
b1 <- 5
# a1 <- 1
# b1 <- 3
# a1 <- 0.5
# b1 <- 0.5
a2 <- 2
b2 <- 2
# a2 <- 5
# b2 <- 1

# The x-axis value
xs <- seq(0, 1, length.out = n)

# Exact y-axis values
# Used for plotting the distribution lines
# and setting upper limit when pulling the random values
f1 <- dbeta(xs, shape = a1, shape2 = b1) # dist1
f2 <- dbeta(xs, shape = a2, shape2 = b2) # dist2

# Random y-axis values
# Generate n uniform-random points within the domain defined
# by the range of x-values and y-value of both distributions
# I.e., sample x,y from uniform dist
ps <- matrix(c(runif(n, min(xs), max(xs)), runif(n, min = 0, max = max(f1, f2))), ncol = 2)

# Generate vectors indicating whether the random points
# fall under each distribution
z1 <- ps[, 2] <= dbeta(ps[, 1], shape = a1, shape2 = b1) # Falls under dist1
z2 <- ps[, 2] <= dbeta(ps[, 1], shape = a2, shape2 = b2) # Falls under dist2

# Fall under both dist1 & dist2; i.e., overlap
z3 <- z1 & z2

# Calculate OVL
# This is the generic solution, uses average of overlap with
# dist1 & dist2
ovl <- (sum(z3) / sum(z1) + sum(z3) / sum(z2)) / 2

##################################
# Make the plot
##################################

# All random points
plot(ps, col = "#137072", pch = 20, ylim = c(0, max(f1, f2)), xlim = range(xs), xlab = "", ylab = "")

# Points under distributions 1 & 2
points(ps[z1, 1], ps[z1, 2], col = "#FBFFC0") # Dist1 points
points(ps[z2, 1], ps[z2, 2], col = "#56B292") # Dist2 points

# Add overlap points
points(ps[z3, 1], ps[z3, 2], col = "#BF223D") # Overlap points

# Lines of the actual distributions
lines(xs, f1, lwd = 2)
lines(xs, f2, lty = "dotted", lwd = 2)

# Add overlap legend
ovl.text <- format(round(ovl, 2), nsmall = 2)
leg.x <- min(xs) + leg.x.std * (max(xs) - min(xs))
leg.y <- leg.y.std * max(f1, f2)
text(leg.x, leg.y, paste0("OVL=", ovl.text), col = "#FBFFC0", cex = 1.5)

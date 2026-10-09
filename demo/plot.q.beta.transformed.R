#######################################################
# Plots beta quality discount function
#######################################################

#################
# Parameters
#################

n.x.vals <- 100 # vector of x values on 0:1
x.text <- "Difference from (fixed) benchmark"
y.text <- "Quality"
title <- "Quality score transformation function"

# Specific raw quality value
x <- 0.098
y <- 0.706

# Load functions
source("includes/functions.R")

#################
# Set x, y values
# and fit line
#################

x.vals <- seq(0, 1, length.out = n.x.vals)
q <- rev(q.scaled.beta(x.vals))
fit.line <- smooth.spline(x.vals, q, spar = 0.35)
# y <- q.scaled.beta(x)	# Fixed example value

#################
# Make the plot
#################

# par( oma=c( b, l, t, r ) )
par(oma = c(1, 1, 1, 1))

# The base plot
plot(x.vals, q,
  type = "n", # Hide the data points
  xlab = x.text,
  ylab = y.text,
  cex.lab = 1.5, cex.axis = 1.0,
  xaxs = "i", yaxs = "i" # Remove inner margins so lines reach to axes
)

# Fit line
lines(fit.line)

# vertical line
segments(x0 = x, y0 = 0, x1 = x, y1 = y, col = "red", lty = 2, lwd = 2)

# Horizontal line
segments(x0 = 0, y0 = y, x1 = x, y1 = y, col = "red", lty = 2, lwd = 2)

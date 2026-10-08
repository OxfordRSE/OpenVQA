# Tinkering with beta discount function
# for discounting q.dist

# Clear workspace
rm(list = ls())

#################################
# Parameters
# Each line is a set of parameters for one
# discount function
#################################
tr1.s1 <- 0.3
tr1.s2 <- 1 # Original transformation
tr2.s1 <- 0.3
tr2.s2 <- 2 # Deeper
tr3.s1 <- 0.5
tr3.s2 <- 1 # Shallower

#################################
# Generate the curves
#################################

# Original value of quality
q <- seq(0, 1, 0.01)

# Original value of proportional distance = (abs(f-b) / max(1-b,b))
pd <- seq(1, 0, -0.01)

# Transformed quality
q.tr1 <- qbeta(q, tr1.s1, tr1.s2) # Original transformation
q.tr2 <- qbeta(q, tr2.s1, tr2.s2)
q.tr3 <- qbeta(q, tr3.s1, tr3.s2)

#################################
# Plot Q transformed versus Q
#################################

op <- par(mar = c(5, 5, 4, 2) + 0.1) # Increase left & bottom margins

# 1:1 line
plot(q, q,
  type = "l", col = "red",
  main = "Quality discount functions",
  ylab = "Discounted Q.means",
  xlab = "Q.means",
  cex.lab = 1.3
)

# Transformations
lines(q, q.tr1, col = "green")
lines(q, q.tr2, col = "blue")
lines(q, q.tr3, col = "orange")

# Legend
legend("topleft",
  inset = .05,
  legend = c("No discount", "Shallow", "Moderate", "Deep"),
  col = c("red", "orange", "green", "blue"), ,
  lty = 1, cex = 1
)

par(op) # Reset the margins


# #################################
# Plot Q versus proportional difference
#################################

op <- par(mar = c(7, 5, 4, 2) + 0.1) # Increase lower margin

# 1:1 line
plot(pd, q,
  type = "l", col = "red",
  main = "Quality discount functions",
  ylab = "Discounted Q.means",
  xlab = "",
  cex.lab = 1.3
)

# Custom x label to control distance from axis
title(xlab = "Relative difference, focal vs. benchmark means\n ( abs(f-b) / max(b,1-b) )", line = 5, cex.lab = 1.3)

# Transformations
lines(pd, q.tr1, col = "green")
lines(pd, q.tr2, col = "blue")
lines(pd, q.tr3, col = "orange")

# Legend
legend("topright",
  inset = .05,
  legend = c("No discount", "Shallow", "Moderate", "Deep"),
  col = c("red", "orange", "green", "blue"), ,
  lty = 1, cex = 1
)

par(op) # Reset the margins

##################################
# Plot Q versus proportional difference
# Single algorithm only
#################################

# Set algorithm here
q.disc <- q.tr1

op <- par(mar = c(7, 5, 4, 2))

plot(pd, q.disc,
  type = "l", col = "black",
  main = "Quality discount function",
  ylab = "Quality (Q)",
  xlab = "",
  cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.5, cex.sub = 1.5,
  frame.plot = FALSE
)

# add xample point and x,y lines
x <- 0.08
y <- 0.75734
segments(x, 0, x, y, lty = 3)
segments(0, y, x, y, lty = 3)
points(x, y, pch = 20, cex = 2)

# Custom x label to control distance from axis
# title(xlab="Relative difference, focal vs. benchmark means\n ( abs(f-b) / max(b,1-b) )", line=5, cex.lab=1.3 )
title(xlab = "Difference, focal mean vs. benchmark", line = 5, cex.lab = 1.5)

par(op) # Reset the margins

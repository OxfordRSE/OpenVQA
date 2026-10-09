# Tinkering with beta discount function
# for discounting q.dist

# Clear workspace
rm(list = ls())

# Load functions
BASEDIR <- "/Users/bboyle/Documents/hga/teck/vqa/scripts/analysis/"
INCLUDESDIR <- paste0(BASEDIR, "includes/")
source(paste(INCLUDESDIR, "functions.R", sep = ""), local = TRUE)

#################################
# Generate the curves
#################################

# Original value of quality
q <- seq(0, 1, 0.01)
# q <- seq(0,1,0.05)

# Shape parameters
# k1 <- 10
# k2 <- 5
# k3 <- 15
b1 <- 1.5
b2 <- 2
b3 <- 3

# Transformed quality
# q.tr1 <- q.logistic(q, k1)
# q.tr2 <- q.logistic(q, k2)
# q.tr3 <- q.logistic(q, k3)
q.tr1 <- q.inverse.logit(q, b1)
q.tr2 <- q.inverse.logit(q, b2)
q.tr3 <- q.inverse.logit(q, b3)

# Legends for the above transformations
# tr1.leg <- paste0("k=",k1)
# tr2.leg <- paste0("k=",k2)
# tr3.leg <- paste0("k=",k3)
tr1.leg <- paste0("beta=", b1)
tr2.leg <- paste0("beta =", b2)
tr3.leg <- paste0("beta =", b3)

#################################
# Plot Q transformed versus Q
#################################

op <- par(mar = c(5, 5, 4, 2) + 0.1) # Increase left & bottom margins

# 1:1 line
plot(q, q,
  type = "l", col = "black",
  main = "Quality discount functions",
  ylab = "Discounted Q.means",
  xlab = "Q.means",
  cex.lab = 1.3
)

# Transformations
lines(q, q.tr1, col = "red")
lines(q, q.tr2, col = "green")
lines(q, q.tr3, col = "blue")

# Legend
legend("topleft",
  inset = .05,
  legend = c(tr1.leg, tr2.leg, tr3.leg),
  col = c("red", "green", "blue"), ,
  lty = 1, cex = 1
)

par(op) # Reset the margins

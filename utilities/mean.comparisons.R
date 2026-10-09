# 1. Define Mean Functions
# Standard Arithmetic Mean
arithmetic_mean <- function(x) mean(x)

# Geometric Mean (Note: returns 0 if any x is 0)
geometric_mean <- function(x) {
  if (any(x <= 0)) {
    return(0)
  }
  exp(mean(log(x)))
}

# Generalized Mean (Power Mean)
# p = 1: Arithmetic Mean
# p -> 0: Geometric Mean
# p = -1: Harmonic Mean
generalized_mean <- function(x, p) {
  if (p == 0) {
    return(geometric_mean(x))
  }
  (mean(x^p))^(1 / p)
}

# 2. Compare scenarios
data_list <- list(
  "High with one low" = c(0.9, 0.8, 0.9, 0.2),
  "High with one zero" = c(0.9, 0.8, 0.9, 0),
  "High with two low" = c(0.9, 0.8, 0.2, 0.2),
  "Low with one zero" = c(0.2, 0.2, 0.1, 0),
  "High with two zeros" = c(0.9, 0.8, 0, 0),
  "High with three zeros" = c(0.9, 0, 0, 0),
  "All zeros" = c(0, 0, 0, 0)
)
data_list <- list(
  "Two perfect, one low" = c(1, 1, 0.2),
  "Two high, one low" = c(0.9, 0.9, 0.2),
  "One high, two low" = c(0.9, 0.2, 0.2),
  "Two high, one zero" = c(0.9, 0.8, 0),
  "Two low, one zero" = c(0.2, 0.2, 0),
  "One high, two zeros" = c(0.9, 0, 0),
  "All zeros" = c(0, 0, 0)
)

# 3. Run Comparison with Input Display
results <- lapply(names(data_list), function(name) {
  x <- data_list[[name]]
  data.frame(
    Scenario = name,
    Values = paste(x, collapse = ", "), # Echos the inputs
    Arithmetic = round(arithmetic_mean(x), 3),
    GenMean_p0.5 = round(generalized_mean(x, 0.5), 3),
    GenMean_p0.3 = round(generalized_mean(x, 0.3), 3),
    GenMean_p0.1 = round(generalized_mean(x, 0.1), 3),
    Geometric = round(geometric_mean(x), 3),
    Harmonic_mean = round(generalized_mean(x, -1), 3)
  )
})

# Combine and print
comparison_table <- do.call(rbind, results)
print(comparison_table, row.names = FALSE)

##########################################
# Plot the behavior of each method using
# an input vector of three values,
# where...


##########################################

library(ggplot2)
library(tidyr)

# 1. Setup the data
x_varying <- seq(0, 1, length.out = 200)
fixed_vals <- c(0.9, 0.8)

# 2. Define the mean functions
generalized_mean <- function(x, p) {
  if (p == 0) {
    return(exp(mean(log(x))))
  }
  (mean(x^p))^(1 / p)
}

# 3. Calculate values for each method
plot_data <- data.frame(x = x_varying)
plot_data$Arithmetic <- sapply(x_varying, function(v) mean(c(fixed_vals, v)))
plot_data$Geometric <- sapply(x_varying, function(v) {
  vals <- c(fixed_vals, v)
  if (any(vals <= 0)) 0 else generalized_mean(vals, 0)
})
plot_data$GenMean_p0.5 <- sapply(x_varying, function(v) generalized_mean(c(fixed_vals, v), 0.5))
plot_data$GenMean_p0.4 <- sapply(x_varying, function(v) generalized_mean(c(fixed_vals, v), 0.4))
plot_data$GenMean_p0.3 <- sapply(x_varying, function(v) generalized_mean(c(fixed_vals, v), 0.3))
plot_data$GenMean_p0.1 <- sapply(x_varying, function(v) generalized_mean(c(fixed_vals, v), 0.1))

# 4. Reshape for ggplot and Plot
plot_data_long <- pivot_longer(plot_data, cols = -x, names_to = "Method", values_to = "Result")

ggplot(plot_data_long, aes(x = x, y = Result, color = Method)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Mean Sensitivity to Low Values",
    subtitle = "Fixed values: 0.9, 0.8 | Varying third value from 0 to 1",
    x = "Third Indicator Value", y = "Aggregated Score"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("black", "red", "yellow", "green", "blue", "orange"))

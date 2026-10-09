# ==============================================================================
# OPTION 1: USER-DEFINED PARAMETER
# ==============================================================================
# Change this value to automatically update the function, labels, and legend
power_p <- 0.3

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Setup demo data
n <- 25
a <- rep(1, n)
b <- rep(1, n)
c_vals <- seq(0, 1, length.out = n)

# Define functions for the means
geom_mean <- function(x) {
  prod(x)^(1 / length(x))
}

power_mean <- function(x, p) {
  mean(x^p)^(1 / p)
}

# Compute the means row-wise
data_means <- data.frame(a = a, b = b, c = c_vals) %>%
  rowwise() %>%
  mutate(
    Arithmetic = mean(c(a, b, c)),
    Geometric = geom_mean(c(a, b, c)),
    # Dynamically apply the parameter and name the column
    Power_Mean = power_mean(c(a, b, c), p = power_p)
  ) %>%
  ungroup()

# Create dynamic label for the legend using your parameter
power_label <- paste0("Power (p=", power_p, ")")

# Reshape data into long format and rename the factor level dynamically
data_long <- data_means %>%
  select(c, Arithmetic, Geometric, Power_Mean) %>%
  pivot_longer(cols = -c, names_to = "Mean_Type", values_to = "Mean_Value") %>%
  mutate(Mean_Type = recode(Mean_Type, "Power_Mean" = power_label))

# Ensure custom ordering in the legend
data_long$Mean_Type <- factor(data_long$Mean_Type, levels = c("Arithmetic", "Geometric", power_label))

# ==============================================================================
# BUILD THE PLOT (WITH OPTIONS 2 & 3)
# ==============================================================================
ggplot(data_long, aes(x = c, y = Mean_Value, color = Mean_Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = setNames(
    c("#E41A1C", "#377EB8", "#4DAF4A"),
    c("Arithmetic", "Geometric", power_label)
  )) +
  labs(
    title = "Comparison of Pythagorean and Power Means",
    subtitle = paste0("Calculated across vectors a=1, b=1, and c varying from 0 to 1"),
    x = "Value of vector c",
    y = "Mean Value",
    color = "Type of Mean"
  ) +
  # OPTION 2: Bounding box, tick marks, and zero gridlines
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(), # Removes major grid lines
    panel.grid.minor = element_blank(), # Removes minor grid lines
    axis.ticks = element_line(color = "black"), # Forces visible tick marks
    axis.ticks.length = unit(0.2, "cm"),

    # OPTION 3: Legend placed inside the lower-right quadrant
    legend.position = c(0.95, 0.05), # Anchor coordinate close to bottom-right corner
    legend.justification = c("right", "bottom"), # Aligns the legend box to that anchor point
    legend.direction = "vertical", # Arranges options vertically
    legend.box.just = "left", # Left-justifies the legend content inside its box
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5), # Optional box around legend

    # Title formatting
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

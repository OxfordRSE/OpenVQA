##################################################################
# Test of modified geometric mean function that prevents single
# zero values from dropping the geometric mean to zero.
# Specialized for values distributed on [0:1], such as quality.
##################################################################

# Offset for modifict gmean function
offset <- 0.1


# This is my usual custom geometric mean function used for VQA
# Does not include zero correction
gmean <- function(x, na.rm = TRUE, na.weighted = FALSE, no.zero = FALSE, offset = 0.001) {
  ######################################
  # Calculates geometric mean
  #
  # Based on:
  # https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
  # But with two critical differences:
  # NA, if not removed, always return NA for whole vector
  # O, if present, always returns 0 for entire vector
  #
  # Parameters:
  #   na.rm: if FALSE and NAs present, returns NA
  #   na.weighted: remove NA from calculation, but weight by
  #     total observations, including NAs (in denominator)
  #
  # IMPORTANT! For VQA, use defaults
  # na.rm=TRUE, na.weighted=FALSE
  ######################################

  # Return NA right away if na.rm=FALSE and NAs present
  if (!na.rm && any(is.na(x))) {
    return(NA)
  }

  # Return NaN if any negative numbers
  if (any(x < 0, na.rm = na.rm)) {
    return(NaN)
  }

  # if(any(x==0, na.rm=TRUE)){
  #   return(0)
  # }

  if (na.weighted) {
    if (any(x == 0, na.rm = TRUE)) {
      return(0)
    } else {
      exp(sum(log(x), na.rm = na.rm) / length(x))
    }
  } else {
    if (no.zero) {
      # Modified geometric mean
      # Adds, then subtracts, small offset to prevent
      # one or more x=0 dropping all to zero
      exp(mean(log(x + offset), na.rm = na.rm)) - offset
    } else {
      # Normal geometric mean
      # Returns zero if any one x=0
      if (any(x == 0, na.rm = TRUE)) {
        return(0)
      } else {
        exp(mean(log(x), na.rm = na.rm))
      }
    }
  }
}


# Function to calculate Geometric Mean with a zero-handling offset
gmean_nozero <- function(x) {
  # Add the offset, calculate mean of logs, exponentiate, then subtract offset
  offset <- 0.001
  exp(mean(log(x + offset))) - offset
}

# --- Examples ---

# 1. High-performing indicators
indicators_high <- c(0.9, 0.85, 0.95)

# 2. Indicators with one "Weak Link"
indicators_1_weak <- c(0.9, 0.1, 0.95)

# 2. Indicators all weak
indicators_all_weak <- c(0.2, 0.1, 0.05)

# 4. Strong indicators with one zero
indicators_high_zero <- c(0.9, 0.0, 0.95, 0.95, 0.95)

# 4. Weak indicators with one zero
indicators_low_zero <- c(0.2, 0.0, 0.1)


# Results
cat("Gmean offset: ", offset, "\n", sep = "")

cat("High indicators: ", indicators_high, "\n")
cat("- Arithmetic: ", mean(indicators_high), "\n")
cat("- Geometric: ", gmean(indicators_high), "\n")
cat("- Geometric zero offset (gmean): ", gmean(indicators_high, no.zero = TRUE), "\n")
cat("- Geometric zero offset (gmean_nozero): ", gmean_nozero(indicators_high), "\n")

cat("One low indicator: ", indicators_1_weak, "\n")
cat("- Arithmetic: ", mean(indicators_1_weak), "\n")
cat("- Geometric: ", gmean(indicators_1_weak), "\n")
cat("- Geometric zero offset (gmean): ", gmean(indicators_1_weak, no.zero = TRUE), "\n")
cat("- Geometric zero offset (gmean_nozero): ", gmean_nozero(indicators_1_weak), "\n")

cat("All indicators low: ", indicators_all_weak, "\n")
cat("- Arithmetic: ", mean(indicators_all_weak), "\n")
cat("- Geometric: ", gmean(indicators_all_weak), "\n")
cat("- Geometric zero offset (gmean): ", gmean(indicators_all_weak, no.zero = TRUE, offset = offset), "\n")
cat("- Geometric zero offset (gmean_nozero): ", gmean_nozero(indicators_all_weak), "\n")

cat("One zero indicator, others high: ", indicators_high_zero, "\n")
cat("- Arithmetic: ", mean(indicators_high_zero), "\n")
cat("- Geometric: ", gmean(indicators_high_zero), "\n")
cat("- Geometric zero offset (gmean): ", gmean(indicators_high_zero, no.zero = TRUE), "\n")
cat("- Geometric zero offset (gmean_nozero): ", gmean_nozero(indicators_high_zero), "\n")

cat("One zero indicator, others low: ", indicators_low_zero, "\n")
cat("- Arithmetic: ", mean(indicators_low_zero), "\n")
cat("- Geometric: ", gmean(indicators_low_zero), "\n")
cat("- Geometric zero offset (gmean): ", gmean(indicators_low_zero, no.zero = TRUE), "\n")
cat("- Geometric zero offset (gmean_nozero): ", gmean_nozero(indicators_low_zero), "\n")

#####################################################
#####################################################
# Test behaviour of function gmean (see functions.R)
#####################################################
#####################################################

gmean <- function(x, na.rm = TRUE, na.weighted = FALSE) {
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
  #   na.rm: if TRUE and NAs present, returns NA
  #   na.weighted: remove NA from calculation, but include number of NAs in denominator
  ######################################

  # Return NA right away if na.rm=FALSE and NAs present
  if (!na.rm && any(is.na(x))) {
    return(NA)
  }

  # Return NaN if any negative numbers
  if (any(x < 0, na.rm = na.rm)) {
    return(NaN)
  }

  if (any(x == 0, na.rm = na.rm)) {
    return(0)
  }

  if (na.weighted) {
    # exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
    exp(sum(log(x), na.rm = na.rm) / length(x))
  } else {
    exp(mean(log(x), na.rm = na.rm))
  }
}

#######################
# Vectors
#######################

Q.vec <- c(0.25, 0.5, 0.75)
Q.vec
gmean(Q.vec)
gmean(Q.vec, na.rm = TRUE, na.weighted = FALSE) # Defaults, should be same as above
gmean(Q.vec, na.rm = TRUE, na.weighted = TRUE)
gmean(Q.vec, na.rm = FALSE, na.weighted = FALSE)
gmean(Q.vec, na.rm = FALSE, na.weighted = TRUE) # Should be no different from preceding

Q.vec.na <- c(0.25, 0.5, 0.75, NA)
Q.vec.na
gmean(Q.vec.na)
gmean(Q.vec.na, na.rm = TRUE, na.weighted = FALSE) # Defaults, should be same as above
gmean(Q.vec.na, na.rm = TRUE, na.weighted = TRUE)
gmean(Q.vec.na, na.rm = FALSE, na.weighted = FALSE)
gmean(Q.vec.na, na.rm = FALSE, na.weighted = TRUE) # Should be no different from preceding

Q.vec0 <- c(0, 0.5, 1)
Q.vec0
gmean(Q.vec0)
gmean(Q.vec0, na.rm = TRUE, na.weighted = FALSE) # Defaults, should be same as above
gmean(Q.vec0, na.rm = TRUE, na.weighted = TRUE)
gmean(Q.vec0, na.rm = FALSE, na.weighted = FALSE)
gmean(Q.vec0, na.rm = FALSE, na.weighted = TRUE) # Should be no different from preceding

Q.vec0.na <- c(0, 0.5, 1, NA)
Q.vec0.na
gmean(Q.vec0.na)
gmean(Q.vec0.na, na.rm = TRUE, na.weighted = FALSE) # Defaults, should be same as above
gmean(Q.vec0.na, na.rm = TRUE, na.weighted = TRUE)
gmean(Q.vec0.na, na.rm = FALSE, na.weighted = FALSE)
gmean(Q.vec0.na, na.rm = FALSE, na.weighted = TRUE) # Should be no different from preceding


Q.vec.neg <- c(-0.25, 0.5, 0.75)
gmean(Q.vec.neg)
gmean(Q.vec.neg, na.rm = TRUE, na.weighted = FALSE) # Defaults, should be same as above
gmean(Q.vec.neg, na.rm = TRUE, na.weighted = TRUE)
gmean(Q.vec.neg, na.rm = FALSE, na.weighted = FALSE)
gmean(Q.vec.neg, na.rm = FALSE, na.weighted = TRUE) # Should be no different from preceding

Q.vec.neg.na <- c(-0.25, 0.5, 0.75, NA)
gmean(Q.vec.neg.na)
gmean(Q.vec.neg.na, na.rm = TRUE, na.weighted = FALSE) # Defaults, should be same as above
gmean(Q.vec.neg.na, na.rm = TRUE, na.weighted = TRUE)
gmean(Q.vec.neg.na, na.rm = FALSE, na.weighted = FALSE)
gmean(Q.vec.neg.na, na.rm = FALSE, na.weighted = TRUE) # Should be no different from preceding

Q.vec.neg.0 <- c(-0.25, 0.5, 0.75, 0)
gmean(Q.vec.neg.0)
gmean(Q.vec.neg.0, na.rm = TRUE, na.weighted = FALSE) # Defaults, should be same as above
gmean(Q.vec.neg.0, na.rm = TRUE, na.weighted = TRUE)
gmean(Q.vec.neg.0, na.rm = FALSE, na.weighted = FALSE)
gmean(Q.vec.neg.0, na.rm = FALSE, na.weighted = TRUE) # Should be no different from preceding

#######################
# Data frames
#######################

df <- data.frame(
  name = character(0),
  w = numeric(0),
  x = numeric(0),
  y = numeric(0),
  z = numeric(0)
)
df <- rbind(
  df,
  data.frame(
    name = c("Group1", "Group1", "Group2", "Group2", "Group3", "Group3", "Group3"),
    w = c(0.25, 0.50, 0.75, 0.2, 0.2, 0.1, 1),
    x = c(0.25, 0.50, 0.75, 0.2, 0.2, 0.1, 1),
    y = c(-0.25, 0.50, 0.75, 0.2, 0.2, 0.1, 0),
    z = c(0.25, NA, 0.75, 0.2, 0.2, 0.1, 1)
  )
)

df
gmean(df)

# Note that this function only works on vectors! To use on data frames, use aggregate:
cols <- c("w", "x", "y", "z")
df.gmeans <- aggregate(
  df[, c(cols)],
  by = list(df$name),
  FUN = c("gmean"),
  na.rm = TRUE
)
names(df.gmeans)[names(df.gmeans) == "Group.1"] <- "name"
df.gmeans

strict <- function(f, pos = 2) eval(substitute(f), as.environment(pos))

ei.params.global <- function(ei.name) {
  ##############################
  # Global version of project-specific function
  # in params.PROJECT_CODE.R
  # Returns parameters associated with
  # this indicator
  ##############################
  if (ei.name == "GC") {
    distn <- "Bet"
    ei.name <- "Ground Cover"
    q.method <- "empirical"
    has.stratum <- T
    test.tail <- "both"
    bm.val <- NA
  } else if (ei.name == "PCESS") {
    distn <- "Bet"
    ei.name <- "Percent Cover Exotic Species"
    q.method <- "empirical"
    has.stratum <- T
    test.tail <- "upper"
    bm.val <- 0
  } else if (ei.name == "PCGF") {
    distn <- "Bet"
    ei.name <- "Percent Cover by Growth Form"
    q.method <- "empirical"
    has.stratum <- T
    test.tail <- "both"
    bm.val <- NA
  } else if (ei.name == "PES") {
    distn <- "Bet"
    ei.name <- "Percent Exotic Species"
    q.method <- "empirical"
    has.stratum <- F
    test.tail <- "upper"
    bm.val <- NA
  } else if (ei.name == "SR") {
    distn <- "NBin"
    ei.name <- "Species Richness"
    q.method <- "empirical"
    has.stratum <- F
    test.tail <- "both"
    bm.val <- NA
  } else if (ei.name == "TD") {
    distn <- "gamma"
    ei.name <- "Taxonomic distance"
    q.method <- "empirical"
    has.stratum <- F
    test.tail <- "both"
    bm.val <- NA
  } else if (ei.name == "asc") {
    distn <- "NBin"
    ei.name <- "Abundance by Size Class"
    q.method <- "empirical"
    has.stratum <- TRUE
    test.tail <- "both"
    bm.val <- NA
  } else if (ei.name == "ba") {
    distn <- "gamma"
    ei.name <- "Basal Area"
    q.method <- "empirical"
    has.stratum <- FALSE
    test.tail <- "both"
    bm.val <- NA
  } else if (ei.name == "mccsc") {
    distn <- "NBin"
    ei.name <- "Mean Cone Count by Size Class"
    q.method <- "empirical"
    has.stratum <- TRUE
    test.tail <- "lower"
    bm.val <- NA
  } else if (ei.name == "mpck") {
    distn <- "Bet"
    ei.name <- "Mean % Canopy Kill"
    q.method <- "fixed"
    has.stratum <- FALSE
    test.tail <- "upper"
    bm.val <- 0
  } else if (ei.name == "pbi") {
    distn <- "Bet"
    ei.name <- "Percent Beetle-Infected Trees"
    q.method <- "fixed"
    has.stratum <- FALSE
    test.tail <- "upper"
    bm.val <- 0
  } else if (ei.name == "pbst") {
    distn <- "Bet"
    ei.name <- "Percent Bark-Stripped Trees"
    q.method <- "fixed"
    has.stratum <- FALSE
    test.tail <- "upper"
    bm.val <- 0
  } else if (ei.name == "pcnwbp") {
    distn <- "Bet"
    ei.name <- "Percent Cover Non-Whitebark Pine"
    q.method <- "fixed"
    has.stratum <- TRUE
    test.tail <- "upper"
    bm.val <- 0
  } else if (ei.name == "pinf") {
    distn <- "Bet"
    ei.name <- "Percent Infected Trees"
    q.method <- "fixed"
    has.stratum <- FALSE
    test.tail <- "upper"
    bm.val <- 0
  } else if (ei.name == "sa") {
    distn <- "NBin"
    ei.name <- "Seedling Abundance"
    q.method <- "empirical"
    has.stratum <- FALSE
    test.tail <- "lower"
    bm.val <- NA
  } else if (ei.name == "pcs") {
    distn <- "Bet"
    ei.name <- "Percent Cover by Stratum"
    q.method <- "empirical"
    has.stratum <- TRUE
    test.tail <- "both"
    bm.val <- NA
  } else if (ei.name == "vhud") {
    distn <- "gamma"
    ei.name <- "Vegetation Height Upper Decile"
    q.method <- "empirical"
    has.stratum <- FALSE
    test.tail <- "both"
    bm.val <- NA
  } else if (ei.name == "pcv") {
    distn <- "Bet"
    ei.name <- "Percent Cover Vegetation"
    q.method <- "empirical"
    has.stratum <- FALSE
    test.tail <- "both"
    bm.val <- NA
  } else {
    stop("ERROR: unknown EI (function ei.params.global)")
  }

  param.list <- list(distn, ei.name, q.method, has.stratum, test.tail, bm.val)
  names(param.list) <- c("distn", "ei.name", "q.method", "has.stratum", "test.tail", "bm.val")

  return(param.list)
}

fixZeros <- function(val) {
  #################################
  # Function for adjusting zero values
  # Accepts value, if value=0, add small amount
  # For single values only. Keeping for
  # backwards-compatibility.
  # For adjusting entire vectors and data
  # frames, set dfAdj0(), below.
  #################################

  tinyVal <- 0.000001
  newVal <- val
  if (val == 0) {
    newVal <- val + tinyVal
  }
  return(newVal)
}

logittr <- function(x) {
  #################################
  # logit transform for proportions
  # Accepts vector x, returns transformed vector of
  # same length
  #################################

  minVal <- min(x[x > 0])
  xtr <- x

  for (i in 1:length(x)) {
    prop <- x[i]
    if (prop == 0) {
      xtr[i] <- log(minVal * 0.000001)
    } else if (prop >= 1) {
      xtr[i] <- 0
    } else {
      xtr[i] <- log(prop / (1 - prop))
    }
  }
  return(xtr)
}

convert.magic <- function(obj, type) {
  ##############################
  # For bulk-converting df data types
  ##############################
  FUN1 <- switch(type,
    character = as.character,
    numeric = as.numeric,
    factor = as.factor
  )
  out <- lapply(obj, FUN1)
  as.data.frame(out)
}

move.col <- function(col.mv, col.next, n.cols) {
  ######################################
  # Rearranges vector of indexes, moving index
  # 'col.mv' before index 'col.next', where n.cols is
  # the total number of indexes. Assume indexes
  # are 1:n.cols. Use for rearranging columns
  # of data frames
  ######################################

  # Make the original vector
  cols <- 1:n.cols

  # Get tne new positions
  cols.a <- c(col.mv, col.next)
  cols.b <- cols[which(!cols %in% cols.a)]
  cols.first <- cols.b[cols.b < col.next]
  cols.last <- cols.b[cols.b > col.next]

  # Assemble & return the new vector
  cols.new <- c(cols.first, col.mv, col.next, cols.last)
  return(cols.new)
}

gmean.orig <- function(x, na.rm = TRUE, na.weighted = FALSE) {
  ######################################
  # Calculates geometric mean
  #
  # Original VQA version; retired 2026-02-11
  #
  # Based on:
  # https://stackoverflow.com/q/2602583
  # But with two critical differences:
  # NA, if not removed, always return NA for whole vector
  # O, if present, always returns 0 for entire vector
  #
  # Parameters:
  #   na.rm: if TRUE and NAs present, returns NA
  #   na.weighted: remove NA from calculation, but weight by
  #     total observations, including NAs (in denominator)
  #
  # IMPORTANT! For VQA, keep defaults to:
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

  if (any(x == 0, na.rm = TRUE)) {
    return(0)
  }

  if (na.weighted) {
    # exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x)) # Not needed; zeros handled above
    exp(sum(log(x), na.rm = na.rm) / length(x))
  } else {
    exp(mean(log(x), na.rm = na.rm))
  }
}

gmean <- function(x, na.rm = TRUE, na.weighted = FALSE, no.zero = TRUE) {
  ######################################
  # Calculates geometric mean
  #
  # Feature:
  # NA, if not removed, always return NA for whole vector
  # If no.zero=FALSE: any(x==0) returns 0 for entire vector
  # If no.zero=TRUE: any(x==0) returns non-zero mean,
  #   heavily down-weighted by low values of x.
  #
  # Parameters:
  #   na.rm: if FALSE and NAs present, returns NA
  #   na.weighted: remove NA from calculation, but weight by
  #     total observations, including NAs (in denominator)
  #
  # IMPORTANT! For VQA:
  # Use default na.rm=TRUE, na.weighted=FALSE, no.zero=TRUE
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
      offset <- 0.0001
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

# Generalized Mean (Power Mean)
# p = 1: Arithmetic Mean
# p -> 0: Geometric Mean
# p = -1: Harmonic Mean
# For VQA, default p=0.3 provides performance similar to
# geometric mean, but with less severe penalty for low
# values and without dropping to zero if one value is
# zero. Hence the default value of p
generalized_mean <- function(x, p = 0.3) {
  if (p == 0) {
    return(gmean(x, no.zero = FALSE))
  }
  (mean(x^p))^(1 / p)
}

df.reorder <- function(
  df = NULL, col.move = NULL, col.before = NULL, move.first = FALSE,
  move.last = FALSE
) {
  ####################################
  # Moves column col.move of data frame df to
  # position after col.before.
  # if move.first==TRUE, ignores col.before and
  # moves col.move to first column of df.
  # If move.last=TRUE, ignores both col.before
  # and move.first and move to last position
  ####################################

  # Extract data frame name for use in error messages
  df.name <- deparse(substitute(df))

  if (is.null(df)) stop("ERROR: parameter 'df' not supplied (fnc: df.cols.reorder)")
  if (is.null(col.move)) stop("ERROR: parameter 'col.move' not specified (fnc: df.cols.reorder)")
  if (!col.move %in% colnames(df)) {
    stop(paste0(
      "ERROR: column '", col.move, "' not in data frame '", df.name,
      "', (fnc: df.cols.reorder)"
    ))
  }

  if (move.first == TRUE) {
    df <- df[, c(which(colnames(df) == col.move), which(colnames(df) != col.move))]
  } else if (move.last == TRUE) {
    df <- df[, c(which(colnames(df) != col.move), which(colnames(df) == col.move))]
  } else {
    # Check for missing parameters
    if (is.null(col.before)) {
      stop(
        "ERROR: parameter 'col.before' not specified (fnc: df.cols.reorder)"
      )
    }
    if (!col.before %in% colnames(df)) {
      stop(paste0(
        "ERROR: column '", col.before, "' not in data frame '", df.name,
        "', (fnc: df.cols.reorder)"
      ))
    }

    # Get index of column to move
    col.move_idx <- which(colnames(df) == col.move)

    # Get indexes of columns before and columns after
    col.before_idx <- which(colnames(df) == col.before)
    col.after_idx <- col.before_idx + 1
    cols.before_idx <- setdiff((1:col.before_idx), col.move_idx)
    cols.after_idx <- setdiff((col.after_idx:ncol(df)), col.move_idx)

    # Reorder columns
    df <- df[, c(cols.before_idx, col.move_idx, cols.after_idx)]
  }
  return(df)
}

is.num <- function(x) {
  ###############################################
  # Tests if x is a single number (integer or float)
  # Returns false if non-numeric or null or NA
  # Complex objects (vectors, dfs, lists, etc) also return false
  # Combine with exists() if object might not exist and don't
  # want to throw error
  ###############################################

  if (length(x) == 1 && is.numeric(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is.valid.lat <- function(lat) {
  is.valid <- FALSE

  if (suppressWarnings(!is.na(as.numeric(as.character(lat))))) {
    lat <- as.numeric(lat)

    if (lat >= -90 & lat <= 90) {
      is.valid <- TRUE
    }
  }

  return(is.valid)
}

is.valid.lon <- function(lon) {
  is.valid <- FALSE

  if (suppressWarnings(!is.na(as.numeric(as.character(lon))))) {
    lon <- as.numeric(lon)

    if (lon >= -180 & lon <= 180) {
      is.valid <- TRUE
    }
  }

  return(is.valid)
}

gcd <- function(lat1, lon1, lat2, lon2) {
  # Great circle distance between two pairs of coordinates
  # Accepts: coordinate in decimal degrees
  # Returns: distance in m

  if (is.valid.lat(lat1) && is.valid.lon(lon1) && is.valid.lat(lat2) && is.valid.lon(lon2)) {
    R <- 6371000 # Radius of earth in meters

    # Convert degrees to radians
    lat1 <- lat1 * pi / 180
    lon1 <- lon1 * pi / 180
    lat2 <- lat2 * pi / 180
    lon2 <- lon2 * pi / 180

    dlon <- lon2 - lon1
    dlat <- lat2 - lat1
    a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
    c <- 2 * asin(min(1, sqrt(a)))
    d <- R * c

    return(d)
  } else {
    return(NA)
  }
}

stop_quietly <- function(exit.msg = NULL) {
  # Workaround for idiotic inability of R to stop without throwing error
  # Modified from: https://stackoverflow.com/a/42945293/2757825
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))

  if (is.null(exit.msg)) {
    stop()
  } else {
    cat(exit.msg)
    stop()
  }
}

quiet <- function(code) {
  # Workaround for R's stupid lack of simple solution for
  # silently suppressing command output
  # Modified from: https://stackoverflow.com/a/49945753/2757825

  # Set correct null redirect for current operating system
  if (.Platform$OS.type == "windows") {
    nr <- "NUL"
  } else {
    # unix (also works on mac os)
    nr <- "/dev/null"
  }

  sink(nr)
  tmp <- code
  sink()
  return(tmp)
}

factors.to.chr <- function(df) {
  # Convert all factors in data frame to character
  # Source: https://stackoverflow.com/a/2853231/2757825
  i <- sapply(df, is.factor)
  df[i] <- lapply(df[i], as.character)
  return(df)
}

pk.uniq <- function(df, pk.col) {
  # Return TRUE if pk.col is unique throughout table,
  # otherwise return FALSE
  if (length(df[, pk.col]) == length(unique(df[, pk.col]))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

ls.df <- function() {
  # List data frames only
  ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(t) is.data.frame(get(t)))]
}

gmean.fix0 <- function(vec, tinyamt = 0.000001) {
  ################################################
  # Corrects exact zeros in a vector by adding
  # a very small number. Use to prepare a vector
  # before calculating geometric mean.
  # Geometric mean fails if any value=0
  ################################################

  if (!is.vector(vec, mode = "numeric")) {
    stop("Function vec_fix_exact_zeros() failed: object is not a numeric vector!\n")
  }

  for (i in length(vec)) {
    x <- vec[i]
    if (x == 0) vec[i] <- tinyamt
  }

  return(vec)
}

df.err.save <- function(df.err, err.file.name, err.file.path, msg.err = NULL, msg.action = NULL) {
  ####################################################
  # Save df of error information to CSV file and echo
  # latest error detected (msg.err) and action taken
  # (msg.action) in response to error.
  # Messages suppressed by setting message text to NULL
  ####################################################
  if (!is.null(msg.err)) cat(msg.err, "\n")
  if (!is.null(msg.action)) cat(msg.action, "\n")
  err.file <- paste0(err.file.path, err.file.name)
  write.csv(df.err, file = err.file, row.names = FALSE)
}

sorensen <- function(spp1, spp2) {
  # Calculate Sorensen overlap index between
  # two sets of species
  A <- length(spp1)
  B <- length(spp2)
  shared <- intersect(spp1, spp2)
  C <- length(shared)
  sorensen.index <- (2 * C) / (A + B)
  return(sorensen.index)
}

input_file_list <- function(
  df.landcover.name = DF.LANDCOVER, df.plotmetadata.name = DF.PLOTMETADATA,
  df.species.name = DF.SPECIES, ei.vec = EI.vec
) {
  ###########################################################
  # Prepare vector of required input files for this assessment
  #
  # * Prepare vector of standard VQA input data frames to be
  # generated for this project/assessment.
  # * Input options are general parameters set in params.R, and
  # should always be present
  # * Use df.input.list to test if required input files exist.
  ############################################################

  # These dfs are always required
  df.input.list.a <- c(df.landcover.name, df.plotmetadata.name, df.species.name)

  # The remainder vary by project & assessment
  for (i in 1:length(ei.vec)) {
    curr.ei <- ei.vec[i]
    df.input <- ei.params(curr.ei)$df.input
    if (i == 1) {
      df.input.list.b <- df.input
    } else {
      df.input.list.b <- c(df.input.list.b, df.input)
    }
  }
  df.input.list.b <- unique(df.input.list.b)
  df.input.list <- c(df.input.list.a, df.input.list.b)

  return(df.input.list)
}

input_file_list_all <- function(
  df.landcover.name = DF.LANDCOVER, df.plotmetadata.name = DF.PLOTMETADATA,
  df.species.name = DF.SPECIES, ei.vec = EI.vec
) {
  ###########################################################
  # Prepare list of vectors or required input files for this assessment
  #
  # * Similar to input_file_list(), but prepares a list of
  #   three vectors:
  #   [1] df.input.list.a: essential input files (always required)
  #   [2] df.input.list.b: assessment-specific input files
  #   [3] df.input.list: df.input.list.a & df.input.list.b combined
  ############################################################

  # These dfs are always required
  df.input.list.a <- c(df.landcover.name, df.plotmetadata.name, df.species.name)

  # The remainder vary by project & assessment
  for (i in 1:length(ei.vec)) {
    curr.ei <- ei.vec[i]
    df.input <- ei.params(curr.ei)$df.input
    if (i == 1) {
      df.input.list.b <- df.input
    } else {
      df.input.list.b <- c(df.input.list.b, df.input)
    }
  }
  df.input.list.b <- unique(df.input.list.b)
  df.input.list <- c(df.input.list.a, df.input.list.b)

  # Compile the final list
  df.input.list.comb <- list(
    df.input.list.a = df.input.list.a,
    df.input.list.b = df.input.list.b,
    df.input.list = df.input.list
  )

  return(df.input.list.comb)
}

transform_column <- function(column, mapping_vector) {
  ####################################################
  # Transform 0, 1 or more elements in a column by
  # matching to a mapping vector of the form:
  # mapping_vector <- c(
  #   "XXX" <- "YYY",
  #   "apples" <- "oranges"
  # )
  # Column elements that do not match are not changed
  ####################################################

  # Find column elements which exist in mapping_vector
  matches <- column %in% names(mapping_vector)

  # Replace only the matching elements
  column[matches] <- mapping_vector[column[matches]]

  return(column)
}

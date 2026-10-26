unix.friendly <- function(sometext) {
  #################################
  # Turn text into a unix-friendly file name
  #################################

  # Make unix-friendly version of stratum for filename
  str <- gsub(" ", "_", sometext)
  str <- gsub("/", "_", str)
  str <- gsub(",", "", str)
  str <- gsub("\\(", "", str)
  str <- gsub("\\)", "", str)
  str <- gsub("_-_", "-", str) # Remove redundant underscores+hyphens
  str <- gsub("]", "", str) # Remove square brackets
  str <- gsub("\\[", "", str) # Remove square brackets
  if (.Platform$OS.type == "windows") {
    str <- gsub('[<>:"|?*]', "_", str)
  }
  friendly <- tolower(str)

  return(friendly)
}

human.readable <- function(str) {
  ##############################
  # Teck-specific
  # Converts database land cover &
  # vegetation codes to something more
  # readable
  ##############################

  str2 <- gsub("v\\[", "", str)
  str2 <- gsub("]\\+s\\[", ", ", str2)
  str2 <- gsub("]\\+d\\[", ", ", str2)
  str2 <- gsub("]\\+r\\[", ", ", str2)

  return(str2)
}

human.readable.landcover <- function(str) {
  ##############################
  # Teck-specific
  # Converts database land cover to
  # something more readable
  ##############################

  str2 <- gsub("\\+d\\[mine]\\+r\\[r.pl]", " (reclaimed)", str)
  str2 <- gsub("\\+r\\[r.pl]", " (reclaimed)", str2)
  str2 <- gsub("\\+d\\[d]", " (disturbed)", str2)
  str2 <- gsub("\\+d\\[mine]", " (disturbed)", str2)
  str2 <- gsub("v\\[", "", str2)
  str2 <- gsub("]\\+s\\[", ", ", str2)
  str2 <- gsub("\\(disturbed) \\(reclaimed)", "(reclaimed)", str2)

  return(str2)
}

human.readable.bm.veg <- function(str) {
  ##############################
  # Teck-specific
  # Converts database benchmark
  # vegetation codes to something more
  # readable
  ##############################

  str2 <- gsub("v\\[", "", str)
  str2 <- gsub("]\\+s\\[", ", ", str2)

  return(str2)
}

reclaimed.make.pretty <- function(str) {
  ##############################
  # Teck-specific
  # Converts reclamation codes to something
  # more readable
  ##############################

  str2 <- gsub(", mine, r.pl", " (reclaimed)", str)
  str2 <- gsub(", mine, r", " (reclaimed)", str2)

  return(str2)
}

specify_decimal <- function(x, k) {
  # Set fixed number of decimals
  if (is.na(x) || is.null(x)) {
    x.formatted <- x
  } else {
    x.formatted <- format(round(x, k), nsmall = k)
  }
  return(x.formatted)
}

trim <- function(str) {
  ##############################
  # Trims leading and trailing whitespace
  ##############################
  str_trimmed <- gsub("^\\s+|\\s+$", "", str)
  return(str_trimmed)
}

replace.last <- function(old, new, str) {
  ####################################
  # Replaces last occurrence of pattern old
  # in target string str with pattern new
  # Sequence of parameters same as for R base
  # function gsub
  ####################################

  exp.old <- paste0(old, "([^", old, "]*)$")
  exp.new <- paste0(new, "\\1")
  str.new <- sub(exp.old, exp.new, str)

  return(str.new)
}

smartpaste <- function(..., sep = " ") {
  ###########################################
  # Pastes together elements using delimiter sep.
  # Differs from base function paste() in that delimiter is
  # omitted for elements that are empty strings
  # Similar to paste() in that single whitespace is appended
  # to delimiter. To omit padding, see smartpaste0().
  ###########################################

  # Pad original delimiter
  sep <- paste0(sep, " ")

  # Temporary delimiter to protect against edge case
  # where sep is also found in items to be concatenated
  delim <- "ExtremelyXXXUnlikelyYYYDelimiter"
  # delim <- paste0(delim," ")

  # Convert elements to be concatenated to delimited string
  # Unlisting & converting to character allows mixing of strings
  # with numeric and/or character vectors in input
  v <- list(...)
  v <- unlist(v, use.names = FALSE, recursive = TRUE)
  v <- as.character(v)
  bad <- paste(v, collapse = delim)

  # strip all internal orphan delimiters by pairs
  better <- bad
  while (grepl(paste0(delim, delim), better, fixed = TRUE)) {
    better <- gsub(paste0(delim, delim), delim, better)
  }

  # Remove initial orphan delimiter if any
  evenbetter <- sub(paste0("^", delim), "", better)

  # Remove trailing orphan delimiters if any
  best <- sub(paste0(delim, "$"), "", evenbetter)

  best <- gsub(delim, sep, best)

  return(best)
}

smartpaste0 <- function(..., sep = "") {
  ###########################################
  # Pastes together elements of vector v using delimiter sep.
  # Differs from base function paste0() in that delimiter is
  # omitted for elements that are empty strings
  # Similar to paste0() in that no whitespace is appended
  # to delimiter. To add padding, see smartpaste().
  ###########################################

  # Temporary delimiter to protect against edge case
  # where sep is also found in items to be concatenated
  delim <- "ExtremelyXXXUnlikelyYYYDelimiter"

  # Unlisting & converting to character allows mixing of strings
  # with numeric and/or character vectors in input
  v <- list(...)
  v <- unlist(v, use.names = FALSE, recursive = TRUE)
  v <- as.character(v)
  bad <- paste(v, collapse = delim)

  # strip all internal orphan delimiters by pairs
  better <- bad
  while (grepl(paste0(delim, delim), better, fixed = TRUE)) {
    better <- gsub(paste0(delim, delim), delim, better)
  }

  # Remove initial orphan delimiter if any
  evenbetter <- sub(paste0("^", delim), "", better)

  # Remove trailing orphan delimiters if any
  best <- sub(paste0(delim, "$"), "", evenbetter)

  best <- gsub(delim, sep, best)

  return(best)
}

capFirst <- function(s) {
  # Capitalize first letter
  # s can be a df column
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

lowerFirst <- function(s) {
  paste(tolower(substring(s, 1, 1)), substring(s, 2), sep = "")
}

pasteNA <- function(..., sep = ", ") {
  # Convert NA to empty string when paste-ing
  # Source: https://stackoverflow.com/a/15673180/2757825
  L <- list(...)
  L <- lapply(L, function(x) {
    x[is.na(x)] <- ""
    x
  })
  ret <- gsub(
    paste0("(^", sep, "|", sep, "$)"), "",
    gsub(
      paste0(sep, sep), sep,
      do.call(paste, c(L, list(sep = sep)))
    )
  )
  is.na(ret) <- ret == ""
  ret
}

smart.concat <- function(v1, v2, delim = ", ", empty.to.na = FALSE) {
  # Concatenate two vectors, strings, or mix of strings and vectors
  # with delimiter, treating NA as "" and omitting delimiter
  # when one or both vectors are NULL, NA or ""
  # Source:
  # https://stackoverflow.com/a/49201394/2757825

  v3 <- apply(cbind(v1, v2), 1, function(x) paste(x[!is.na(x)], collapse = delim))
  if (empty.to.na == TRUE) v3[v3 %in% ""] <- NA
  return(v3)
}

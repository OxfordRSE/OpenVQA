xlcolconv <- function(col) {
  ################################################
  # Convert Excel letter column names to numbers,
  # and vice versa
  # Source: https://stackoverflow.com/a/52214227/2757825
  ################################################

  # test: 1 = A, 26 = Z, 27 = AA, 703 = AAA
  if (is.character(col)) {
    # codes from https://stackoverflow.com/a/34537691/2292993
    s <- col
    # Uppercase
    s_upper <- toupper(s)
    # Convert string to a vector of single letters
    s_split <- unlist(strsplit(s_upper, split = ""))
    # Convert each letter to the corresponding number
    s_number <- sapply(s_split, function(x) {
      which(LETTERS == x)
    })
    # Derive the numeric value associated with each letter
    numbers <- 26^((length(s_number) - 1):0)
    # Calculate the column number
    column_number <- sum(s_number * numbers)
    return(column_number)
  } else {
    n <- col
    letters <- ""
    while (n > 0) {
      r <- (n - 1) %% 26 # remainder
      letters <- paste0(intToUtf8(r + utf8ToInt("A")), letters) # ascii
      n <- (n - 1) %/% 26 # quotient
    }
    return(letters)
  }
}

xl.col.wj <- function(
  curr.wb, curr.ws, curr.df,
  increment = 2, row.start = 1, row.end = 500
) {
  ##############################################################
  # Adaptively set column width and justification of Excel
  # workbook sheet
  #
  # Set width to widest string (data or header) plus increment.
  # Set justification left for test, right for numbers.
  # Apply styling from row.start to row.end
  # Required objects:
  #   wb: existing Excel workbook
  #   ws: existing worksheet in wb
  #   df: data frame
  # Dependencies:
  #   Package "openxlsx2"
  ##############################################################

  for (i in 1:ncol(curr.df)) {
    # Get Excel column name for this index
    curr.col.xl <- xlcolconv(i)

    # Set col width to widest string (including header) plus increment
    max.chars.data <- max(nchar(curr.df[, i]), na.rm = TRUE)
    chars.header <- nchar(colnames(curr.df)[i])
    curr.col.max.chars <- max(max.chars.data, chars.header, na.rm = TRUE)
    curr.col.wid <- curr.col.max.chars + increment

    # Apply column width setting
    curr.wb$set_col_widths(sheet = curr.ws, cols = c(i), widths = c(curr.col.wid))

    # Set justification: left for text, right for numeric data
    if (suppressWarnings(any(is.na(as.numeric(curr.df[, i]))))) {
      # Character; left justify
      cmd <- paste0(
        "curr.wb$add_cell_style( sheet='", curr.ws, "', dims='", curr.col.xl,
        row.start, ":", curr.col.xl, row.end, "', horizontal='left' )"
      )
    } else {
      # A number; right justify
      cmd <- paste0(
        "curr.wb$add_cell_style( sheet='", curr.ws, "', dims='", curr.col.xl,
        row.start, ":", curr.col.xl, row.end, "', horizontal='right' )"
      )
    }
    eval(parse(text = cmd))
  }
}

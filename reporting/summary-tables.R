format_table_columns <- function(table, precision) {
  for (column in names(precision)) {
    for (row in seq_len(nrow(table))) {
      table[[column]][row] <- specify_decimal(as.numeric(table[[column]][row]), precision[[column]])
    }
  }
  table
}

format_quality_interval <- function(value, lower, upper) {
  paste0(value, " [", lower, ", ", upper, "]")
}

build_summary_exports <- function(summary, indicator_details) {
  tables <- list()
  #######################################################
  # Makes restructured copies of selected VQA suymmary files
  # for easier reading and compilation into spreadsheets.
  # Prepares readable assessment summary tables
  #######################################################

  ##########################################
  # Set local parameters
  ##########################################

  # Input files
  file.qh.bm <- "summary_focal_qh_bm.csv"
  file.q.overall <- "summary_focal_q_qh.csv"
  file.q.fg <- "summary_focal_fg.csv"
  file.q.ei <- "summary_focal_ei.csv"

  # Output files, in same order
  file.qh.bm.out <- "vqa_summary_qh_bm.csv"
  file.q.overall.out <- "vqa_summary_q.overall.csv"
  file.q.fg.out <- "vqa_summary_q.fg.csv"
  file.q.ei.out <- "vqa_summary_q.ei.csv"

  # # Drop benchmark vegetation column (if redundant)
  # DROP.BM.COL <- TRUE

  ##########################################
  # Begin
  ##########################################

  cat("******************************************\n")
  cat("Exporting VQA results as CSV files\n\n")

  ##########################################
  # Overall quality results file
  ##########################################

  cat("Preparing file ", basename(file.q.overall), "...", sep = "")
  q.overall <- summary[[file.q.overall]]

  # Drop unwanted columns
  drop.cols <- c("bm.vegetation", "notes")
  q.overall <- q.overall[, !(names(q.overall) %in% drop.cols)]

  # Set decimal places
  q.overall <- format_table_columns(q.overall, c(
    q = 2, q.lcl = 2, q.ucl = 2, area_ha = 1,
    qh = 1, qh.lcl = 1, qh.ucl = 1
  ))

  # Write the summary file
  tables[[file.q.overall.out]] <- q.overall

  cat("done\n")

  ##########################################
  # Benchmark vegetation QH
  ##########################################

  cat("Preparing file ", basename(file.qh.bm.out), "...", sep = "")
  qh.bm <- summary[[file.qh.bm]]

  # Drop unwanted columns
  drop.cols <- c("notes")
  qh.bm <- qh.bm[, !(names(qh.bm) %in% drop.cols)]

  # Set decimal places
  qh.bm <- format_table_columns(qh.bm, c(area_ha = 2, qh = 2, qh.lcl = 3, qh.ucl = 3))

  # Write the summary file
  tables[[file.qh.bm.out]] <- qh.bm

  cat("done\n")

  ##########################################
  # Functional group quality results file
  ##########################################

  cat("Preparing file ", basename(file.q.fg), "...", sep = "")
  q.fg <- summary[[file.q.fg]]

  # Drop unwanted columns
  drop.cols <- c("bm.vegetation")
  q.fg <- q.fg[, !(names(q.fg) %in% drop.cols)]
  names(q.fg)[names(q.fg) == "q.fg"] <- "q"

  # Set decimal places
  q.fg <- format_table_columns(q.fg, c(q = 2, q.lcl = 2, q.ucl = 2))

  # Rename columns
  names(q.fg)[names(q.fg) == "category"] <- "functional.group"

  # Write the summary file
  tables[[file.q.fg.out]] <- q.fg

  cat("done\n")

  ##########################################
  # Indicator quality results file
  ##########################################

  cat("Preparing file ", basename(file.q.ei), "...", sep = "")

  q.ei <- summary[[file.q.ei]]
  q.ei <- q.ei[q.ei$include != FALSE, ]

  # Prune unnecessary stratum names
  q.ei$stratum <- as.character(q.ei$stratum)
  q.ei$stratum[q.ei$stratum == "nostrata"] <- ""
  # q.ei$stratum[ q.ei $EI == "PCESS" ] <- ""
  q.ei$stratum.prefix <- q.ei$stratum
  q.ei$stratum.prefix[q.ei$EI %in% c("PCESS", "GC", "PCGF")] <- "Percent Cover"

  # Make new combined EI+stratum column
  combine.cols <- c("stratum.prefix", "stratum")
  q.ei$ei.full <- apply(q.ei[, combine.cols], 1, paste, collapse = " ")
  q.ei <- q.ei[!is.na(q.ei$EI), ]
  q.ei <- within(q.ei, ei.full[stratum == ""] <- gsub("-", "", ei.full[stratum == ""]))

  # Rename columns
  names(q.ei)[names(q.ei) == "EI"] <- "indicator"
  names(q.ei)[names(q.ei) == "ei.full"] <- "indicator.stratum"
  names(q.ei)[names(q.ei) == "f.group"] <- "functional.group"
  names(q.ei)[names(q.ei) == "q.method"] <- "quality.method"
  names(q.ei)[names(q.ei) == "dist"] <- "distribution"
  names(q.ei)[names(q.ei) == "diff"] <- "means.diff"
  names(q.ei)[names(q.ei) == "q.diff.raw"] <- "q.means.raw"
  names(q.ei)[names(q.ei) == "p.diff"] <- "perm.test.sig"
  names(q.ei)[names(q.ei) == "q.diff"] <- "q.means"
  names(q.ei)[names(q.ei) == "ol"] <- "q.overlap"
  names(q.ei)[names(q.ei) == "ol.w"] <- "overlap.weight"

  # List columns to keep
  keep.cols <- c(
    "landcover",
    "bm.veg",
    "indicator",
    "stratum",
    "indicator.stratum",
    "functional.group",
    "quality.method",
    "distribution",
    "f.n",
    "f.mean",
    "b.n",
    "b.mean",
    "means.diff",
    "perm.test.sig",
    "q.means.raw",
    "q.means",
    "q.overlap",
    "overlap.weight",
    "q",
    "q.lcl",
    "q.ucl"
  )
  q.ei <- q.ei[, names(q.ei) %in% keep.cols]

  # Reorder columns
  q.ei <- df.reorder(q.ei, col.move = "indicator.stratum", col.before = "indicator")

  q.ei$p.diff <- q.ei$perm.test.sig
  q.ei$perm.test.sig <- as.character(q.ei$perm.test.sig)

  # Set decimal places
  q.ei <- format_table_columns(q.ei, c(
    q.means.raw = 2, q.means = 2, q.overlap = 2,
    overlap.weight = 3, q = 2, q.lcl = 2, q.ucl = 2
  ))
  for (i in 1:nrow(q.ei)) {
    if (!is.na(q.ei$p.diff[i])) {
      if (q.ei$p.diff[i] >= 0.5) {
        q.ei$perm.test.sig[i] <- "FALSE"
      } else {
        q.ei$perm.test.sig[i] <- "TRUE"
      }
    }
  }

  drop.cols <- c("p.diff")
  q.ei <- q.ei[, !(names(q.ei) %in% drop.cols)]

  # Add full name of indicator
  ei.details <- indicator_details
  ei.details <- ei.details[, c("ei", "ei.name")]
  q.ei <- merge(q.ei, ei.details, by.x = "indicator", by.y = "ei")
  q.ei <- df.reorder(q.ei, col.move = "ei.name", col.before = "indicator")

  # Write the summary file
  tables[[file.q.ei.out]] <- q.ei

  cat("done\n\n")

  tables
}

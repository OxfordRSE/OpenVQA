#######################################################
# Makes restructured copies of selected VQA suymmary files
# for easier reading and compilation into spreadsheets.
# Sourced by vqa.summary.R
#######################################################

##########################################
# Set local parameters
##########################################

# Input files
file.qh.bm <- paste0(RESULTSDIR, SUMMARY.FOCAL.BM.FILE)
file.q.overall <- paste0(RESULTSDIR, SUMMARY.FOCAL.FILE)
file.q.fg <- paste0(RESULTSDIR, SUMMARY.FOCAL.FG.FILE)
file.q.ei <- paste0(RESULTSDIR, SUMMARY.FOCAL.EI.FILE)

# Output files, in same order
file.qh.bm.out <- paste0(RESULTSDIR, FNAME.Q.BM.OUT)
file.q.overall.out <- paste0(RESULTSDIR, FNAME.Q.OVERALL.OUT)
file.q.fg.out <- paste0(RESULTSDIR, FNAME.Q.FG.OUT)
file.q.ei.out <- paste0(RESULTSDIR, FNAME.Q.EI.OUT)

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
q.overall <- read.csv(file.q.overall, header = T)

# Drop unwanted columns
drop.cols <- c("bm.vegetation", "notes")
q.overall <- q.overall[, !(names(q.overall) %in% drop.cols)]

# Set decimal places
for (i in 1:nrow(q.overall)) {
  q.overall$q[i] <- specify_decimal(as.numeric(q.overall$q[i]), 2)
  q.overall$q.lcl[i] <- specify_decimal(as.numeric(q.overall$q.lcl[i]), 2)
  q.overall$q.ucl[i] <- specify_decimal(as.numeric(q.overall$q.ucl[i]), 2)
  q.overall$area_ha[i] <- specify_decimal(as.numeric(q.overall$area_ha[i]), 1)
  q.overall$qh[i] <- specify_decimal(as.numeric(q.overall$qh[i]), 1)
  q.overall$qh.lcl[i] <- specify_decimal(as.numeric(q.overall$qh.lcl[i]), 1)
  q.overall$qh.ucl[i] <- specify_decimal(as.numeric(q.overall$qh.ucl[i]), 1)
}

# Write the summary file
write.csv(q.overall, file = file.q.overall.out, row.names = FALSE, na = "")

cat("done\n")

##########################################
# Benchmark vegetation QH
##########################################

cat("Preparing file ", basename(file.qh.bm.out), "...", sep = "")
qh.bm <- read.csv(file.qh.bm, header = T)

# Drop unwanted columns
drop.cols <- c("notes")
qh.bm <- qh.bm[, !(names(qh.bm) %in% drop.cols)]

# Set decimal places
for (i in 1:nrow(qh.bm)) {
  qh.bm$area_ha[i] <- specify_decimal(as.numeric(qh.bm$area_ha[i]), 2)
  qh.bm$qh[i] <- specify_decimal(as.numeric(qh.bm$qh[i]), 2)
  qh.bm$qh.lcl[i] <- specify_decimal(as.numeric(qh.bm$qh.lcl[i]), 3)
  qh.bm$qh.ucl[i] <- specify_decimal(as.numeric(qh.bm$qh.ucl[i]), 3)
}

# Write the summary file
write.csv(qh.bm, file = file.qh.bm.out, row.names = FALSE, na = "")

cat("done\n")

##########################################
# Functional group quality results file
##########################################

cat("Preparing file ", basename(file.q.fg), "...", sep = "")
q.fg <- read.csv(file.q.fg, header = T)

# Drop unwanted columns
drop.cols <- c("bm.vegetation")
q.fg <- q.fg[, !(names(q.fg) %in% drop.cols)]
names(q.fg)[names(q.fg) == "q.fg"] <- "q"

# Set decimal places
for (i in 1:nrow(q.fg)) {
  q.fg$q[i] <- specify_decimal(as.numeric(q.fg$q[i]), 2)
  q.fg$q.lcl[i] <- specify_decimal(as.numeric(q.fg$q.lcl[i]), 2)
  q.fg$q.ucl[i] <- specify_decimal(as.numeric(q.fg$q.ucl[i]), 2)
}

# Rename columns
names(q.fg)[names(q.fg) == "category"] <- "functional.group"

# Write the summary file
write.csv(q.fg, file = file.q.fg.out, row.names = FALSE, na = "")

cat("done\n")

##########################################
# Indicator quality results file
##########################################

cat("Preparing file ", basename(file.q.ei), "...", sep = "")

q.ei <- read.csv(file.q.ei, header = T)
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
for (i in 1:nrow(q.ei)) {
  q.ei$q.means.raw[i] <- specify_decimal(as.numeric(q.ei$q.means.raw[i]), 2)
  q.ei$q.means[i] <- specify_decimal(as.numeric(q.ei$q.means[i]), 2)
  q.ei$q.overlap[i] <- specify_decimal(as.numeric(q.ei$q.overlap[i]), 2)
  q.ei$overlap.weight[i] <- specify_decimal(as.numeric(q.ei$overlap.weight[i]), 3)
  q.ei$q[i] <- specify_decimal(as.numeric(q.ei$q[i]), 2)
  q.ei$q.lcl[i] <- specify_decimal(as.numeric(q.ei$q.lcl[i]), 2)
  q.ei$q.ucl[i] <- specify_decimal(as.numeric(q.ei$q.ucl[i]), 2)
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
file.ei.summary <- paste0(RESULTSDIR, FILENAME.EI.SUMMARY)
ei.details <- read.csv(file.ei.summary, header = T, stringsAsFactors = FALSE)
ei.details <- ei.details[, c("ei", "ei.name")]
q.ei <- merge(q.ei, ei.details, by.x = "indicator", by.y = "ei")
q.ei <- df.reorder(q.ei, col.move = "ei.name", col.before = "indicator")

# Write the summary file
write.csv(q.ei, file = file.q.ei.out, row.names = FALSE, na = "")

cat("done\n\n")

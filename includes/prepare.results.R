########################################################
# Prepare (focal) results file and populate with basic summary stats
#
# Adds stratum column if not already present in raw data file.
# This operation is same for all EIs. Called by EI-specific file. The following
# parameters are inherited from calling script:
# 		curr.EI
# 		EI.name
# 		distn
# 		RESULTSDIR (from params file)
# 		test.tail		Tail to be compared to benchmark; same for entire EI
# 		distn			Code for distribution
# 		q.method		Benchmark quality scoring method:
# 		bm.val			Benchmark value (fixed method only)
########################################################

cat("Summarizing focal values...")

###################################################
# Get EI and distribution (from calling script): curr.EI, distn
###################################################

curr.EI <- as.character(curr.EI)
# print(paste(curr.EI, ' - ', EI.name, " (distribution=", distn, ')', sep=''))

###################################################
# Load transformed raw data for this EI
###################################################

rawFileName <- paste(curr.EI, "_raw.csv", sep = "")
rawFile <- paste(RESULTSDIR, rawFileName, sep = "")
dat.EI.raw <- read.csv(rawFile, header = T, stringsAsFactors = FALSE)
dat.EI <- dat.EI.raw

###################################################
# Standardize stratum column.
# If current EI does not use strata, add dummy column "stratum" and
# populate with dummy value "nostrata"
###################################################

# Get column names
cols <- colnames(dat.EI)

# Name of the EI column in new data frame
# Making separate variable for now to allow rename if desired
EI.col.name <- curr.EI

# Reorder columns, keeping stratum column if present,
# and keeping the transformed EI column
if (any("stratum" %in% cols)) {
  # Keep stratum column
  has.stratum <- TRUE
} else {
  has.stratum <- FALSE
  dat.EI$stratum <- "nostrata"
}

dat.EI <- dat.EI[, c("landCover", "vegClass", "focalOrBenchmark", "plotCode", "stratum", "EI.tr")]
colnames(dat.EI) <- c("landCover", "vegClass", "focalOrBenchmark", "plotCode", "stratum", "EI")

# Re-order by first two columns
dat.EI <- dat.EI[with(dat.EI, order(dat.EI[, 1], dat.EI[, 2])), ]

# Get column names again
cols <- colnames(dat.EI)

#######################################
# Extract data frame of basic bm stats
#######################################

# Extract benchmark data only
EI.b.all <- dat.EI[dat.EI$focalOrBenchmark == "b", ]
EI.b.all <- EI.b.all[!is.na(EI.b.all$EI), ] # Remove NAs

# Get summary stats
EI.b <- with(EI.b.all, aggregate(EI, list(vegClass = vegClass, stratum = stratum),
  FUN = function(x) {
    c(
      n = length(x),
      mean = mean(x),
      sd = sd(x),
      med = median(x),
      max = max(x),
      min = min(x)
    )
  }
))
EI.b <- cbind(EI.b[-ncol(EI.b)], EI.b[[ncol(EI.b)]])
colnames(EI.b) <- c("vegClass", "stratum", "b.n", "b.mean", "b.sd", "b.med", "b.max", "b.min")

# Fix n
EI.b.plots <- unique(EI.b.all[, c("plotCode", "vegClass", "stratum")])
EI.b.n <- aggregate(
  plotCode ~ vegClass + stratum,
  data = EI.b.plots,
  FUN = length
)
colnames(EI.b.n) <- c("vegClass", "stratum", "b.n.new")

EI.b.n$vegClass.stratum <- paste0(EI.b.n$vegClass, "|", EI.b.n$stratum)
EI.b.n <- EI.b.n[, c("vegClass.stratum", "b.n.new")]
EI.b$vegClass.stratum <- paste0(EI.b$vegClass, "|", EI.b$stratum)

EI.b <- merge(EI.b, EI.b.n, by = "vegClass.stratum")
names(EI.b)[names(EI.b) == "b.n"] <- "b.n.bad" # Save temporarily in case compare before delete
names(EI.b)[names(EI.b) == "b.n.new"] <- "b.n"
EI.b <- EI.b[, !names(EI.b) %in% c("vegClass.stratum", "b.n.bad")]
EI.b <- df.reorder(EI.b, col.move = "b.n", col.before = "stratum")

#######################################
# Extract data frame of basic focal stats
#######################################

# Extract focal data only
EI.f.all <- dat.EI[dat.EI$focalOrBenchmark == "f", ]
EI.f.all <- EI.f.all[!is.na(EI.f.all$EI), ] # Remove NAs

# Get summary stats
EI.f <- with(EI.f.all, aggregate(EI, list(landCover = landCover, vegClass = vegClass, stratum = stratum),
  FUN = function(x) {
    c(
      n = length(x), # This may be incorrect for indicators with multiple strata
      mean = mean(x),
      sd = sd(x),
      med = median(x),
      max = max(x),
      min = min(x)
    )
  }
))
EI.f <- cbind(EI.f[-ncol(EI.f)], EI.f[[ncol(EI.f)]])
colnames(EI.f) <- c("landCover", "vegClass", "stratum", "f.n", "f.mean", "f.sd", "f.med", "f.max", "f.min")

# Fix n
EI.f.plots <- unique(EI.f.all[, c("plotCode", "landCover", "stratum")])
EI.f.n <- aggregate(
  plotCode ~ landCover + stratum,
  data = EI.f.plots,
  FUN = length
)
colnames(EI.f.n) <- c("landCover", "stratum", "f.n.new")

EI.f.n$landCover.stratum <- paste0(EI.f.n$landCover, "|", EI.f.n$stratum)
EI.f.n <- EI.f.n[, c("landCover.stratum", "f.n.new")]
EI.f$landCover.stratum <- paste0(EI.f$landCover, "|", EI.f$stratum)

EI.f <- merge(EI.f, EI.f.n, by = "landCover.stratum")
names(EI.f)[names(EI.f) == "f.n"] <- "f.n.bad" # Save temporarily in case compare before delete
names(EI.f)[names(EI.f) == "f.n.new"] <- "f.n"
EI.f <- EI.f[, !names(EI.f) %in% c("landCover.stratum", "f.n.bad")]
EI.f <- df.reorder(EI.f, col.move = "f.n", col.before = "stratum")

# Make initial data frame with EI name, vegetation and strata, and focal stats
EI.f$EI <- curr.EI

# Add columns for basic analysis parameters
EI.f$test.tail <- test.tail # Tail to be compared to benchmark; same for entire EI
EI.f$dist <- distn # Code for distribution
EI.f$q.method <- q.method # Benchmark quality scoring method:
# 	("fixed","empirical")
EI.f$bm.val <- bm.val # Benchmark fixed value. NA if q.method="empirical"

# Rearrange the columns
EI.f <- EI.f[, c("EI", "landCover", "vegClass", "stratum", "dist", "test.tail", "q.method", "bm.val", "f.n", "f.mean", "f.sd", "f.med", "f.max", "f.min")]

#######################################
# Merge the two data frames into single "focal
# results" data frame
#######################################

EI.f.temp <- EI.f
# Merge the data frames
# Left join ensures that all land cover classes are included
# EI.f <- merge(EI.f.temp, EI.b, by=c("vegClass", "stratum", all.x=T))
EI.f <- merge(EI.f.temp, EI.b, by.x = c("vegClass", "stratum"), by.y = c("vegClass", "stratum"), all.x = TRUE)

# Move columns EI & landCover to front
col_idx <- grep("landCover", names(EI.f))
EI.f <- EI.f[, c(col_idx, (1:ncol(EI.f))[-col_idx])]
col_idx <- grep("EI", names(EI.f))
EI.f <- EI.f[, c(col_idx, (1:ncol(EI.f))[-col_idx])]

if (curr.EI == "asc") {
  stratum.ord <- unique(dat.EI.raw[, c("stratum", "ord")])
  EI.f <- merge(EI.f, stratum.ord, by = "stratum", all.x = TRUE)
  EI.f <- df.reorder(df = EI.f, col.move = "stratum", col.before = "vegClass")
  EI.f <- df.reorder(df = EI.f, col.move = "ord", col.before = "stratum")
  EI.f <- EI.f[order(EI.f$landCover, EI.f$vegClass, EI.f$ord, EI.f$stratum), ]
}

# print(head(EI.f))

#######################################
# Add remaining columns
#######################################

EI.f$diff <- EI.f$f.mean - EI.f$b.mean # Difference, f.mean - b.mean
EI.f$p.diff <- NA # P-value of difference, from permutation test
EI.f$q.diff <- NA # Distance-based quality
EI.f$ol <- NA # Overlap of fitted f and b distributions
EI.f$ol.lcl <- NA # Lower 95% CL of overlap
EI.f$ol.ucl <- NA # Upper 95% CL of overlap
EI.f$ol.w <- NA # Weight of ol relative to diff in calculating overall quality
EI.f$q <- NA # Overall quality (based on combination of d and o)
EI.f$q.lcl <- NA # Lower 95% CL of quality
EI.f$q.ucl <- NA # Upper 95% CL of quality

# Save file so far
f.results.file <- paste(RESULTSDIR, resultsFocal, sep = "")
write.csv(EI.f, file = f.results.file, row.names = FALSE)

cat("done\n")

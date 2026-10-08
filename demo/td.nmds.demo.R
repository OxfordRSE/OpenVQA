#######################################################
# Tweaks Teck VQA script "td.R" to demonstrate NMDS and
# VQA indicator taxonomic distance (TD)
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
# 21 Oct. 2016
#
# Purpose:
# - Modify input file, removing focal data and using benchmark data only
# - Contrasts pairs of different vegetation types instead of focal vs. benchmark
#
# Requires tab-delimited file of raw EI values:
# 		plotCode: 							unique alphanumeric code for plot
# 		focalOrBenchmark: 		'b' for benchmark, 'f' for focal site
# 		vegClass: 							short code for vegetation class or category
# 		species: 							species name, excluding morphospecies
# 		cover: 								Maximum percent cover of the species across all
# 													stratum
#######################################################

#######################################################
# Parameters and functions
#######################################################

# Load packages specific to this EI
library(vegan) # NMDS
library(picante) # Used to convert input file to vegan format

# Set working directory
# Must set first to set directory parameters
wd <- getwd()
setwd(wd)
wd

# load parameters
source("params.R", local = TRUE)

# load global functions
source(paste(INCLUDESDIR, "functions.R", sep = ""), local = TRUE)

# load distribution-fitting and quality coefficient functions
source(paste(INCLUDESDIR, "functions.R", sep = ""), local = TRUE)

# Current EI (ecological indicator)
# Standard abbreviation for this EI; will be used for file names
curr.EI <- "TD"

# Plain English name of this EI
EI.name <- "NMDS Taxonomic Distance"

##############################
# Benchmark distribution model:
# 		Bin - Binomial
# 		NBin - Negative binomial
# 		ZIBin - Zero-inflated negative binomial
# 		P - Poisson
# 		ZIP - Zero-inflated Poisson
# 		Bet - Beta
# 		ZIBet - Zero-inflated beta
# 		OIBet - One-inflated beta
# 		ZOIBet - Zero-and-one-inflated beta
# 		Norm - Normal
# 		tNorm - Zero-truncated normal
##############################

distn <- "gamma"

##############################
# Distribution tail to compare for Quality
# Score. Values:
# 		'upper'
# 		'lower'
# 		'both'
##############################

test.tail <- "upper"

##############################
# Graphing parameters
##############################

figs.dir <- "figs/"
figdir.abs <- paste(wd, figs.dir, sep = "/")

# Graphing file. Must be in same directory as this file
graph.file <- "plot.nmds.demo.R"

##############################
# Names and locations of input and output files
##############################
inputFileName <- "speciesCover.txt"
rawFileName <- paste("nmds.demo_raw.csv", sep = "") # Complete nmds results
resultsFileName <- paste("nmds.demo.results.csv", sep = "")

##############################
# Transformation options
##############################

# TRUE if data are proportions or percent, otherwise false
proportions <- TRUE

# Set TRUE if data are percent
convert.percent <- FALSE

#  Set TRUE to scale abundance values between 0 and 1
# Useful if cover method sums to >100% (or >1 proportional abundance)
scale <- TRUE

# Set to TRUE to set any proportions > 1 to 1
# Scale=TRUE does this automatically
truncate.at.one <- FALSE

# Set TRUE to logit transform (must also convert to proportions)
# Nb. Really seems to be a bad idea to do this for nmds
logit <- FALSE

#######################################################
# Load raw data
#######################################################

# Import table containing cover values of all species each plot,
# plus vegetation class by stratum (growth form)
inputFile <- paste(INPUTDIR, inputFileName, sep = "")
dat <- read.table(inputFile, header = T, sep = "\t")
# detachAllData()
# attach(dat)
dat <- dat

#######################################################
# Validations and transformations
#######################################################

# Import table containing cover values of all species each plot,
# plus vegetation class.
inputFile <- paste(INPUTDIR, inputFileName, sep = "")
dat <- read.table(inputFile, header = T, na.strings = "NULL", sep = "\t")
dat$EI.tr <- NA

dat <- dat[, c("plotCode", "focalOrBenchmark", "vegClass", "species", "cover")]
colnames(dat) <- c("plotCode", "focalOrBenchmark", "vegClass", "species", "EI")
dat$EI.tr <- NA

# Load the transformations file, which acts on df dat
source(paste(INCLUDESDIR, "transformations.R", sep = ""), local = TRUE)

# attach(dat)

#######################################################
# Fiddle input matrix
#######################################################

# Choose one
bm.vegClass <- "v[Wet forest]+s[old]"
f1.veg <- "v[Dry forest]+s[old]"
f2.veg <- "v[Alpine dwarf shrub]"
f3.veg <- "v[Brushland/Grassland]"

dat <- dat[focalOrBenchmark == "b", ]
dat$actualVegClass <- dat$vegClass
dat$vegClass <- NA

# Set Old-growth wet forest as "target" (=benchmark) and select four other
# vegetation types to compare
dat$vegClass[dat$actualVegClass == bm.vegClass] <- bm.vegClass
dat$vegClass[dat$actualVegClass == f1.veg] <- bm.vegClass
dat$vegClass[dat$actualVegClass == f2.veg] <- bm.vegClass
dat$vegClass[dat$actualVegClass == f3.veg] <- bm.vegClass

dat <- dat[!is.na(dat$vegClass), ] # Remove remaining classes

# Set "benchmark and multiple "focal" groups
dat$focalOrBenchmark <- as.character(dat$focalOrBenchmark)
dat$focalOrBenchmark[dat$actualVegClass == bm.vegClass] <- "b"
dat$focalOrBenchmark[dat$actualVegClass == f1.veg] <- "f1"
dat$focalOrBenchmark[dat$actualVegClass == f2.veg] <- "f2"
dat$focalOrBenchmark[dat$actualVegClass == f3.veg] <- "f3"
dat$focalOrBenchmark <- as.factor(dat$focalOrBenchmark)

df.actualPlotVegClasses <- dat[, c("plotCode", "actualVegClass")]
df.actualVegClasses <- unique(dat[, c("focalOrBenchmark", "actualVegClass")])

#######################################################
# Calculate taxonomic distances
#######################################################

# Set up loop to extract all plots from a given vegetation type
vegClasses <- unique(dat$vegClass)

i <- 1 # For testing without loop; over-ridden if following line
# and closing brace are uncommented
for (i in 1:length(vegClasses)) {
  # Get each unique vegClass
  currVegClass <- vegClasses[i]
  print(currVegClass)

  # Extract matrix of plots for this vegClass, fully identified species only
  spCov <- subset(dat, !is.nan(species) & !(species == "<NA>") & vegClass == currVegClass, select = c(plotCode, EI.tr, species))

  # Make another matrix to hold grouping of plot (b, f1, f2, etc.)
  plots <- subset(dat, vegClass == currVegClass, select = c(plotCode, focalOrBenchmark, vegClass))
  plots <- plots[!duplicated(plots[, c("plotCode", "focalOrBenchmark", "vegClass")]), ]

  #############################
  # NMDS
  #############################

  # write the file with headers
  write.table(spCov, file = "spCov.txt", quote = FALSE, sep = "\t", col.names = FALSE, row.names = FALSE)

  # Read it back in using readsample function readsample to coerce into
  # format used by vegan
  # species x site matrix
  spCov <- readsample("spCov.txt")
  unlink("spCov.txt")

  # look at the untransformed data
  spCov[1:5, 1:5]

  if (scale == TRUE) {
    # check total abundance in each sample
    apply(spCov, 1, sum)

    # Turn percent cover to relative abundance by dividing each value by sample
    # total abundance
    spCov <- decostand(spCov, method = "total")

    # check total abundance in each sample
    apply(spCov, 1, sum)

    # look at the transformed data
    spCov[1:5, 1:5]
  }

  # The NMDS
  # spCov.mds <- metaMDS(spCov, distance="euclidean", trymax=50, k=2, autotransform=FALSE, trace=TRUE)
  spCov.mds <- metaMDS(spCov, distance = "bray", trymax = 50, k = 2, autotransform = FALSE, trace = TRUE)

  # Get species scores
  sampleScores <- data.frame(spCov.mds$points)

  # Convert the labels (plot names) to a  column
  for (j in 1:nrow(sampleScores)) {
    rn <- rownames(sampleScores)
    sampleScores[j, 3] <- rn[j]
  }
  colnames(sampleScores) <- c("x", "y", "plotCode")

  # merge the two matrices
  TdPlotTemp <- merge(plots, sampleScores, by = "plotCode")

  ###################################
  # Calculate centroid for benchmark plots only
  ###################################
  bmTd <- subset(TdPlotTemp, TdPlotTemp$focalOrBenchmark == "b", select = c("x", "y"))
  bm.centroid.x <- mean(bmTd$x)
  bm.centroid.y <- mean(bmTd$y)
  # centroid <-c(mean(bmTd$x),mean(bmTd$y))
  centroid <- c(bm.centroid.x, bm.centroid.y)

  print("###########################")
  print(paste("Bm centroid x: ", bm.centroid.x, sep = ""))
  print(paste("Bm centroid y: ", bm.centroid.y, sep = ""))
  print(paste("Bm centroid: ", centroid, sep = ""))
  print("###########################")

  ###################################
  # Calculate TD: distance from each plot to centroid
  ###################################
  TdPlotTemp$td <- sqrt((TdPlotTemp$x - centroid[1])^2 + (TdPlotTemp$y - centroid[2])^2)

  if (i == 1) {
    # Begin the final matrix
    TdPlot <- TdPlotTemp
  } else {
    # Append the next set of rows
    TdPlot <- rbind(TdPlot, TdPlotTemp)
  }
}

# Remove duplicates
TdPlot <- TdPlot[!duplicated(TdPlot[, c("plotCode", "focalOrBenchmark", "vegClass")]), ]

td.plot <- merge(TdPlot, df.actualPlotVegClasses, by = c("plotCode"))

# Add column with human-readable veg classes
td.plot$vegClass2 <- gsub("v\\[", "", td.plot$vegClass)
td.plot$vegClass2 <- gsub("]\\+s\\[", ", ", td.plot$vegClass2)
td.plot$vegClass2 <- gsub("]", "", td.plot$vegClass2)

# Add column with human-readable veg classes
td.plot$actualVegClass2 <- gsub("v\\[", "", td.plot$actualVegClass)
td.plot$actualVegClass2 <- gsub("]\\+s\\[", ", ", td.plot$actualVegClass2)
td.plot$actualVegClass2 <- gsub("]", "", td.plot$actualVegClass2)

# colnames(TdPlot) <- c('plotCode', 'focalOrBenchmark', 'vegClass', 'x', 'y', 'EI', 'landCover')
names(td.plot)[names(td.plot) == "td"] <- "EI"
td.plot$EI.tr <- td.plot$EI
td.plot <- td.plot[, c("vegClass", "actualVegClass", "vegClass2", "actualVegClass2", "focalOrBenchmark", "plotCode", "x", "y", "EI", "EI.tr")]

# Remove nasty outliers
# Need better way to do this, but will do for now
td.plot <- td.plot[td.plot$EI < 500, ]

td.plot.saved <- td.plot[, c("vegClass2", "actualVegClass2", "focalOrBenchmark", "plotCode", "x", "y", "EI", "EI.tr")]
names(td.plot.saved)[names(td.plot.saved) == "vegClass2"] <- "vegClass"
names(td.plot.saved)[names(td.plot.saved) == "actualVegClass2"] <- "actualVegClass"

# Write file of taxonomic distances as raw data file
rawFile <- paste(RESULTSDIR, rawFileName, sep = "")
write.csv(td.plot.saved, file = rawFile, row.names = FALSE)

#######################################################
# Calculate mean distances in each group
#######################################################

# Make data frame of moments for transformed exotic species cover
# aggregated by vegclass + stratum
td.bm <- subset(td.plot, td.plot$focalOrBenchmark == "b")
# td.bm <- aggregate(td.bm $EI.tr, by=list(td.bm $actualVegClass), FUN = c("count","mean", "sd", "median", "max","min"))
td.bm <- with(td.plot, aggregate(EI.tr, list(actualVegClass = actualVegClass),
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
td.bm <- cbind(td.bm[-ncol(td.bm)], td.bm[[ncol(td.bm)]])
colnames(td.bm) <- c("actualVegClass", "n", "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min")

td.f1 <- subset(td.plot, td.plot$focalOrBenchmark == "f1")
# td.f1 <- aggregate(td.f1 $EI.tr, by=list(td.f1 $actualVegClass), FUN = c("count","mean", "sd", "median", "max","min"))
td.f1 <- with(td.f1, aggregate(EI.tr, list(actualVegClass = actualVegClass),
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
td.f1 <- cbind(td.f1[-ncol(td.f1)], td.f1[[ncol(td.f1)]])
colnames(td.f1) <- c("actualVegClass", "n", "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min")

td.f2 <- subset(td.plot, td.plot$focalOrBenchmark == "f2")
# td.f2 <- aggregate(td.f2 $EI.tr, by=list(td.f2 $actualVegClass), FUN = c("count","mean", "sd", "median", "max","min"))
td.f2 <- with(td.f2, aggregate(EI.tr, list(actualVegClass = actualVegClass),
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
td.f2 <- cbind(td.f2[-ncol(td.f2)], td.f2[[ncol(td.f2)]])
colnames(td.f2) <- c("actualVegClass", "n", "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min")

td.f3 <- subset(td.plot, td.plot$focalOrBenchmark == "f3")
# td.f3 <- aggregate(td.f3 $EI.tr, by=list(td.f3 $actualVegClass), FUN = c("count","mean", "sd", "median", "max","min"))
td.f3 <- with(td.f3, aggregate(EI.tr, list(actualVegClass = actualVegClass),
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
td.f3 <- cbind(td.f3[-ncol(td.f3)], td.f3[[ncol(td.f3)]])
colnames(td.f3) <- c("actualVegClass", "n", "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min")

td.temp <- rbind(td.bm, td.f1, td.f2, td.f3)
td <- merge(td.temp, df.actualVegClasses, by = "actualVegClass")
td$targetVegClass <- bm.vegClass
td <- td[, c("actualVegClass", "targetVegClass", "focalOrBenchmark", "n", "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min")]

# Add column with human-readable veg classes
td$targetVegClass2 <- gsub("v\\[", "", td$targetVegClass)
td$targetVegClass2 <- gsub("]\\+s\\[", ", ", td$targetVegClass2)
td$targetVegClass2 <- gsub("]", "", td$targetVegClass2)

# Add column with human-readable veg classes
td$actualVegClass2 <- gsub("v\\[", "", td$actualVegClass)
td$actualVegClass2 <- gsub("]\\+s\\[", ", ", td$actualVegClass2)
td$actualVegClass2 <- gsub("]", "", td$actualVegClass2)

td <- td[, c("actualVegClass2", "targetVegClass2", "focalOrBenchmark", "n", "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min")]

names(td)[names(td) == "targetVegClass2"] <- "vegClass"
names(td)[names(td) == "actualVegClass2"] <- "actualVegClass"

# export results summarized by vegetation
resultsFile <- paste(RESULTSDIR, resultsFileName, sep = "")
write.csv(td, file = resultsFile, row.names = FALSE)

####################################################
# plot the results
####################################################

source(graph.file)

#######################################################
# End of script
#######################################################

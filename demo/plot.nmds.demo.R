##########################################################
# Plots first two axes of NMDS separately for each vegetation + stratum combination
#
# Warning:
# 	This file MUST be called by nmds.demo.R. Will not work if called on its own.
#
# Requires:
# 	TD_raw.csv (produced as intermediate step by td.nmds.2samp.R)
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
# Created: Oct. 20, 2016
#
##########################################################

graphics.off()

############################################
# Parameters
############################################

b.txt <- "Benchmark"
f1.txt <- "Focal1"
f2.txt <- "Focal2"
f3.txt <- "Focal3"

######################
# Set path to save figures
######################

# Base directory name for ungroup figure of combine focal and bm dists
figs.fitted.dir.name <- "nmds"

# Create directory if missing
dir.create(file.path(figdir.abs, figs.fitted.dir.name), showWarnings = F)
figs.fitted.dir <- paste(figs.dir, figs.fitted.dir.name, sep = "")


##########################################################
# Load transformed raw data for this EI & start results df
##########################################################

curr.EI <- as.character(curr.EI) # Just in case this is factor in calling script

rawFile <- paste(RESULTSDIR, rawFileName, sep = "")
dat.EI <- read.csv(rawFile, header = T)
# detachAllData()
# attach(dat.EI)

# get vegetation classes
allveg <- unique(dat.EI$vegClass)
n.veg <- length(allveg)

##########################################################
# Loop through vegetation, make separate plot for each vegetation class,
# plotting all lancover classes against benchmark in same figure
##########################################################

j <- 1 # Alternative for testing without loop
# Comment out next line and closing brace to use
# for (j in 1:n.veg) {		# START veg loop
curr.veg <- allveg[j]

# Make unix-friendly version of vegetation for filename
veg.fname <- gsub(" ", "_", curr.veg)
veg.fname <- gsub("/", "_", veg.fname)
veg.fname <- gsub(",", "", veg.fname)

print("")
print("====================================")
print(paste("Current vegetation: ", veg.fname, sep = ""))

EI.veg <- subset(dat.EI, dat.EI$vegClass == curr.veg)

# Set general graph attributes plus attributes
# specific to current EI and vegetation
text.main <- "NMDS of Species Composition\n of Four Vegetation Types"
text.x <- "NMDS Axis 1"
text.y <- "NMDS Axis 2"

######################################
# Plot four vegetation types
######################################

# Manually trim outliers
EI.veg <- EI.veg[EI.veg$x > -2, ]

# Set x values for plot, need lots
x.max <- max(EI.veg$x)
y.max <- max(EI.veg$y)
x.min <- min(EI.veg$x)
y.min <- min(EI.veg$y)

# set axis limits
extra.bit <- 0.01
xlim.max <- x.max + ((x.max - x.min) * extra.bit)
ylim.max <- y.max + ((y.max - y.min) * extra.bit * 100)
xlim.min <- x.min - ((x.max - x.min) * extra.bit)
ylim.min <- y.min - ((y.max - y.min) * extra.bit)

# Make the plot
p <- qplot(EI.veg$x, EI.veg$y,
  data = EI.veg,
  color = EI.veg$actualVegClass,
  main = text.main
)
p <- p + theme_classic()
p <- p + theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
p <- p + scale_color_discrete(name = "Vegetation")
p <- p + theme(legend.position = c(0.18, 0.85))
p <- p + theme(legend.background = element_rect(colour = "black"))
p <- p + labs(
  x = text.x,
  y = text.y
)

######################################
# Print and save the figure 1
######################################

print(p)
graphFileName <- paste("nmds.demo", veg.fname, sep = "_")
graphFile <- paste(figs.fitted.dir, "/", graphFileName, ".png", sep = "")
print(paste("Printing graph: ", graphFileName, sep = ""))
dev.copy(png, graphFile)
dev.off()

######################################
# Plot two vegetation types as focal and benchmark
######################################
# select classes to use
f1 <- "f1"
f2 <- "f2"

# Add column with human-readable veg classes
bm.vegClass2 <- gsub("v\\[", "", bm.vegClass)
bm.vegClass2 <- gsub("]\\+s\\[", ", ", bm.vegClass2)
bm.vegClass2 <- gsub("]", "", bm.vegClass2)

text.main <- paste("NMDS, Focal vs. Benchmark\n", bm.vegClass2, sep = "")
text.main <- paste("NMDS, Focal vs. Benchmark\n", sep = "")

# Manually trim outliers
EI.veg2 <- EI.veg[EI.veg$focalOrBenchmark == f1 | EI.veg$focalOrBenchmark == f2, ]
EI.veg2$actualVegClass <- as.character(EI.veg2$actualVegClass)
EI.veg2$actualVegClass[EI.veg2$focalOrBenchmark == f1] <- "Benchmark"
EI.veg2$actualVegClass[EI.veg2$focalOrBenchmark == f2] <- "Focal"
EI.veg2$actualVegClass <- as.factor(EI.veg2$actualVegClass)

# Set x values for plot, need lots
x.max <- max(EI.veg2$x)
y.max <- max(EI.veg2$y)
x.min <- min(EI.veg2$x)
y.min <- min(EI.veg2$y)

# set axis limits
extra.bit <- 0.01
xlim.max <- x.max + ((x.max - x.min) * extra.bit)
ylim.max <- y.max + ((y.max - y.min) * extra.bit * 100)
xlim.min <- x.min - ((x.max - x.min) * extra.bit)
ylim.min <- y.min - ((y.max - y.min) * extra.bit)

# Make the plot
p2 <- qplot(EI.veg2$x, EI.veg2$y,
  data = EI.veg2,
  color = EI.veg2$actualVegClass,
  main = text.main
)
p2 <- p2 + theme_classic()
p2 <- p2 + theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
p2 <- p2 + theme(legend.title = element_blank())
p2 <- p2 + theme(legend.position = c(0.83, 0.9))
p2 <- p2 + theme(legend.background = element_rect(colour = "black"))
p2 <- p2 + labs(
  x = text.x,
  y = text.y
)
p2 <- p2 + theme(plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"))
p2 <- p2 + guides(colour = guide_legend(reverse = T))
p2

######################################
# Print and save the figure 2
######################################

print(p2)
graphFileName <- paste("nmds.demo", veg.fname, "simulated.f.b", sep = "_")
graphFile <- paste(figs.fitted.dir, "/", graphFileName, ".png", sep = "")
print(paste("Printing graph: ", graphFileName, sep = ""))
dev.copy(png, graphFile)
dev.off()

# }	# end veg loop

##########################################################
# Plots first two axes of NMDS separately for each vegetation + stratum combination
#
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
f.txt <- "Focal"

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

rawFileName <- paste(curr.EI, "_raw.csv", sep = "")
rawFile <- paste(RESULTSDIR, rawFileName, sep = "")
dat.EI <- read.csv(rawFile, header = T)
detachAllData()
attach(dat.EI)

# Warning: the following line is a HACK
# Remove once have generalize parent script to preserve separate land cover classes
dat.EI$vegClass <- dat.EI$landCover

# get vegetation classes
allveg <- unique(dat.EI$vegClass)
n.veg <- length(allveg)

##########################################################
# Loop through vegetation, make separate plot for each vegetation class,
# plotting all lancover classes against benchmark in same figure
##########################################################

j <- 1 # Alternative for testing without loop
# Comment out next line and closing brace to use
for (j in 1:n.veg) { # START veg loop
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
  text.main <- curr.veg
  text.x <- "NMDS Axis 1"
  text.y <- "NMDS Axis 2"

  ######################################
  # Plot figure
  ######################################

  par(bg = "white")

  # Set transparency for overlaps
  # Must be from 0 to 1
  alpha.val <- 0.75

  # Set x values for plot, need lots
  x.max <- max(EI.veg$x)
  y.max <- max(EI.veg$y)
  x.min <- min(EI.veg$x)
  y.min <- min(EI.veg$y)

  # set axis limits
  extra.bit <- 0.01
  xlim.max <- x.max + ((x.max - x.min) * extra.bit)
  y.lim.max <- y.max + ((y.max - y.min) * extra.bit * 100)
  xlim.min <- x.min - ((x.max - x.min) * extra.bit)
  y.lim.max <- y.min - ((y.max - y.min) * extra.bit)

  # set colours
  b.col <- c(rgb(0, 0, 1, alpha.val))
  f.col <- c(rgb(1, 0, 0, alpha.val))

  # Make the plot
  plot(EI.veg$x, EI.veg$y,
    col = ifelse(
      EI.veg$focalOrBenchmark == "b",
      b.col,
      f.col
    ),
    main = text.main,
    xlab = text.x, ylab = text.y,
    ylim = c(ylim.min, ylim.max),
    xlim = c(xlim.min, xlim.max),
    pch = 19
  )

  legend(xlim.max - extra.bit * 100, ylim.max - extra.bit,
    pch = c(16, 16),
    col = c(b.col, f.col),
    c(b.txt, f.txt),
    cex = 0.8
  )
  # legend( xlim.max-extra.bit*100, ylim.max-extra.bit,
  # col=c(b.col, f.col),
  # c(b.txt, f.txt)
  # )

  # legend(xlim.max-extra.bit, ylim.max-extra.bit,
  # pch=c(2,2),
  # col=c(rgb(0,0,1,alpha.val), rgb(1,0,0, alpha.val)), c(b.txt, f.txt),
  # bty=”o”,
  # box.col=”darkgreen”,
  # bty='o',
  # cex=.8
  # )

  ######################################
  # Save the figure
  ######################################

  graphFileName <- paste("nmds", veg.fname, sep = "_")
  graphFile <- paste(figs.fitted.dir, "/", graphFileName, ".png", sep = "")
  print(paste("Printing graph: ", graphFileName, sep = ""))
  dev.copy(png, graphFile)
  dev.off()
} # end veg loop

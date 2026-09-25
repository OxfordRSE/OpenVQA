#####################################
#####################################
# Libraries (R packages)
#
# Will throw error if not already installed
#####################################
#####################################

cat("Loading libraries...")

# Vector of packages to load.
# One package name per line, in
# double quotes.
pkgs <- c(
  "tidyverse", # General tools; fixes much R awfulness
  "Hmisc", # Import MS Access data; requires mdb-tools
  "betareg", # beta distribution
  "lmtest", # Likelihood-ratio test (lrtest)
  "car", # Analysis of deviance
  "pscl", # Zero-inflated Poisson and neg. binomial
  # "zoib",	     	  # Zero-inflated beta distribution
  "MASS", # negative binomial distribution (fitdistr)
  "vcd", # goodness of fit tests
  "fitdistrplus", # General distribution fitting (fitdist)
  "boot", # bootstrapping
  "gamlss", # for inflated beta models
  "gamlss.dist", # contains inflated beta distributions
  "pwr", # Power analysis
  "simpleboot", # Func one.boot, used in uni.fit.beta()
  "VGAM", # For betanorm functions
  "vegan", # NMDS, used by td.R
  "picante", # Convert file to vegan format (td.R only)
  "readxl", # Import directly from Excel files
  "openxlsx2", # Read/write Excel files (need to update to this pkg only)
  "RCurl", # API request (TNRS)
  "jsonlite", # JSON coding/decoding (TNRS)
  "data.table",
  "drc",
  "SemNetCleaner", # Language manipulation (e.g., pluralize function)
  "tools", # Fnc file_ext(), etc.
  "optparse" # Command line arguments
)

# Load silently
for (pkg in pkgs) {
  if (LIB.LOAD.SILENT == TRUE) {
    eval(parse(text = paste0("suppressMessages(library(", pkg, "))")))
  } else {
    eval(parse(text = paste0("library(", pkg, ")")))
  }
}

cat("done\n")


# Turn on option that warns you if pernicious Core R
# "feature" (aka bug) of partial matching object names occurs
# Can't turn it off but this is better than nothing
# Doing after loading libraries because many packages trigger warnings.
options(warnPartialMatchDollar = TRUE)
options(warnPartialMatchArgs = TRUE)
options(warnPartialMatchAttr = TRUE)

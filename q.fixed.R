###################################################
# Fixed Quality Assessment
#
# Alternative VQA pipeline when QH.METHOD="assume.0"
#   or QH.METHOD="assume.1". Generates only the
#   benchmark quality results files needed by
#   script qh.net.R to calculate Net Quality Hectares.
#
# Called by:
#   vqa.batch.R
# Input:
#   inputs/landCover.csv
# Output:
# 	SUMMARY.FOCAL.BM.FILE (summary_focal_qh_bm.csv)
#   SUMMARY.QH.BOOT.BM.FILE (summary_qh_boot_bm.csv)
###################################################

cat("\n")
cat("**************************************************\n")
cat("Running alternative VQA for QH.METHOD='", QH.METHOD, "'\n", sep = "")
cat("**************************************************\n")

# ##########################################
# # Report parameters & confirm operation
# ##########################################
#
# # Display confirmation message
# cat( paste0( "Calculate Q.fixed for project '", PROJ, "' using the following settings: \n") )
# cat( paste0( MSG.CONF.START, MSG.CONF.QH.NET ) )
#
# if (interactive()==FALSE) {
#   yes <- c("y", "Y", "Yes", "yes")
#   cat("Continue? (y/n):")
#   response <- readLines("stdin",n=1)
#   if ( ! response %in% yes ) stop_quietly("Operation cancelled\n\n")
# } else {
#   cat("\n\n")
# }
#
# cat("\n")
# cat("##########################################\n")
# cat("Begin operation\n")
# cat("\n")
# source("libraries.R")
#
#######################################
# Determine fixed Q value to use
#######################################

cat("Determining default quality...")
if (QH.METHOD == "assume.0") {
  q.fixed <- 0
} else if (QH.METHOD == "assume.1") {
  q.fixed <- 1
} else {
  msg <- paste0("ERROR: '", QH.METHOD, "': invalid value of QH.METHOD!")
  stop_quietly(msg)
}
cat("Q.fixed=", q.fixed, "...done\n", sep = "")

#######################################
# Import land cover data
#######################################

fileandpath <- paste0(INPUTDIR, LANDCOVER.FILE)
cat("Importing df.landCover from file 'inputs/", LANDCOVER.FILE, "'...", sep = "")
df.landCover <- read.csv(fileandpath, header = TRUE)
df.landCover$area_ha <- as.numeric(df.landCover$area_ha)
df.landCover$area_ha[is.na(df.landCover$area_ha)] <- 0
cat("done\n")

#######################################
# Create bm QH results df
#######################################

cat("Creating df.qh.bm:\n")
cat("- Aggregating land cover by bm vegetation...")
df.qh.bm <- aggregate(
  area_ha ~ vegClass,
  data = df.landCover,
  FUN = sum
)
cat("done\n")

cat("- Populating quality and quality hectares columns...")
df.qh.bm$qh <- df.qh.bm$area_ha * q.fixed
df.qh.bm$qh.lcl <- df.qh.bm$area_ha * q.fixed
df.qh.bm$qh.ucl <- df.qh.bm$area_ha * q.fixed
cat("done\n")

cat("- Restructing data frame...")
names(df.qh.bm)[names(df.qh.bm) == "vegClass"] <- "bm.veg"
df.qh.bm$notes <- ""
df.qh.bm <- df.qh.bm[, c(
  "bm.veg", "area_ha", "qh", "qh.lcl", "qh.ucl", "notes"
)]
cat("done\n")

#######################################
# Create bm quality bootstrap results df
#######################################

cat("Creating df.qh.bm.boot:\n")
cat("- Extracting applicable veg classes from df.qh.bm...")
df.qh.bm.boot <- df.qh.bm[!is.na(df.qh.bm$qh), c("bm.veg"), drop = FALSE]
cat("done\n")

cat("- Appending and populating n=", boot.reps, " bootrapped quality columns...", sep = "")
n.rows <- nrow(df.qh.bm.boot)
boot.cols <- data.frame(matrix(q.fixed, nrow = n.rows, ncol = boot.reps))
boot.cols <- convert.magic(boot.cols, "numeric")
df.qh.bm.boot <- cbind(df.qh.bm.boot, boot.cols)
cat("done\n")

#######################################
# Save results files
#######################################

cat("Saving final results files:\n")

filename <- SUMMARY.FOCAL.BM.FILE
fileandpath <- paste0(RESULTSDIR, filename)
cat("- df.qh.bm as file 'results/", filename, "'...", sep = "")
# suppressWarnings( write_excel_csv( df.qh.bm, file=fileandpath ) )
suppressMessages(write_excel_csv(df.qh.bm, file = fileandpath))
cat("done\n")

filename <- SUMMARY.QH.BOOT.BM.FILE
fileandpath <- paste0(RESULTSDIR, filename)
cat("- df.qh.bm.boot as file 'results/", filename, "'...", sep = "")
suppressMessages(write_excel_csv(df.qh.bm.boot, file = fileandpath))
cat("done\n")

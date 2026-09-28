cat("\n############################################\nBegin operation\n\n")

cat("Checking for required input files...")
if (QH.METHOD == "empirical") {
  required_files <- input_file_list(DF.LANDCOVER, DF.PLOTMETADATA, DF.SPECIES, EI.vec)
} else {
  required_files <- DF.LANDCOVER
}
for (fname in required_files) {
  if (!file.exists(paste0(INPUTDIR, fname, ".csv"))) {
    stop_quietly("ERROR: one or more required input files missing!\nHave you run import.R yet?\n")
  }
}
cat("done\n")

source("libraries.R")

if (QH.METHOD %in% c("assume.0", "assume.1")) {
  source("q.fixed.R")
} else if (VQA.SUMMARY.ONLY == FALSE) {
  cat("******************************************\nCalculate indicator quality\n\n")
  if (!exists("TD.ONLY")) TD.ONLY <- FALSE
  if (TD.ONLY == TRUE) {
    cat("*** Running indicator TD only ***\n\n")
    source("indicators/td.R", local = TRUE)
  } else {
    for (EI in EI.vec) source(paste0("indicators/", tolower(EI), ".R"), local = TRUE)
  }
}

if (PLOT.FIGS.ONLY == FALSE && TD.ONLY == FALSE && QH.METHOD == "empirical") {
  source("vqa.summary.R", local = TRUE)
  source("vqa.summary/vqa.summary.csv.R", local = TRUE)
  source("vqa.summary/vqa.summary.xl.R", local = TRUE)
}

cat("\nOperation completed\n############################################\n\n")

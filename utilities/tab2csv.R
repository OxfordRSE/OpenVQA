################################################
# Convert all files in current directory from tab-delimitted to csv
#
# Tabbed files identified by extension (see below)
# Makes copy of file with same basename plus .csv extension
################################################

# Set to TRUE to delete originals after conversion
delete.files <- TRUE

# Get list of all files in current directory
files.to.convert <- list.files(pattern = "txt")

# Read each file and write it to csv
lapply(files.to.convert, function(f) {
  df <- read.table(f, sep = "\t", header = TRUE)
  f.basename <- tools::file_path_sans_ext(f)
  f.csv <- paste0(f.basename, ".csv")
  write.csv(df, file = f.csv, row.names = FALSE)
})

if (delete.files == TRUE) {
  lapply(files.to.convert, function(f) {
    file.remove(f)
  })
}

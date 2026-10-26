# Load shared helpers beside this file into the caller's evaluation environment.
local({
  includes_dir <- NULL
  for (frame in sys.frames()) {
    path <- frame$ofile
    if (is.null(path)) path <- frame$file
    if (is.character(path) && length(path) == 1L && basename(path) == "functions.R") {
      includes_dir <- dirname(path)
    }
  }
  if (is.null(includes_dir)) stop("Cannot locate includes/functions.R.")
  for (file in c(
    "quality-functions.R", "distribution-functions.R",
    "overlap-functions.R", "bootstrap-functions.R",
    "utility-functions.R", "formatting-functions.R",
    "spreadsheet-functions.R", "plotting-functions.R"
  )) {
    sys.source(file.path(includes_dir, file), envir = parent.env(environment()))
  }
})


# Marker used by scripts to detect whether the shared functions are loaded.
functions.loaded <- function() {}

#!/usr/bin/env Rscript

source("rv/scripts/activate.R")

suppressPackageStartupMessages({
  library(lintr)
  library(styler)
})

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop("Expected a mode argument: 'style' or 'lint'.", call. = FALSE)
}

mode <- args[[1]]
files <- if (length(args) > 1) args[-1] else character()
files <- files[grepl("\\.(R|Rmd)$", files)]

if (!length(files)) {
  quit(status = 0)
}

if (identical(mode, "style")) {
  # Format the staged R files in place. pre-commit will notice the rewrite and
  # ask the developer to restage the files.
  styler::style_file(files)
  quit(status = 0)
}

if (!identical(mode, "lint")) {
  stop(sprintf("Unknown mode '%s'.", mode), call. = FALSE)
}

# Lint the selected files and fail the hook if any diagnostics are returned.
issues <- list()
for (file in files) {
  lint_result <- lintr::lint(file)
  if (length(lint_result)) {
    issues[[file]] <- lint_result
  }
}

if (length(issues)) {
  cat("Lint failures detected:\n")
  for (file in names(issues)) {
    cat(sprintf("\n%s\n", file))
    print(issues[[file]])
  }
  stop(sprintf("Pre-commit linting failed for %d file(s).", length(issues)), call. = FALSE)
}

quit(status = 0)

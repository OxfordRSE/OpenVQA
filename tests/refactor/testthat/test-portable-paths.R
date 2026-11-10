testthat::test_that("subprocess arguments preserve spaces and inherit scoped libraries", {
  root <- tempfile("project [with spaces] ")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  script <- file.path(root, "print arguments.R")
  writeLines(c(
    'cat(paste(commandArgs(TRUE), collapse = "|"), "\n", sep = "")',
    'cat(Sys.getenv("R_LIBS_USER"), "\n", sep = "")'
  ), script)
  previous <- Sys.getenv("R_LIBS_USER", unset = NA_character_)
  output <- run_refactor_command(script, root)
  testthat::expect_identical(output, c(
    paste("--data-root", root, sep = "|"),
    paste(.libPaths(), collapse = .Platform$path.sep)
  ))
  testthat::expect_identical(Sys.getenv("R_LIBS_USER", unset = NA_character_), previous)
})

testthat::test_that("relative filenames treat the root as a literal path", {
  root <- tempfile("project [with spaces] ")
  dir.create(file.path(root, "results"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))
  file <- file.path(root, "results", "table.csv")
  writeLines("value", file)
  testthat::expect_identical(relative_file_names(file, root), "results/table.csv")
  testthat::expect_identical(relative_file_names(character(), root), character())
})

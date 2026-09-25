refactor_project_root <- normalizePath(file.path("..", "..", ".."))
refactor_rscript <- file.path(R.home("bin"), "Rscript")
source(file.path(refactor_project_root, "pipeline", "io.R"), local = TRUE)

copy_refactor_project <- function(project) {
  source_root <- file.path(refactor_project_root, "data", paste0(project, "-min"))
  target_root <- tempfile(paste0(project, "-refactor-"), tmpdir = tempdir())
  testthat::expect_true(dir.create(target_root, recursive = TRUE))
  copied <- file.copy(
    list.files(source_root, full.names = TRUE),
    target_root,
    recursive = TRUE
  )
  testthat::expect_true(all(copied))
  target_root
}

run_refactor_command <- function(script, data_root, assessment = NULL,
                                 seed = NULL) {
  args <- c("--vanilla", script, "--data-root", data_root)
  if (!is.null(assessment)) {
    args <- c(args, "--assessment", assessment)
  }
  environment <- c(
    paste0(
      "R_LIBS_USER=",
      paste(.libPaths(), collapse = .Platform$path.sep)
    ),
    if (is.null(seed)) {
      character()
    } else {
      paste0(
        "VQA_TEST_SEED=", as.integer(seed)
      )
    }
  )
  output <- system2(
    refactor_rscript, args,
    stdout = TRUE, stderr = TRUE,
    env = environment
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  testthat::expect_equal(
    status, 0L,
    info = paste(c(output, collapse = "\n"))
  )
  invisible(output)
}

run_refactor_workflow <- function(project, assessment, seed = 20260810L) {
  data_root <- copy_refactor_project(project)
  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(refactor_project_root)

  run_refactor_command("validate.R", data_root)
  run_refactor_command(
    "analyse.R", data_root,
    assessment = assessment, seed = seed
  )
  data_root
}

cleanup_refactor_project <- function(data_root) {
  if (dir.exists(data_root)) {
    unlink(data_root, recursive = TRUE, force = TRUE)
  }
  invisible(TRUE)
}

read_refactor_tables <- function(directory) {
  files <- list.files(directory,
    pattern = "\\.csv$", full.names = TRUE,
    recursive = TRUE
  )
  tables <- lapply(files, read_csv_table)
  names(tables) <- sort(relative_file_names(files, directory))
  tables
}

relative_file_names <- function(files, root) {
  root <- normalizePath(root, mustWork = TRUE)
  files <- normalizePath(files, mustWork = TRUE)
  sub(paste0("^", root, "[/\\\\]?"), "", files)
}

compare_named_table_sets <- function(actual, expected, tolerance = 1e-12) {
  testthat::expect_identical(sort(names(actual)), sort(names(expected)))

  for (file_name in names(expected)) {
    testthat::expect_identical(
      names(actual[[file_name]]), names(expected[[file_name]]),
      info = paste("Columns differ:", file_name)
    )
    testthat::expect_equal(
      actual[[file_name]], expected[[file_name]],
      tolerance = tolerance, check.attributes = FALSE,
      info = paste("Table differs:", file_name)
    )
  }
  invisible(TRUE)
}

compare_refactor_table_sets <- function(actual_directory, expected_directory,
                                        tolerance = 1e-12,
                                        allowed_new = "coverByStratum.csv") {
  actual <- read_refactor_tables(actual_directory)
  expected <- read_refactor_tables(expected_directory)

  testthat::expect_equal(
    sort(setdiff(names(actual), allowed_new)), sort(names(expected)),
    info = "The new workflow produced an unexpected or missing CSV table."
  )
  compare_named_table_sets(actual[names(expected)], expected, tolerance)
  invisible(TRUE)
}

read_refactor_xlsx_tables <- function(file_path) {
  testthat::expect_true(
    requireNamespace("readxl", quietly = TRUE),
    info = "Package 'readxl' is required to compare XLSX results."
  )
  sheets <- readxl::excel_sheets(file_path)
  tables <- lapply(sheets, function(sheet) {
    table <- as.data.frame(readxl::read_excel(file_path, sheet = sheet),
      check.names = FALSE
    )
    if (identical(sheet, "Meta") && ncol(table) > 0L) {
      first_column <- as.character(table[[1L]])
      table <- table[
        is.na(first_column) | first_column != "Analysis date:", ,
        drop = FALSE
      ]
    }
    table
  })
  names(tables) <- sheets
  tables
}

compare_refactor_result_sets <- function(actual_directory, expected_directory,
                                         tolerance = 1e-12,
                                         allowed_new = character()) {
  actual_files <- list.files(actual_directory,
    recursive = TRUE,
    full.names = TRUE
  )
  expected_files <- list.files(expected_directory,
    recursive = TRUE,
    full.names = TRUE
  )
  actual_names <- relative_file_names(actual_files, actual_directory)
  expected_names <- relative_file_names(expected_files, expected_directory)

  testthat::expect_identical(
    sort(setdiff(actual_names, allowed_new)), sort(expected_names),
    info = "The new workflow produced unexpected or missing result files."
  )

  for (file_name in expected_names) {
    actual_path <- file.path(actual_directory, file_name)
    expected_path <- file.path(expected_directory, file_name)
    extension <- tolower(tools::file_ext(file_name))

    if (identical(extension, "csv")) {
      compare_named_table_sets(
        setNames(list(read_csv_table(actual_path)), file_name),
        setNames(list(read_csv_table(expected_path)), file_name),
        tolerance
      )
    } else if (identical(extension, "xlsx")) {
      compare_named_table_sets(
        read_refactor_xlsx_tables(actual_path),
        read_refactor_xlsx_tables(expected_path),
        tolerance
      )
    } else {
      testthat::fail(
        paste("No semantic comparator is defined for result file:", file_name)
      )
    }
  }
  invisible(TRUE)
}

read_refactor_png <- function(file_path) {
  testthat::expect_true(
    requireNamespace("png", quietly = TRUE),
    info = "Package 'png' is required to compare PNG figures."
  )
  png::readPNG(file_path, native = FALSE)
}

compare_refactor_figure_sets <- function(actual_directory, expected_directory,
                                         pixel_tolerance = 1 / 255,
                                         allowed_new = character()) {
  actual_files <- list.files(actual_directory,
    recursive = TRUE,
    full.names = TRUE
  )
  expected_files <- list.files(expected_directory,
    recursive = TRUE,
    full.names = TRUE
  )
  actual_names <- relative_file_names(actual_files, actual_directory)
  expected_names <- relative_file_names(expected_files, expected_directory)

  testthat::expect_identical(
    sort(setdiff(actual_names, allowed_new)), sort(expected_names),
    info = "The new workflow produced unexpected or missing figure files."
  )

  for (file_name in expected_names) {
    actual_path <- file.path(actual_directory, file_name)
    expected_path <- file.path(expected_directory, file_name)
    extension <- tolower(tools::file_ext(file_name))
    testthat::expect_identical(
      extension, "png",
      info = paste("Unsupported figure format:", file_name)
    )

    actual <- read_refactor_png(actual_path)
    expected <- read_refactor_png(expected_path)
    testthat::expect_identical(
      dim(actual), dim(expected),
      info = paste("Figure dimensions/channels differ:", file_name)
    )

    difference <- abs(actual - expected)
    max_difference <- max(difference)
    differing_pixels <- sum(apply(difference, c(1, 2), max) > pixel_tolerance)
    total_pixels <- dim(actual)[1L] * dim(actual)[2L]
    testthat::expect_true(
      max_difference <= pixel_tolerance,
      info = sprintf(
        paste0(
          "Figure differs: %s; %d/%d pixels differ (%.2f%%),",
          " maximum channel difference %.6f, tolerance %.6f"
        ),
        file_name, differing_pixels, total_pixels,
        100 * differing_pixels / total_pixels,
        max_difference, pixel_tolerance
      )
    )
  }
  invisible(TRUE)
}

minimal_projects <- list.files(
  file.path(refactor_project_root, "data"),
  pattern = "-min$", full.names = FALSE
)
minimal_projects <- minimal_projects[dir.exists(file.path(
  refactor_project_root, "data", minimal_projects
))]
contracts <- unlist(lapply(minimal_projects, function(minimal_project) {
  project <- sub("-min$", "", minimal_project)
  assessments <- list.dirs(
    file.path(refactor_project_root, "data", minimal_project),
    full.names = FALSE, recursive = FALSE
  )
  lapply(assessments, function(assessment) {
    list(project = project, assessment = assessment)
  })
}), recursive = FALSE)

selected_contract <- Sys.getenv("VQA_TEST_CONTRACT")
if (nzchar(selected_contract)) {
  contract_parts <- strsplit(selected_contract, "/", fixed = TRUE)[[1L]]
  if (length(contract_parts) != 2L || any(!nzchar(contract_parts))) {
    stop("VQA_TEST_CONTRACT must have the form 'project/assessment'.")
  }
  contracts <- Filter(function(contract) {
    identical(contract$project, contract_parts[[1L]]) &&
      identical(contract$assessment, contract_parts[[2L]])
  }, contracts)
  if (length(contracts) != 1L) {
    stop("VQA_TEST_CONTRACT does not identify exactly one assessment.")
  }
}

numeric_tolerance <- suppressWarnings(as.numeric(
  Sys.getenv("VQA_NUMERIC_TOLERANCE", "1e-12")
))
if (length(numeric_tolerance) != 1L ||
    !is.finite(numeric_tolerance) || numeric_tolerance < 0) {
  stop("VQA_NUMERIC_TOLERANCE must be one finite non-negative number.")
}

parse_flag <- function(name, default = "true") {
  value <- tolower(Sys.getenv(name, default))
  if (!value %in% c("true", "false")) {
    stop(name, " must be 'true' or 'false'.")
  }
  identical(value, "true")
}

compare_figures <- parse_flag("VQA_COMPARE_FIGURES")
compare_golden <- parse_flag("VQA_COMPARE_GOLDEN")
test_distinct_seed <- parse_flag("VQA_TEST_DISTINCT_SEED")

if (compare_figures && !compare_golden) {
  stop("VQA_COMPARE_FIGURES=true requires VQA_COMPARE_GOLDEN=true.")
}

for (contract in contracts) {
  contract_check <- if (compare_golden) {
    "preserves the complete output contract"
  } else {
    "is portable and same-seed repeatable"
  }
  description <- sprintf(
    "%s/%s %s", contract$project, contract$assessment, contract_check
  )
  testthat::test_that(
    description,
    {
      expected_root <- file.path(
        refactor_project_root,
        "tests/refactor/fixtures",
        contract$project,
        contract$assessment
      )
      seed <- 20260810L

      first_root <- run_refactor_workflow(
        contract$project, contract$assessment,
        seed = seed
      )
      on.exit(cleanup_refactor_project(first_root), add = TRUE)

      compare_refactor_result_sets(
        file.path(first_root, contract$assessment, "results"),
        file.path(expected_root, "results"),
        tolerance = numeric_tolerance,
        compare_values = compare_golden
      )
      if (compare_figures) {
        compare_refactor_figure_sets(
          file.path(first_root, contract$assessment, "figs"),
          file.path(expected_root, "figs")
        )
      }

      second_root <- run_refactor_workflow(
        contract$project, contract$assessment,
        seed = seed
      )
      on.exit(cleanup_refactor_project(second_root), add = TRUE)

      second_expected <- if (compare_golden) {
        file.path(expected_root, "results")
      } else {
        file.path(first_root, contract$assessment, "results")
      }
      compare_refactor_result_sets(
        file.path(second_root, contract$assessment, "results"),
        second_expected,
        tolerance = numeric_tolerance
      )
      if (compare_figures) {
        compare_refactor_figure_sets(
          file.path(second_root, contract$assessment, "figs"),
          file.path(expected_root, "figs")
        )
      }

      if (test_distinct_seed &&
          identical(contract$project, "vqa-demo2") &&
          identical(contract$assessment, "project_current")) {
        different_seed_root <- run_refactor_workflow(
          contract$project, contract$assessment,
          seed = seed + 1L
        )
        on.exit(cleanup_refactor_project(different_seed_root), add = TRUE)
        first <- read_csv_table(file.path(
          first_root, contract$assessment, "results", "SR_boot.q.csv"
        ))
        different <- read_csv_table(file.path(
          different_seed_root, contract$assessment,
          "results", "SR_boot.q.csv"
        ))
        testthat::expect_false(
          identical(first, different),
          info = "Different seeds produced the same empirical bootstrap table."
        )
      }
    }
  )
}

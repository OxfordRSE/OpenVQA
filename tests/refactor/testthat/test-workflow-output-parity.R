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

for (contract in contracts) {
  testthat::test_that(
    sprintf("%s/%s preserves the complete output contract", contract$project, contract$assessment),
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
        file.path(expected_root, "results")
      )
      compare_refactor_figure_sets(
        file.path(first_root, contract$assessment, "figs"),
        file.path(expected_root, "figs")
      )

      second_root <- run_refactor_workflow(
        contract$project, contract$assessment,
        seed = seed
      )
      on.exit(cleanup_refactor_project(second_root), add = TRUE)

      compare_refactor_result_sets(
        file.path(second_root, contract$assessment, "results"),
        file.path(expected_root, "results")
      )
      compare_refactor_figure_sets(
        file.path(second_root, contract$assessment, "figs"),
        file.path(expected_root, "figs")
      )
    }
  )
}

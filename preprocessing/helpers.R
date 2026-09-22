# Shared preprocessing helpers.

load_preprocessing_context <- function(data_root, assessment) {
  inputs_dir <- file.path(data_root, assessment, "inputs")
  file_map <- build_file_map(inputs_dir)
  project_config <- load_project_config(data_root, required = FALSE)
  params <- resolve_assessment_params(project_config$data, assessment)

  list(
    data_root = data_root,
    assessment = assessment,
    inputs_dir = inputs_dir,
    output_dir = inputs_dir,
    params = params,
    params_path = project_config$path,
    file_map = file_map,
    inputs = load_assessment_inputs(file_map)
  )
}

context_has_input <- function(context, file_name) {
  has_input(context$inputs, file_name)
}

context_input <- function(context, file_name) {
  load_input(context$inputs, file_name)
}

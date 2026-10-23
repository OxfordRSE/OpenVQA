# Shared preprocessing helpers.

load_preprocessing_context <- function(data_root, assessment, seed = NULL,
                                       defaults = list(), explicit = list()) {
  resolved <- resolve_runtime_config(data_root, assessment, seed,
                                     defaults = defaults, explicit = explicit)
  inputs_dir <- resolved$paths$inputs
  file_map <- build_file_map(inputs_dir)

  list(
    data_root = resolved$config$data_root,
    assessment = assessment,
    inputs_dir = inputs_dir,
    output_dir = inputs_dir,
    params = resolved$params,
    config = resolved$config,
    paths = resolved$paths,
    params_path = file.path(resolved$config$data_root, "params.yaml"),
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

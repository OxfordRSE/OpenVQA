prepare_analysis_inputs <- function(context) {
  if (identical(context$params$qh_method, "assume.0")) {
    return(context)
  }

  context <- prune_loaded_data_objects(context)
  context <- build_allowlists(context)
  context <- standardise_preprocessed_data(context)
  write_preprocessed_outputs(context)
  write_land_cover_summaries(context)
  context
}

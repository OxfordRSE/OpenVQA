#' Prune loaded data objects
#'
#' Apply the preprocessing pruning rules to the in-memory assessment inputs.
#' This keeps the preprocessing stage separate from validation and avoids
#' rewriting source files on disk.
#'
#' @param context A list containing loaded assessment inputs and related
#'   preprocessing state.
#' @return The updated context with pruned loaded data objects.
#' @keywords internal
prune_loaded_data_objects <- function(context) {
  # The legacy import pipeline pruned land-cover classes and orphan benchmark
  # plots in place on loaded objects. Rebuild that behaviour here, but keep it
  # strictly in-memory so the preprocessing stage does not rewrite source files.
  context <- prune_low_sample_land_cover(context)
  context <- prune_orphan_benchmark_plots(context)
  context
}

#' Prune low-sample land cover classes
#'
#' Remove land-cover classes whose `all.n.OK` flag is not `TRUE`, then drop
#' dependent plot metadata rows that no longer belong to a retained land-cover
#' class.
#'
#' @param context A list containing loaded assessment inputs and related
#'   preprocessing state.
#' @return The updated context with pruned `landCover.csv` and `plotMetadata.csv`
#'   inputs.
#' @keywords internal
prune_low_sample_land_cover <- function(context) {
  land_cover <- context_input(context, "landCover.csv")
  plot_metadata <- context_input(context, "plotMetadata.csv")

  if (is.null(land_cover)) {
    stop("Cannot prune land cover because landCover.csv is not loaded.", call. = FALSE)
  }
  if (is.null(plot_metadata)) {
    stop("Cannot prune land cover because plotMetadata.csv is not loaded.", call. = FALSE)
  }
  if (!"all.n.OK" %in% names(land_cover)) {
    stop(
      "Cannot prune land cover because landCover.csv does not expose all.n.OK.",
      call. = FALSE
    )
  }
  if (!"landCover" %in% names(land_cover)) {
    stop(
      "Cannot prune land cover because landCover.csv does not expose landCover.",
      call. = FALSE
    )
  }

  all_n_ok <- toupper(as.character(land_cover$all.n.OK)) %in% c("TRUE", "T", "1")
  keep_land_cover <- unique(land_cover$landCover[all_n_ok])
  if (!length(keep_land_cover)) {
    stop(
      "No land-cover classes satisfy all.n.OK == TRUE, so nothing can be kept.",
      call. = FALSE
    )
  }

  context$inputs[["landCover.csv"]] <- land_cover[
    land_cover$landCover %in% keep_land_cover, ,
    drop = FALSE
  ]

  keep_focal <- plot_metadata$focalOrBenchmark == "f" &
    plot_metadata$landCover %in% keep_land_cover
  keep_focal_veg <- unique(plot_metadata$vegClass[keep_focal])
  keep_benchmark <- plot_metadata$focalOrBenchmark == "b" &
    plot_metadata$vegClass %in% keep_focal_veg

  context$inputs[["plotMetadata.csv"]] <- plot_metadata[
    keep_focal | keep_benchmark, ,
    drop = FALSE
  ]

  context$prune <- list(
    kept_land_cover = keep_land_cover,
    kept_focal_veg_class = keep_focal_veg,
    kept_plot_codes = unique(context$inputs[["plotMetadata.csv"]]$plotCode)
  )

  context
}

#' Prune orphan benchmark plots
#'
#' Remove benchmark plots whose vegetation class is no longer represented by
#' any retained focal plot. Any loaded inputs with a `plotCode` column are then
#' filtered to the surviving plot codes.
#'
#' @param context A list containing loaded assessment inputs and related
#'   preprocessing state.
#' @return The updated context with orphan benchmark plots removed.
#' @keywords internal
prune_orphan_benchmark_plots <- function(context) {
  plot_metadata <- context_input(context, "plotMetadata.csv")
  if (is.null(plot_metadata)) {
    stop("Cannot prune benchmark plots because plotMetadata.csv is not loaded.", call. = FALSE)
  }
  if (!all(c("plotCode", "focalOrBenchmark", "vegClass") %in% names(plot_metadata))) {
    stop(
      paste(
        "plotMetadata.csv must expose plotCode, focalOrBenchmark, and vegClass",
        "before benchmark pruning."
      ),
      call. = FALSE
    )
  }

  focal_veg_classes <- unique(
    plot_metadata$vegClass[plot_metadata$focalOrBenchmark == "f"]
  )
  keep_benchmark <- plot_metadata$focalOrBenchmark == "b" &
    plot_metadata$vegClass %in% focal_veg_classes
  keep_plot_codes <- unique(
    plot_metadata$plotCode[plot_metadata$focalOrBenchmark == "f" | keep_benchmark]
  )

  context$inputs[["plotMetadata.csv"]] <- plot_metadata[
    plot_metadata$plotCode %in% keep_plot_codes, ,
    drop = FALSE
  ]
  context$prune$kept_plot_codes <- keep_plot_codes
  context$inputs <- prune_inputs_by_plot_codes(
    context$inputs,
    keep_plot_codes,
    exclude = c("landCover.csv", "plotMetadata.csv", "species.csv")
  )

  context
}

#' Filter loaded inputs by plot code
#'
#' Retain only rows whose `plotCode` value appears in the supplied keep set.
#' Tables listed in `exclude` are left unchanged.
#'
#' @param inputs A named list of loaded input data frames.
#' @param keep_plot_codes A character vector of plot codes to keep.
#' @param exclude A character vector of file names to leave unchanged.
#' @return The filtered input list.
#' @keywords internal
prune_inputs_by_plot_codes <- function(inputs, keep_plot_codes, exclude = character()) {
  for (file_name in names(inputs)) {
    if (file_name %in% exclude) {
      next
    }
    df <- inputs[[file_name]]
    if (is.null(df) || !is.data.frame(df) || !("plotCode" %in% names(df))) {
      next
    }
    inputs[[file_name]] <- df[df$plotCode %in% keep_plot_codes, , drop = FALSE]
  }
  inputs
}

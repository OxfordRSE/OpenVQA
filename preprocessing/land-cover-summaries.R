land_cover_summary_profile <- function(context) {
  if (identical(context$params$data_type, "mixed")) "mixed" else "cover"
}

summary_plot_counts <- function(summary, metadata, plot_codes, focal_name, benchmark_name) {
  plot_codes <- unique(plot_codes)
  rows <- metadata[metadata$plotCode %in% plot_codes, , drop = FALSE]
  focal <- rows[rows$focalOrBenchmark == "f", , drop = FALSE]
  benchmark <- rows[rows$focalOrBenchmark == "b", , drop = FALSE]
  focal_counts <- table(focal$landCover)
  benchmark_counts <- table(benchmark$vegClass)
  summary[[focal_name]] <- as.integer(focal_counts[match(summary$landCover, names(focal_counts))])
  summary[[benchmark_name]] <- as.integer(
    benchmark_counts[match(summary$vegClass, names(benchmark_counts))]
  )
  summary[[focal_name]][is.na(summary[[focal_name]])] <- 0L
  summary[[benchmark_name]][is.na(summary[[benchmark_name]])] <- 0L
  summary
}

summary_indicator_specs <- function(context) {
  specs <- list(
    SR = c("speciesCover.csv", "SR"),
    TD = c("speciesCover.csv", "TD"),
    TS = c("speciesCover.csv", "TD"),
    PCES = c("exoticCoverByStratum.csv", "PCESS"),
    PCESS = c("exoticCoverByStratum.csv", "PCESS"),
    PCGF = c("coverByGrowthForm.csv", "PCGF"),
    GC = c("groundCover.csv", "GC")
  )
  active <- unique(unlist(context$params$active_indicators, use.names = FALSE))
  specs[intersect(names(specs), active)]
}

build_land_cover_summaries <- function(context, n_min_abs = 4L) {
  land_cover <- context_input(context, "landCover.csv")
  metadata <- context_input(context, "plotMetadata.csv")
  profile <- land_cover_summary_profile(context)

  summary <- data.frame(
    landCover = land_cover$landCover,
    vegClass = land_cover$vegClass,
    lc.type = land_cover$lc.type,
    stringsAsFactors = FALSE
  )
  summary <- summary_plot_counts(
    summary, metadata, metadata$plotCode, "n.f.plots", "n.b.plots"
  )

  detailed <- summary
  if (identical(profile, "mixed")) {
    for (spec in list(
      c("speciesStems.csv", "speciesStems"),
      c("coverByGrowthForm.csv", "coverByStratum")
    )) {
      if (context_has_input(context, spec[[1L]])) {
        detailed <- summary_plot_counts(
          detailed, metadata, context_input(context, spec[[1L]])$plotCode,
          paste0("n.f.", spec[[2L]]), paste0("n.b.", spec[[2L]])
        )
      }
    }
  }

  for (spec in summary_indicator_specs(context)) {
    if (!context_has_input(context, spec[[1L]])) next
    detailed <- summary_plot_counts(
      detailed, metadata, context_input(context, spec[[1L]])$plotCode,
      paste0("n.f.", spec[[2L]]), paste0("n.b.", spec[[2L]])
    )
  }

  if (identical(profile, "cover")) {
    indicator_columns <- grep("^n\\.[fb]\\.(SR|TD|PCESS|PCGF|GC)$", names(detailed), value = TRUE)
    detailed$all.n.OK <- apply(
      detailed[indicator_columns], 1L, function(counts) all(counts >= n_min_abs)
    )
    area <- context$params$legacy_summary_area_ha %||% land_cover$area_ha
    summary$n.f.plots.veg <- detailed$n.f.SR
    summary$n.b.plots.veg <- detailed$n.b.SR
    summary$all.n.OK <- detailed$all.n.OK
    summary$ha <- area
    summary <- summary[c(
      "landCover", "lc.type", "vegClass", "n.f.plots", "n.b.plots",
      "n.f.plots.veg", "n.b.plots.veg", "all.n.OK", "ha"
    )]
    names(summary)[names(summary) == "lc.type"] <- "eg.type"
    names(summary)[names(summary) == "vegClass"] <- "bm.veg"
    detailed <- detailed[c(
      "landCover", "vegClass", "all.n.OK", "n.f.plots", "n.b.plots",
      indicator_columns
    )]
    names(detailed)[names(detailed) == "vegClass"] <- "bm.veg"
  } else {
    summary$n.f.plots.n.min <- ifelse(summary$n.f.plots >= n_min_abs, summary$n.f.plots, 0L)
    summary$n.b.plots.n.min <- ifelse(summary$n.b.plots >= n_min_abs, summary$n.b.plots, 0L)
    summary$all.n.OK <- summary$n.f.plots >= n_min_abs & summary$n.b.plots >= n_min_abs
    summary$area_ha <- land_cover$area_ha
    summary <- summary[c(
      "landCover", "vegClass", "lc.type", "area_ha", "n.f.plots", "n.b.plots",
      "n.f.plots.n.min", "n.b.plots.n.min", "all.n.OK"
    )]
    names(summary)[names(summary) == "vegClass"] <- "bm.veg"
    detailed <- detailed[c("landCover", "vegClass", "lc.type", setdiff(
      names(detailed), c("landCover", "vegClass", "lc.type")
    ))]
    names(detailed)[names(detailed) == "vegClass"] <- "bm.veg"
  }

  list(summary = summary, detailed = detailed)
}

write_land_cover_summaries <- function(context) {
  summaries <- build_land_cover_summaries(context)
  results_dir <- file.path(context$data_root, context$assessment, "results")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summaries$summary, file.path(results_dir, "lc.summary.csv"), row.names = FALSE)
  names(summaries$detailed)[names(summaries$detailed) == "landCover"] <- "Land cover class"
  names(summaries$detailed)[names(summaries$detailed) == "bm.veg"] <- "Benchmark vegetation"
  if ("lc.type" %in% names(summaries$detailed)) {
    names(summaries$detailed)[
      names(summaries$detailed) == "lc.type"
    ] <- "Vegetation structural type"
  }
  utils::write.csv(
    summaries$detailed,
    file.path(results_dir, "lc.summary.detailed.csv"),
    row.names = FALSE
  )
  invisible(summaries)
}

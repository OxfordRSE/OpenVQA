# Map application settings to the parameter names used by analysis scripts.
legacy_common_symbols <- c(
  library_load_silent = "LIB.LOAD.SILENT",
  logfile_basename = "LOGFILE.BASENAME",
  replace_log = "REPLACE.LOG",
  beta_algorithm = "beta.algorithm",
  use_quality_difference_raw = "use.q.diff.raw",
  quality_discount_method = "discount.method",
  beta_weight_method = "wt.method",
  beta_buffer = "buffer",
  test_difference_bootstrap = "test.diff.boot",
  permutation_reps = "perm.reps",
  infer_missing_cls = "infer.missing.cls",
  infer_missing_cls_strict = "infer.missing.cls.strict",
  td_outliers_append = "TD.OUTLIERS.APPEND",
  td_outlier_upper_threshold = "TD.OUTLIER.UPPER.THRESHOLD",
  td_bm_plots_deduplicate = "TD.BM.PLOTS.DEDUPLICATE",
  remove_ba_outliers = "REMOVE.BA.OUTLIERS",
  ba_outliers_stdevs = "BA.OUTLIERS.STDEVS",
  ba_outliers_append = "BA.OUTLIERS.APPEND",
  ba_outliers_upper_threshold = "BA.OUTLIERS.UPPER.THRESHOLD",
  quality_inverse_logit = "q.tr.logit.inverse",
  inverse_logit_beta = "logit.inverse.beta",
  zero_to_one = "zero_to_one",
  plot_overlap = "plot.overlap",
  plot_hist = "plot.hist",
  quality_legend_only = "quality.legend.only",
  quality_legend_percent = "Q.LEGEND.PERCENT",
  boot_graph_subsample = "boot.graph.subsample",
  show_overlap = "show.overlap",
  plot_boot_pdfs = "plot.boot.pdfs",
  plot_pdf = "plot.pdf",
  no_fit_keep_separate = "no.fit.keep.separate",
  group_mtext_line = "GROUP.MTEXT.LINE",
  workbook_drop_notes = "VQA.XLS.DROP.NOTES",
  workbook_remove_missing_quality = "VQA.XLS.REMOVE.Q.NA",
  workbook_drop_excluded = "VQA.XLS.DROP.INCLUDE.FALSE",
  summary_pluralize_strata = "VQA.SUMMARY.PLURALIZE.STRATA",
  plot_dists_fitted = "plot.dists_fitted",
  plot_dists_fitted_bm = "plot.dists_fitted_bm",
  plot_dists_fitted_bm_grouped = "plot.dists_fitted_bm_grouped",
  plot_dists_fitted_bm_grouped_nofit = "plot.dists_fitted_bm_grouped_nofit",
  plot_dists_fitted_bm_grouped_rescaled = "plot.dists_fitted_bm_grouped_rescaled",
  plot_dists_fitted_bm_grouped_rescaled_nofit = "plot.dists_fitted_bm_grouped_rescaled_nofit",
  plot_dists_fitted_bm_nofit = "plot.dists_fitted_bm_nofit",
  plot_dists_fitted_bm_rescaled = "plot.dists_fitted_bm_rescaled",
  plot_dists_fitted_bm_rescaled_nofit = "plot.dists_fitted_bm_rescaled_nofit",
  plot_dists_fitted_focal = "plot.dists_fitted_focal",
  plot_dists_fitted_focal_grouped = "plot.dists_fitted_focal_grouped",
  plot_dists_fitted_focal_grouped_nofit = "plot.dists_fitted_focal_grouped_nofit",
  plot_dists_fitted_focal_grouped_rescaled = "plot.dists_fitted_focal_grouped_rescaled",
  plot_dists_fitted_focal_grouped_rescaled_nofit = "plot.dists_fitted_focal_grouped_rescaled_nofit",
  plot_dists_fitted_focal_nofit = "plot.dists_fitted_focal_nofit",
  plot_dists_fitted_focal_rescaled = "plot.dists_fitted_focal_rescaled",
  plot_dists_fitted_focal_rescaled_nofit = "plot.dists_fitted_focal_rescaled_nofit",
  plot_dists_fitted_grouped = "plot.dists_fitted_grouped",
  plot_dists_fitted_grouped_nofit = "plot.dists_fitted_grouped_nofit",
  plot_dists_fitted_grouped_rescaled_nofit = "plot.dists_fitted_grouped_rescaled_nofit",
  plot_dists_fitted_nofit = "plot.dists_fitted_nofit",
  plot_dists_fitted_rescaled_nofit = "plot.dists_fitted_rescaled_nofit"
)

legacy_common_params <- function(config) {
  values <- config[names(legacy_common_symbols)]
  names(values) <- unname(legacy_common_symbols)
  values
}

legacy_project_symbols <- c(
  data_type = "DATA.TYPE", min_plots = "N.MIN.ABS",
  source_plot_data_filename = "RAW_PLOTDATA_FILENAME",
  missing_exotic_status_assume_native = "IS.EXOTIC.MISSING.ASSUME.NATIVE",
  force_prepare_raw = "force.prepare.raw", td_only = "TD.ONLY",
  remove_td_outliers = "REMOVE.TD.OUTLIERS", td_outlier_stdevs = "TD.OUTLIER.STDEVS",
  plot_figures_only = "PLOT.FIGS.ONLY", summary_only = "VQA.SUMMARY.ONLY",
  omit_quality_hectares = "QH.OMIT", functional_group_group_by = "Q.FG.GROUP.BY",
  functional_group_method = "Q.FG.METHOD", overall_quality_method = "Q.OVERALL.METHOD",
  include_plots_nodata = "INCLUDE.PLOTS.NODATA", nodata_sd = "NODATA.SD",
  bootstrap_reps = "boot.reps", qh_method = "QH.METHOD", figure_type = "FIG.TYPE",
  plot_legends = "plot.legends", legend_no_focal_benchmark = "LEGEND.NO.FB",
  show_sample_sizes = "show.n", legend_combined = "legend.combined",
  plot_dists_fitted_grouped_rescaled = "plot.dists_fitted_grouped_rescaled",
  plot_dists_fitted_rescaled = "plot.dists_fitted_rescaled", pcess_herbs_only = "pcess.herbs.only"
)

legacy_indicator_symbols <- c(
  name = "ei.name", distribution = "distn", quality_method = "q.method",
  has_stratum = "has.stratum", test_tail = "test.tail", benchmark_value = "bm.val",
  prepare_raw = "prepare.raw", convert_percent = "convert.percent", logit = "logit",
  remove_zero_cover_plots = "remove.zero.cover.plots", scale_abundance = "scale.abund",
  td_multiplier = "td.multiplier", td_multiplier_omit = "td.multiplier.omit",
  exclude_exotics = "exclude.exotics", na_to_zero = "NA.TO.ZERO"
)

legacy_project_params <- function(config) {
  values <- config[names(legacy_project_symbols)]
  names(values) <- unname(legacy_project_symbols)
  active <- unlist(config$active_indicators, use.names = FALSE)
  if (config$td_only) active <- "TD"
  values$EI.vec <- active
  values$EI.F.GROUPS <- cbind(
    EI = active,
    f.group = unname(unlist(config$indicator_groups[active]))
  )
  values$df.EI.FG <- as.data.frame(values$EI.F.GROUPS)
  values$EI.list <- data.frame(fname = active)
  values$EI.agg.list <- data.frame(fname = unlist(config$aggregate_indicators, use.names = FALSE))
  nodata <- unlist(config$vegetation_without_data, use.names = FALSE)
  values$veg.nodata <- if (length(nodata)) nodata else ""
  values$ei.params <- function(ei_code) {
    settings <- config$indicator_settings[[ei_code]]
    if (is.null(settings)) stop("Unknown indicator: ", ei_code, call. = FALSE)
    settings <- resolve_params(config$indicator_defaults, settings)
    parameters <- settings[names(legacy_indicator_symbols)]
    names(parameters) <- unname(legacy_indicator_symbols)
    if (is.null(parameters$bm.val)) parameters$bm.val <- NA
    if (is.null(parameters$td.multiplier)) parameters$td.multiplier <- NA
    omit <- unlist(parameters$td.multiplier.omit, use.names = FALSE)
    parameters$td.multiplier.omit <- if (length(omit)) omit else ""
    parameters$ei.data.type <- if (ei_code %in% c("SR", "TD")) config$data_type else "cover"
    inputs <- c(
      SR = "speciesCover", TD = "speciesCover", PCGF = "coverByGrowthForm",
      GC = "groundCover", PCESS = "exoticCoverByStratum"
    )
    parameters$df.input <- if (parameters$ei.data.type == "ind") {
      "speciesStems"
    } else {
      unname(inputs[[ei_code]])
    }
    parameters$source.file <- paste0(parameters$df.input, ".csv")
    parameters
  }
  values$nmds.params <- function(land_cover) {
    symbols <- c(
      verbose = "nmds.verbose", seed_enabled = "nmds.set.seed", seed = "nmds.seed",
      trymax = "nmds.trymax", dimensions = "nmds.k", max_iterations = "nmds.maxit"
    )
    parameters <- config$nmds[names(symbols)]
    names(parameters) <- unname(symbols)
    c(list(land.cover = land_cover), parameters)
  }
  values
}

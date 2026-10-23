list(assessments = list(project_baseline = list(
  DATA.TYPE = "mixed",
  N.MIN.ABS = 4, RAW_PLOTDATA_FILENAME = "Demo_data_main.xlsx",
  IS.EXOTIC.MISSING.ASSUME.NATIVE = TRUE, force.prepare.raw = TRUE,
  TD.ONLY = FALSE, REMOVE.TD.OUTLIERS = TRUE, TD.OUTLIER.STDEVS = 4,
  PLOT.FIGS.ONLY = FALSE, VQA.SUMMARY.ONLY = FALSE, QH.OMIT = FALSE,
  Q.FG.GROUP.BY = "indicator_group", Q.FG.METHOD = "generalized_mean",
  Q.OVERALL.METHOD = "mixed", INCLUDE.PLOTS.NODATA = TRUE,
  NODATA.SD = 4, boot.reps = 10, QH.METHOD = "empirical", FIG.TYPE = "png",
  plot.legends = TRUE, LEGEND.NO.FB = FALSE, show.n = FALSE,
  legend.combined = FALSE, plot.dists_fitted_grouped_rescaled = FALSE,
  plot.dists_fitted_rescaled = TRUE, pcess.herbs.only = FALSE,
  veg.nodata = ""
), project_current = list(
  DATA.TYPE = "mixed",
  N.MIN.ABS = 4, RAW_PLOTDATA_FILENAME = "Demo_data_main.xlsx",
  IS.EXOTIC.MISSING.ASSUME.NATIVE = TRUE, force.prepare.raw = TRUE,
  TD.ONLY = FALSE, REMOVE.TD.OUTLIERS = TRUE, TD.OUTLIER.STDEVS = 4,
  PLOT.FIGS.ONLY = FALSE, VQA.SUMMARY.ONLY = FALSE, QH.OMIT = FALSE,
  Q.FG.GROUP.BY = "indicator_group", Q.FG.METHOD = "generalized_mean",
  Q.OVERALL.METHOD = "mixed", INCLUDE.PLOTS.NODATA = TRUE,
  NODATA.SD = 4, boot.reps = 10, QH.METHOD = "empirical", FIG.TYPE = "png",
  plot.legends = TRUE, LEGEND.NO.FB = FALSE, show.n = FALSE,
  legend.combined = FALSE, plot.dists_fitted_grouped_rescaled = FALSE,
  plot.dists_fitted_rescaled = TRUE, pcess.herbs.only = FALSE,
  veg.nodata = ""
), offset_baseline = list(
  DATA.TYPE = "mixed",
  N.MIN.ABS = 4, RAW_PLOTDATA_FILENAME = "Demo_data_offsets.xlsx",
  IS.EXOTIC.MISSING.ASSUME.NATIVE = TRUE, force.prepare.raw = TRUE,
  TD.ONLY = FALSE, REMOVE.TD.OUTLIERS = TRUE, TD.OUTLIER.STDEVS = 4,
  PLOT.FIGS.ONLY = FALSE, VQA.SUMMARY.ONLY = FALSE, QH.OMIT = FALSE,
  Q.FG.GROUP.BY = "indicator_group", Q.FG.METHOD = "generalized_mean",
  Q.OVERALL.METHOD = "mixed", INCLUDE.PLOTS.NODATA = TRUE,
  NODATA.SD = 4, boot.reps = 10, QH.METHOD = "assume.0", FIG.TYPE = "png",
  plot.legends = TRUE, LEGEND.NO.FB = FALSE, show.n = FALSE,
  legend.combined = FALSE, plot.dists_fitted_grouped_rescaled = FALSE,
  plot.dists_fitted_rescaled = TRUE, pcess.herbs.only = FALSE,
  veg.nodata = ""
), offset_current = list(
  DATA.TYPE = "mixed",
  N.MIN.ABS = 4, RAW_PLOTDATA_FILENAME = "Demo_data_offsets.xlsx",
  IS.EXOTIC.MISSING.ASSUME.NATIVE = TRUE, force.prepare.raw = TRUE,
  TD.ONLY = FALSE, REMOVE.TD.OUTLIERS = TRUE, TD.OUTLIER.STDEVS = 4,
  PLOT.FIGS.ONLY = FALSE, VQA.SUMMARY.ONLY = FALSE, QH.OMIT = FALSE,
  Q.FG.GROUP.BY = "indicator_group", Q.FG.METHOD = "generalized_mean",
  Q.OVERALL.METHOD = "mixed", INCLUDE.PLOTS.NODATA = TRUE,
  NODATA.SD = 4, boot.reps = 10, QH.METHOD = "empirical", FIG.TYPE = "png",
  plot.legends = TRUE, LEGEND.NO.FB = FALSE, show.n = FALSE,
  legend.combined = FALSE, plot.dists_fitted_grouped_rescaled = FALSE,
  plot.dists_fitted_rescaled = TRUE, pcess.herbs.only = FALSE,
  veg.nodata = ""
)), indicators = list(
  SR = list(
    ei.name = "Species Richness",
    distn = "NBin", q.method = "empirical", ei.data.type = "cover",
    has.stratum = FALSE, test.tail = "both", bm.val = NA, df.input = "speciesCover",
    source.file = "speciesCover.csv", prepare.raw = FALSE, convert.percent = FALSE,
    logit = FALSE, remove.zero.cover.plots = FALSE, scale.abund = TRUE,
    td.multiplier = NA, td.multiplier.omit = "", exclude.exotics = FALSE,
    ei.multiplier = 1, normalize.m2 = FALSE, NA.TO.ZERO = TRUE
  ),
  TS = list(
    ei.name = "Taxonomic Similarity", distn = "Bet",
    q.method = "empirical", ei.data.type = "mixed", has.stratum = FALSE,
    test.tail = "lower", bm.val = NA, df.input = "speciesCover",
    source.file = "speciesCover.csv", prepare.raw = FALSE,
    convert.percent = FALSE, logit = FALSE, remove.zero.cover.plots = FALSE,
    scale.abund = TRUE, td.multiplier = NA, td.multiplier.omit = "",
    exclude.exotics = FALSE, ei.multiplier = 1, normalize.m2 = FALSE,
    NA.TO.ZERO = TRUE
  ), PCES = list(
    ei.name = "Percent Cover Exotic Species",
    distn = "Bet", q.method = "fixed", ei.data.type = "cover",
    has.stratum = FALSE, test.tail = "upper", bm.val = 0,
    df.input = "exoticCoverByStratum", source.file = "exoticCoverByStratum.csv",
    prepare.raw = FALSE, convert.percent = FALSE, logit = FALSE,
    remove.zero.cover.plots = FALSE, scale.abund = TRUE,
    td.multiplier = NA, td.multiplier.omit = "", exclude.exotics = FALSE,
    ei.multiplier = 1, normalize.m2 = FALSE, NA.TO.ZERO = TRUE
  ),
  PCGF = list(
    ei.name = "Percent Cover by Growth Form", distn = "Bet",
    q.method = "empirical", ei.data.type = "cover", has.stratum = TRUE,
    test.tail = "both", bm.val = NA, df.input = "coverByGrowthForm",
    source.file = "coverByGrowthForm.csv", prepare.raw = FALSE,
    convert.percent = FALSE, logit = FALSE, remove.zero.cover.plots = FALSE,
    scale.abund = TRUE, td.multiplier = NA, td.multiplier.omit = "",
    exclude.exotics = FALSE, ei.multiplier = 1, normalize.m2 = FALSE,
    NA.TO.ZERO = TRUE
  ), BA = list(
    ei.name = "Basal Area (m2/ha)",
    distn = "gamma", q.method = "empirical", ei.data.type = "ind",
    has.stratum = FALSE, test.tail = "both", bm.val = NA,
    df.input = "speciesStems", source.file = "speciesStems.csv",
    prepare.raw = FALSE, convert.percent = FALSE, logit = FALSE,
    remove.zero.cover.plots = FALSE, scale.abund = TRUE,
    td.multiplier = NA, td.multiplier.omit = "", exclude.exotics = FALSE,
    ei.multiplier = 1, normalize.m2 = 10000, NA.TO.ZERO = TRUE
  ),
  ASC = list(
    ei.name = "Abundance by Size Class", distn = "gamma",
    q.method = "empirical", ei.data.type = "ind", has.stratum = TRUE,
    test.tail = "both", bm.val = NA, df.input = "speciesStems",
    source.file = "speciesStems.csv", prepare.raw = FALSE,
    convert.percent = FALSE, logit = FALSE, remove.zero.cover.plots = FALSE,
    scale.abund = TRUE, td.multiplier = NA, td.multiplier.omit = "",
    exclude.exotics = FALSE, ei.multiplier = 1, normalize.m2 = FALSE,
    NA.TO.ZERO = FALSE
  )
), groups = structure(c(
  "SR", "TS",
  "PCES", "PCGF", "BA", "ASC", "composition", "composition", "integrity",
  "structure", "structure", "structure"
), dim = c(6L, 2L), dimnames = list(
  NULL, c("EI", "f.group")
)), basal_area = list(dbh = c(
  -1,
  0, 2.499, 2.5, 9.999, 10, 25, NA, NaN, Inf
), per_hectare = c(
  NA,
  NA, NA, 0.102265385859043, 1.6359189408724, 0.409061543436171,
  2.55663464647607, NA, NA, Inf
), per_100_m2 = c(
  NA, NA, NA, 0.00102265385859043,
  0.016359189408724, 0.00409061543436171, 0.0255663464647607, NA,
  NA, Inf
)))

list(
  DATA.TYPE = "cover", N.MIN.ABS = 6, force.prepare.raw = TRUE,
  TD.ONLY = FALSE, REMOVE.TD.OUTLIERS = TRUE, TD.OUTLIER.STDEVS = 4,
  PLOT.FIGS.ONLY = FALSE, VQA.SUMMARY.ONLY = FALSE, QH.OMIT = TRUE,
  Q.FG.GROUP.BY = "indicator", Q.FG.METHOD = "arithmetic",
  Q.OVERALL.METHOD = "gmean", INCLUDE.PLOTS.NODATA = TRUE,
  NODATA.SD = 3, boot.reps = 100, QH.METHOD = "empirical",
  FIG.TYPE = "png", plot.legends = TRUE, LEGEND.NO.FB = FALSE,
  show.n = FALSE, legend.combined = FALSE, plot.dists_fitted_grouped_rescaled = FALSE,
  plot.dists_fitted_rescaled = TRUE, pcess.herbs.only = TRUE,
  veg.nodata = "", EI.F.GROUPS = structure(c(
    "SR", "TD", "PCGF",
    "GC", "PCESS", "composition", "composition", "structure",
    "function", "integrity"
  ), dim = c(5L, 2L), dimnames = list(
    NULL, c("EI", "f.group")
  )), EI.vec = c(
    "SR", "TD", "PCGF",
    "GC", "PCESS"
  ), EI.agg.list = structure(list(fname = c(
    "PCGF",
    "PCESS", "GC"
  )), class = "data.frame", row.names = c(
    NA,
    -3L
  )), indicators = list(SR = list(
    ei.name = "Species Richness",
    distn = "NBin", q.method = "empirical", ei.data.type = "cover",
    has.stratum = FALSE, test.tail = "lower", bm.val = NA,
    df.input = "speciesCover", source.file = "speciesCover.csv",
    prepare.raw = FALSE, convert.percent = FALSE, logit = FALSE,
    remove.zero.cover.plots = FALSE, scale.abund = TRUE,
    td.multiplier = NA, td.multiplier.omit = "", exclude.exotics = FALSE,
    NA.TO.ZERO = TRUE
  ), TD = list(
    ei.name = "Taxonomic distance",
    distn = "gamma", q.method = "empirical", ei.data.type = "cover",
    has.stratum = FALSE, test.tail = "both", bm.val = NA,
    df.input = "speciesCover", source.file = "speciesCover.csv",
    prepare.raw = FALSE, convert.percent = FALSE, logit = FALSE,
    remove.zero.cover.plots = TRUE, scale.abund = FALSE,
    td.multiplier = 20, td.multiplier.omit = "", exclude.exotics = FALSE,
    NA.TO.ZERO = TRUE
  ), PCGF = list(
    ei.name = "Percent Cover by Growth Form",
    distn = "Bet", q.method = "empirical", ei.data.type = "cover",
    has.stratum = TRUE, test.tail = "both", bm.val = NA,
    df.input = "coverByGrowthForm", source.file = "coverByGrowthForm.csv",
    prepare.raw = FALSE, convert.percent = FALSE, logit = FALSE,
    remove.zero.cover.plots = FALSE, scale.abund = TRUE,
    td.multiplier = NA, td.multiplier.omit = "", exclude.exotics = FALSE,
    NA.TO.ZERO = TRUE
  ), GC = list(
    ei.name = "Ground Cover",
    distn = "Bet", q.method = "empirical", ei.data.type = "cover",
    has.stratum = TRUE, test.tail = "both", bm.val = NA,
    df.input = "groundCover", source.file = "groundCover.csv",
    prepare.raw = FALSE, convert.percent = FALSE, logit = FALSE,
    remove.zero.cover.plots = FALSE, scale.abund = TRUE,
    td.multiplier = NA, td.multiplier.omit = "", exclude.exotics = FALSE,
    NA.TO.ZERO = TRUE
  ), PCESS = list(
    ei.name = "Percent Cover Exotic Species",
    distn = "Bet", q.method = "fixed", ei.data.type = "cover",
    has.stratum = TRUE, test.tail = "upper", bm.val = 0,
    df.input = "exoticCoverByStratum", source.file = "exoticCoverByStratum.csv",
    prepare.raw = FALSE, convert.percent = FALSE, logit = FALSE,
    remove.zero.cover.plots = FALSE, scale.abund = TRUE,
    td.multiplier = NA, td.multiplier.omit = "", exclude.exotics = FALSE,
    NA.TO.ZERO = TRUE
  )), nmds = list(
    land.cover = "forest",
    nmds.verbose = FALSE, nmds.set.seed = TRUE, nmds.seed = 10,
    nmds.trymax = 1000, nmds.k = 3, nmds.maxit = 200
  ),
  RAW_PLOTDATA_FILENAME = "vqa-demo1_data.accdb",
  IS.EXOTIC.MISSING.ASSUME.NATIVE = TRUE
)

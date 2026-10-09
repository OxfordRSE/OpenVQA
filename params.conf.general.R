###################################################################
# General parameter confirmation messages
# Echo major parameters at the start of several VQA operations
# Sourced toward the end of "params.R" and also in project-
# specific parameters file to pick up any changes.
###################################################################

######################################
# Prepare display version of selected
# parameters
######################################

if (RAW_LANDCOVER_FILENAME == "") {
  lc.file.disp <- "[see plot data file]"
} else {
  lc.file.disp <- paste0("'", RAW_LANDCOVER_FILENAME, "'")
}

if (RAW_SPP_FILENAME == "") {
  spp.file.disp <- "[see plot data file]"
} else {
  spp.file.disp <- paste0("'", RAW_SPP_FILENAME, "'")
}

if (REMOVE.TD.OUTLIERS == TRUE) {
  f.outlier.disp <- F.TD.OUTLIERS
} else {
  f.outlier.disp <- "[n/a]"
}

MSG.IND <- "  Indicators: "
n.ei <- length(EI.vec)
for (i in 1:n.ei) {
  MSG.IND <- paste0(MSG.IND, EI.vec[i])
  if (i < n.ei) {
    MSG.IND <- paste0(MSG.IND, ", ")
  } else {
    MSG.IND <- paste0(MSG.IND, "\n")
  }
}

MSG.FG <- "  Functional groups: "
f.groups <- unique(EI.F.GROUPS[, c("f.group")])
n.fg <- length(f.groups)
for (i in 1:n.fg) {
  MSG.FG <- paste0(MSG.FG, f.groups[i])
  if (i < n.fg) {
    MSG.FG <- paste0(MSG.FG, ", ")
  } else {
    MSG.FG <- paste0(MSG.FG, "\n")
  }
}

if (QH.METHOD == "assume.0") {
  qh.method.disp <- "Q.fixed, assume Q=0"
} else if (QH.METHOD == "assume.1") {
  qh.method.disp <- "Q.fixed, assume Q=1"
} else {
  qh.method.disp <- "Empirical"
}

if (exists("BM.VEG.TRANSFORM")) {
  if (BM.VEG.TRANSFORM && exists("BM.VEG.TRANSFORM.VECTOR")) {
    bm.tr <- BM.VEG.TRANSFORM.VECTOR
    msg.bm.tr <- "  Transformations: \n"
    n.tr <- length(bm.tr)
    for (i in 1:n.tr) {
      msg.bm.tr <- paste0(msg.bm.tr, "    ", names(bm.tr[1]), "-->", bm.tr[i])
      msg.bm.tr <- paste0(msg.bm.tr, "\n")
    }
  }
}

# if ( job=="qh.net" ) {
#   # Prepare list of QH.NET parameters for display
#   msg.assess.current.all <- "  Current assessments: "
#   n.assess.current.all <- length(ASSESS.CURRENT.ALL)
#
#   for ( i in 1:n.assess.current.all ) {
#     msg.assess.current.all <- paste0( msg.assess.current.all, ASSESS.CURRENT.ALL[i] )
#     if ( i < n.assess.current.all ) {
#       msg.assess.current.all <- paste0( msg.assess.current.all, ", " )
#     } else {
#       msg.assess.current.all <- paste0( msg.assess.current.all, "\n")
#     }
#   }
# }

######################################
# General startup message
######################################
MSG.CONF.START <- ""
# MSG.CONF.START <- paste0( MSG.CONF.START, "  Base directory: ", BASEDIR, "\n" )
MSG.CONF.START <- paste0(MSG.CONF.START, "  Data location relative to src/: ", LOC_DATA_DIR, "\n")
MSG.CONF.START <- paste0(MSG.CONF.START, "  Project-specific script location relative to src/: ", LOC_PSFILES_DIR, "\n")
MSG.CONF.START <- paste0(MSG.CONF.START, "  Data directory: ", DATA_BASEDIR, "\n")

# Initialize job-specific messages
MSG.CONF <- ""
MSG.CONF.IMP <- ""
MSG.CONF.BATCH <- ""
MSG.CONF.QH.NET <- ""

if (job == "import") {
  ######################################
  # Import message
  ######################################

  # General import messages
  MSG.CONF.IMP <- paste0(MSG.CONF.IMP, "  Raw data directory: ", RAWDATADIR, "\n")
  MSG.CONF.IMP <- paste0(MSG.CONF.IMP, "  Input directory: ", INPUTDIR, "\n")
  MSG.CONF.IMP <- paste0(MSG.CONF.IMP, "  QH method: ", qh.method.disp, "\n")

  if (QH.METHOD == "empirical") {
    MSG.CONF.IMP <- paste0(MSG.CONF.IMP, "  Plot data file: '", RAW_PLOTDATA_FILENAME, "'\n")
    MSG.CONF.IMP <- paste0(MSG.CONF.IMP, "  Land cover file: ", lc.file.disp, "\n")
    MSG.CONF.IMP <- paste0(MSG.CONF.IMP, "  Species attribute file: ", spp.file.disp, "\n")
    MSG.CONF.IMP <- paste0(MSG.CONF.IMP, "  Data type: ", DATA.TYPE, "\n")
    MSG.CONF.IMP <- paste0(MSG.CONF.IMP, "  N.MIN.ABS: ", N.MIN.ABS, "\n")
    MSG.CONF.IMP <- paste0(MSG.CONF.IMP, "  Delete land cover where n.plots < N.MIN.ABS?: ", DELETE.LC.BELOW.N.MIN.ABS, "\n")
    MSG.CONF.IMP <- paste0(MSG.CONF.IMP, "  Replace include files (whitelist/blacklist): ", REPLACE.INCLUDE.FILES, "\n")
  } else {
    # Skip irrelevant parameters if using fixed quality (i.e., QH="assume.0" or "assume.1")
  }

  # Append project-specific confirmation messages, if any
  if (exists("MSG.CONF.IMP.PS")) MSG.CONF.IMP <- paste0(MSG.CONF.IMP, MSG.CONF.IMP.PS)
} else if (job == "vqa.batch") {
  ######################################
  # vqa.batch message
  ######################################

  # General vqa.batch messages
  if (PLOT.FIGS.ONLY == TRUE) {
    MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Input directory: ", INPUTDIR, "\n")
    MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Results directory: ", RESULTSDIR, "\n")
    MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Figures directory: ", FIGDIR, "\n")
    MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  PLOT.FIGS.ONLY: ", PLOT.FIGS.ONLY, "\n")
    MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Figure type: ", FIG.TYPE, "\n")
    MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Plot figures in folder dists_fitted_rescaled/: ", plot.dists_fitted_rescaled, "\n")
    MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Plot figures in folder dists_fitted_grouped_rescaled/: ", plot.dists_fitted_grouped_rescaled, "\n")
  } else {
    if (QH.METHOD == "empirical") {
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Input directory: ", INPUTDIR, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Results directory: ", RESULTSDIR, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Figures directory: ", FIGDIR, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  PLOT.FIGS.ONLY: ", PLOT.FIGS.ONLY, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Figure type: ", FIG.TYPE, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  VQA.SUMMARY.ONLY: ", VQA.SUMMARY.ONLY, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  TD.ONLY: ", TD.ONLY, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  boot.reps: ", boot.reps, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Infer missing confidence limits: ", infer.missing.cls, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, MSG.IND) # Append list of indicators
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, MSG.FG) # Append list of functional groups
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Remove TD NMDS outliers: ", REMOVE.TD.OUTLIERS, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  TD outlier plots file: ", f.outlier.disp, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Include no-data plots: ", INCLUDE.PLOTS.NODATA, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Q.FG.GROUP.BY: ", Q.FG.GROUP.BY, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Q.FG.METHOD: ", Q.FG.METHOD, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  Q.OVERALL.METHOD: ", Q.OVERALL.METHOD, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  QH.METHOD: ", qh.method.disp, "\n")
    } else {
      MSG.CONF.BATCH <- ""
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  QH method: ", qh.method.disp, "\n")
      MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, "  boot.reps: ", boot.reps, "\n")
    }
  }
  # Append project-specific confirmation messages, if any
  if (exists("MSG.CONF.BATCH.PS")) MSG.CONF.BATCH <- paste0(MSG.CONF.BATCH, MSG.CONF.BATCH.PS)
} else if (job == "qh.net") {
  ######################################
  # qh.net message
  ######################################

  # General qh.net messages
  MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  Current assessment: ", ASSESS, "\n")
  MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  Baseline assessment: ", ASSESS.BASELINE, "\n")
  MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  Current assessment input directory: ", QH.NET.INPUTDIR.CURRENT, "\n")
  MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  Baseline assessment input directory: ", QH.NET.INPUTDIR.BASELINE, "\n")
  MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  QH.net results directory: ", QH.NET.RESULTSDIR, "\n")
  MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  Include offset? ", INCLUDE.OFFSET, "\n")
  if (INCLUDE.OFFSET) {
    MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  Offset assessment: ", ASSESS.OFFSET, "\n")
    MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  Offset assessment input directory: ", QH.NET.INPUTDIR.OFFSET, "\n")
  }
  MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  boot.reps: ", boot.reps, "\n")
  if (exists("BM.VEG.TRANSFORM")) {
    MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  Transform selected vegetation types? ", BM.VEG.TRANSFORM, "\n")
    if (BM.VEG.TRANSFORM && exists("BM.VEG.TRANSFORM.VECTOR")) {
      MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, msg.bm.tr)
    }
  }
  MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  Allow trading up? ", ALLOW.TRADE.UP, "\n")
  MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  Prepare QH.net.p (potential quality hectares)? ", PREPARE.QH.NET.P, "\n")
  if (PREPARE.QH.NET.P) MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, "  Reset potential quality to zero for vegetation with zero area? ", QH.NET.P.ZERO.AREA.RESET, "\n")

  # Append project-specific qh.net confirmation message, if any
  if (exists("MSG.CONF.QH.NET.PS")) MSG.CONF.QH.NET <- paste0(MSG.CONF.QH.NET, MSG.CONF.QH.NET.PS)
} else {
  # Default messages
  MSG.CONF <- paste0(MSG.CONF, "  Input directory: ", INPUTDIR, "\n")
  MSG.CONF <- paste0(MSG.CONF, "  Results directory: ", RESULTSDIR, "\n")
}

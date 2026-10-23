# Common configuration inventory

The inventory follows the six assessment fixtures through indicator scripts, quality and
distribution includes, summaries, workbook output, and figure dispatch. It excludes unused
functions in `includes/functions.R` and standalone diagnostics. The expected common script
parameters for these assessments are recorded in
`tests/testthat/fixtures-common-params.R`.

## Application defaults

These settings have no project-specific assignments. Values live in root
`params.yaml`; definitions and descriptions live in `schema/params.schema.json`.
The explicit symbol mapping is in `analysis/legacy-params.R`. A disabled feature's switch
counts as a consumer because it decides which calculations or outputs run.

| Resolved key | Script parameter | Consumers |
| --- | --- | --- |
| `library_load_silent` | `LIB.LOAD.SILENT` | `libraries.R` |
| `logfile_basename` | `LOGFILE.BASENAME` | `analysis/legacy.R` |
| `replace_log` | `REPLACE.LOG` | `analysis/legacy.R` |
| `beta_algorithm` | `beta.algorithm` | `includes/quality.R`, `includes/graph.dists.R` |
| `use_quality_difference_raw` | `use.q.diff.raw` | `includes/quality.R` |
| `quality_discount_method` | `discount.method` | `includes/quality.R` |
| `beta_weight_method` | `wt.method` | `includes/quality.R` |
| `beta_buffer` | `buffer` | `includes/quality.R` |
| `test_difference_bootstrap` | `test.diff.boot` | `includes/quality.R` |
| `permutation_reps` | `perm.reps` | `includes/quality.R` |
| `infer_missing_cls` | `infer.missing.cls` | `vqa.summary.R` |
| `infer_missing_cls_strict` | `infer.missing.cls.strict` | `vqa.summary.R` |
| `td_outliers_append` | `TD.OUTLIERS.APPEND` | `indicators/td.R` |
| `td_outlier_upper_threshold` | `TD.OUTLIER.UPPER.THRESHOLD` | `indicators/td.R` |
| `td_bm_plots_deduplicate` | `TD.BM.PLOTS.DEDUPLICATE` | `indicators/td.R` |
| `remove_ba_outliers` | `REMOVE.BA.OUTLIERS` | `indicators/ba.R` |
| `ba_outliers_stdevs` | `BA.OUTLIERS.STDEVS` | `indicators/ba.R` |
| `ba_outliers_append` | `BA.OUTLIERS.APPEND` | `indicators/ba.R` |
| `ba_outliers_upper_threshold` | `BA.OUTLIERS.UPPER.THRESHOLD` | `indicators/ba.R` |
| `quality_inverse_logit` | `q.tr.logit.inverse` | `includes/quality.R` |
| `inverse_logit_beta` | `logit.inverse.beta` | `includes/quality.R` |
| `zero_to_one` | `zero_to_one` | `indicators/sr.R`, `indicators/asc.R` |
| `plot_overlap` | `plot.overlap` | `indicators/sr.R`, `indicators/td.R`, `indicators/ts.R`, `indicators/gc.R`, `indicators/pcess.R`, `indicators/pces.R`, `indicators/pcgf.R`, `indicators/ba.R`, `indicators/asc.R` |
| `plot_hist` | `plot.hist` | `includes/call.graph.dists.R` |
| `quality_legend_only` | `quality.legend.only` | `includes/graph.dists.R` |
| `quality_legend_percent` | `Q.LEGEND.PERCENT` | `includes/graph.dists.R` |
| `boot_graph_subsample` | `boot.graph.subsample` | `includes/overlap.R` |
| `show_overlap` | `show.overlap` | `includes/graph.dists.R` |
| `plot_boot_pdfs` | `plot.boot.pdfs` | `includes/overlap.R` |
| `plot_pdf` | `plot.pdf` | `includes/call.graph.dists.R` |
| `no_fit_keep_separate` | `no.fit.keep.separate` | `includes/graph.dists.R` |
| `group_mtext_line` | `GROUP.MTEXT.LINE` | `includes/graph.dists.R` |
| `workbook_drop_notes` | `VQA.XLS.DROP.NOTES` | `vqa.summary/vqa.summary.xl.R` |
| `workbook_remove_missing_quality` | `VQA.XLS.REMOVE.Q.NA` | `vqa.summary/vqa.summary.xl.R` |
| `workbook_drop_excluded` | `VQA.XLS.DROP.INCLUDE.FALSE` | `vqa.summary/vqa.summary.xl.R` |
| `summary_pluralize_strata` | `VQA.SUMMARY.PLURALIZE.STRATA` | `vqa.summary/vqa.summary.xl.R` |
| `plot_dists_fitted` | `plot.dists_fitted` | `includes/graph.dists.R` |
| `plot_dists_fitted_bm` | `plot.dists_fitted_bm` | `includes/graph.dists.R` |
| `plot_dists_fitted_bm_grouped` | `plot.dists_fitted_bm_grouped` | `includes/graph.dists.R` |
| `plot_dists_fitted_bm_grouped_nofit` | `plot.dists_fitted_bm_grouped_nofit` | `includes/graph.dists.R` |
| `plot_dists_fitted_bm_grouped_rescaled` | `plot.dists_fitted_bm_grouped_rescaled` | `includes/graph.dists.R` |
| `plot_dists_fitted_bm_grouped_rescaled_nofit` | `plot.dists_fitted_bm_grouped_rescaled_nofit` | `includes/graph.dists.R` |
| `plot_dists_fitted_bm_nofit` | `plot.dists_fitted_bm_nofit` | `includes/graph.dists.R` |
| `plot_dists_fitted_bm_rescaled` | `plot.dists_fitted_bm_rescaled` | `includes/graph.dists.R` |
| `plot_dists_fitted_bm_rescaled_nofit` | `plot.dists_fitted_bm_rescaled_nofit` | `includes/graph.dists.R` |
| `plot_dists_fitted_focal` | `plot.dists_fitted_focal` | `includes/graph.dists.R` |
| `plot_dists_fitted_focal_grouped` | `plot.dists_fitted_focal_grouped` | `includes/graph.dists.R` |
| `plot_dists_fitted_focal_grouped_nofit` | `plot.dists_fitted_focal_grouped_nofit` | `includes/graph.dists.R` |
| `plot_dists_fitted_focal_grouped_rescaled` | `plot.dists_fitted_focal_grouped_rescaled` | `includes/graph.dists.R` |
| `plot_dists_fitted_focal_grouped_rescaled_nofit` | `plot.dists_fitted_focal_grouped_rescaled_nofit` | `includes/graph.dists.R` |
| `plot_dists_fitted_focal_nofit` | `plot.dists_fitted_focal_nofit` | `includes/graph.dists.R` |
| `plot_dists_fitted_focal_rescaled` | `plot.dists_fitted_focal_rescaled` | `includes/graph.dists.R` |
| `plot_dists_fitted_focal_rescaled_nofit` | `plot.dists_fitted_focal_rescaled_nofit` | `includes/graph.dists.R` |
| `plot_dists_fitted_grouped` | `plot.dists_fitted_grouped` | `includes/graph.dists.R` |
| `plot_dists_fitted_grouped_nofit` | `plot.dists_fitted_grouped_nofit` | `includes/graph.dists.R` |
| `plot_dists_fitted_grouped_rescaled_nofit` | `plot.dists_fitted_grouped_rescaled_nofit` | `includes/graph.dists.R` |
| `plot_dists_fitted_nofit` | `plot.dists_fitted_nofit` | `includes/graph.dists.R` |
| `plot_dists_fitted_rescaled_nofit` | `plot.dists_fitted_rescaled_nofit` | `includes/graph.dists.R` |

## Project and runtime settings

| Classification | Values | Consumers / reason |
| --- | --- | --- |
| Project setting | `DATA.TYPE`, `force.prepare.raw`, `PLOT.FIGS.ONLY`, `VQA.SUMMARY.ONLY`, `QH.OMIT`, `TD.ONLY`, `boot.reps`, `Q.FG.GROUP.BY`, `Q.FG.METHOD`, `Q.OVERALL.METHOD`, `INCLUDE.PLOTS.NODATA`, `NODATA.SD` | Indicator preparation, quality calculations, and summary aggregation; project scripts assign these. |
| Project setting | `REMOVE.TD.OUTLIERS`, `TD.OUTLIER.STDEVS`, `pcess.herbs.only`, `ei.params()`, `nmds.params()` | TD/outlier handling and project-specific indicator definitions. |
| Project setting | `FIG.TYPE`, `plot.legends`, `LEGEND.NO.FB`, `show.n`, `legend.combined`, `plot.dists_fitted_grouped_rescaled`, `plot.dists_fitted_rescaled` | Project-controlled figure rendering and output selection. |
| Project setting | `EI.F.GROUPS`, `EI.agg.list`, `veg.nodata`, `eg.*`, project helper functions, `legacy_summary_area_ha` | Group membership, summary classification, and project-specific data handling. |
| Assessment override | `QH.METHOD`, active indicators | Demo 2 assumed-zero branch and indicator selection are defined in its project script and YAML manifest. |
| Runtime selection | Project data root, assessment, explicit seed and compatibility `set.seed`/`seed` | Runtime configuration selects the assessment and seed; analysis operations reset the RNG individually. |
| Derived value / application file convention | All directory paths; `DF.*`, input and output filenames, `EI.vec`, `EI.list`, `df.EI.FG` | Constructed in code. Application file conventions are defined in `params.R`. |
| Obsolete / inactive | `allow.discrete`, `plot.mean`, `remove.grid`, `VQA.XLS.NO.AREA` | No reads in the active scripts; retained only for compatibility. |
| Import-only / other commands | Raw filenames, sample-size import filters, include-file replacement, taxonomy/TNRS settings, `QH.NET.*`, offsets and trade-up configuration | Import, net-quality, and utilities are outside the six assessment analysis paths. |

Project YAML fields are catalogued and validated by the parameter schema.
Root defaults are loaded by module location, then project, assessment, and
explicit overrides are merged and validated at the runtime boundary. Validation-only
assessment resolution uses the same defaults and schema. The generic `resolve_params()`
helper only merges named lists.

Standalone callers (`import.R`, `qh.net.R`, indicator/summary scripts) source
`params.R`. That wrapper obtains application defaults from the same YAML and mapping.
Callable analysis injects the already resolved mapping into its private environment, so
the wrapper does not reread defaults or project YAML for the run. Project-specific R scripts
supply indicator definitions and project settings.

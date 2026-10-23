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

## Project analysis configuration

Both demo projects use root defaults plus their project `params.yaml` files.
`legacy_project_symbols` and `legacy_indicator_symbols` in `analysis/legacy-params.R`
map resolved settings to script parameters. The adapter derives indicator lists, groups,
input filenames, and the `ei.params()` and `nmds.params()` functions from that configuration.

| Settings | Consumers |
| --- | --- |
| `data_type`, `force_prepare_raw`, `td_only`, `plot_figures_only`, `summary_only` | Input selection and indicator dispatch. |
| `bootstrap_reps`, `include_plots_nodata`, `nodata_sd`, `remove_td_outliers`, `td_outlier_stdevs` | Indicator calculations, quality bootstrap, and TD outlier handling. |
| `qh_method`, `omit_quality_hectares`, `functional_group_group_by`, `functional_group_method`, `overall_quality_method`, `indicator_groups`, `aggregate_indicators` | Quality aggregation and summary output. |
| `figure_type`, `plot_legends`, `legend_no_focal_benchmark`, `show_sample_sizes`, `legend_combined`, `plot_dists_fitted_grouped_rescaled`, `plot_dists_fitted_rescaled` | Distribution figures. |
| `indicator_defaults`, `indicator_settings`, `nmds`, `pcess_herbs_only`, `vegetation_without_data` | Indicator scripts, NMDS fitting, and workbook indicator metadata. |
| `min_plots`, `legacy_summary_area_ha`, `source_plot_data_filename`, `missing_exotic_status_assume_native` | Workbook metadata and preprocessing summaries. |
| `stem_sampling` and indicator `normalize_area_m2` | Basal-area normalization using the sampled area for each DBH size class. |

Both demo 1 assessments share the same settings. Their input tables determine the
assessment differences. Root defaults contain application settings and indicator
definitions; project YAML supplies data type, bootstrap count, aggregation choices,
aggregate indicators, and herbs-only exotic cover. Abundance scaling is a shared default,
with a separate setting for taxonomic distance.

Demo 2 uses mixed data: species richness selects cover observations, taxonomic similarity
uses the project data type, and basal area and abundance by size class select stem records.
Basal area uses configured DBH sampling geometry; abundance by size class counts individuals
from standardized stem records. Indicator settings select the distributions, transformations,
and handling of missing observations.

The `offset_baseline` override explicitly selects `assume.0`, with no active or aggregate
indicators. Other assessments use empirical quality. Source-data labels for the offset
assessments are explicit overrides. Analysis settings do not depend on assessment-name
patterns. Baseline/offset relationships for net-quality calculations are not read by the
assessment analysis workflow.

Standalone commands such as `import.R` and `qh.net.R` load project parameter scripts through
`params.R`. Callable analysis supplies resolved project settings directly to that wrapper.

## Runtime and file conventions

Runtime arguments select the data root, assessment, and seed. Analysis operations reset
RNG seeds individually; NMDS has its own seed settings. Paths, input and output filenames,
and indicator lists are constructed in code. `params.R` defines application file conventions.

Raw-data loading, import filters, include-file replacement, taxonomy/TNRS settings, and
net-quality calculations are separate from assessment analysis. Parameters such as
`allow.discrete`, `plot.mean`, `remove.grid`, and `VQA.XLS.NO.AREA` have no reads in the
active scripts and are retained for standalone caller compatibility.

Root defaults are loaded by module location, then project, assessment, and explicit
overrides are merged and validated. Named objects merge by field; arrays replace previous
values. Validation-only assessment resolution uses the same defaults and schema.

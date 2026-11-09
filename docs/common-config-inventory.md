# Common configuration inventory

This inventory lists resolved settings consumed by callable indicator calculations,
quality summaries, workbooks and figures. Expected values for the six demo assessments
are recorded in the fixed parameter fixtures under `tests/testthat/`.

## Application defaults

These settings have no project-specific assignments. Values live in root
`params.yaml`; definitions and descriptions live in `schema/params.schema.json`.
Settings are passed explicitly to their consumers. A disabled feature switch is still a
consumer because it decides which calculations or outputs run.

| Resolved key | Consumers |
| --- | --- |
| `library_load_silent` | `analysis/libraries.R` |
| `logfile_basename` | `analysis/main.R` |
| `replace_log` | `analysis/main.R` |
| `beta_algorithm` | `includes/indicator-results-functions.R`, `includes/indicator-figure-functions.R` |
| `use_quality_difference_raw` | `includes/indicator-results-functions.R` |
| `quality_discount_method` | `includes/indicator-results-functions.R` |
| `beta_weight_method` | `includes/indicator-results-functions.R` |
| `beta_buffer` | `includes/indicator-results-functions.R` |
| `test_difference_bootstrap` | `includes/indicator-results-functions.R` |
| `permutation_reps` | `includes/indicator-results-functions.R` |
| `infer_missing_cls` | `analysis/quality-summary.R` |
| `infer_missing_cls_strict` | `analysis/quality-summary.R` |
| `td_outliers_append` | `indicators/td.R` |
| `td_outlier_upper_threshold` | `indicators/td.R` |
| `td_bm_plots_deduplicate` | `indicators/td.R` |
| `remove_ba_outliers` | `indicators/ba.R` |
| `ba_outliers_stdevs` | `indicators/ba.R` |
| `ba_outliers_append` | `indicators/ba.R` |
| `ba_outliers_upper_threshold` | `indicators/ba.R` |
| `quality_inverse_logit` | `includes/indicator-results-functions.R` |
| `inverse_logit_beta` | `includes/indicator-results-functions.R` |
| `zero_to_one` | `indicators/sr.R`, `indicators/asc.R` |
| `plot_overlap` | `indicators/sr.R`, `indicators/td.R`, `indicators/ts.R`, `indicators/gc.R`, `indicators/pcess.R`, `indicators/pces.R`, `indicators/pcgf.R`, `indicators/ba.R`, `indicators/asc.R` |
| `plot_hist` | `includes/indicator-figure-functions.R` |
| `quality_legend_only` | `includes/indicator-figure-functions.R` |
| `quality_legend_percent` | `includes/indicator-figure-functions.R` |
| `boot_graph_subsample` | `includes/indicator-figure-functions.R` |
| `show_overlap` | `includes/indicator-figure-functions.R` |
| `plot_boot_pdfs` | `includes/indicator-figure-functions.R` |
| `plot_pdf` | `includes/indicator-figure-functions.R` |
| `no_fit_keep_separate` | `includes/indicator-figure-functions.R` |
| `group_mtext_line` | `includes/indicator-figure-functions.R` |
| `workbook_drop_notes` | `reporting/workbook-tables.R` |
| `workbook_remove_missing_quality` | `reporting/workbook-tables.R` |
| `workbook_drop_excluded` | `reporting/workbook-tables.R` |
| `summary_pluralize_strata` | `reporting/workbook-tables.R` |
| `plot_dists_fitted` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_bm` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_bm_grouped` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_bm_grouped_nofit` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_bm_grouped_rescaled` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_bm_grouped_rescaled_nofit` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_bm_nofit` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_bm_rescaled` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_bm_rescaled_nofit` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_focal` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_focal_grouped` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_focal_grouped_nofit` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_focal_grouped_rescaled` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_focal_grouped_rescaled_nofit` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_focal_nofit` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_focal_rescaled` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_focal_rescaled_nofit` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_grouped` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_grouped_nofit` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_grouped_rescaled_nofit` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_nofit` | `includes/indicator-figure-functions.R` |
| `plot_dists_fitted_rescaled_nofit` | `includes/indicator-figure-functions.R` |

## Project analysis configuration

Both demo projects use root defaults plus their project `params.yaml` files.
Indicator settings and input filenames are derived by `indicator_parameters()` in
`analysis/indicator-settings.R`. NMDS receives `config$nmds` directly. Indicator groups,
active indicators and aggregate indicator lists come from the resolved configuration.

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
`params.R`. Callable analysis receives resolved configuration directly and does not load
that script or project parameter scripts.

## Runtime and file conventions

Runtime arguments select the data root, assessment, and seed. Analysis operations reset
RNG seeds individually; NMDS has its own seed settings. Paths, input and output filenames,
and indicator lists are constructed in code by the pipeline and analysis modules.

Raw-data loading, import filters, include-file replacement, taxonomy/TNRS settings, and
net-quality calculations are separate from assessment analysis. Parameters such as
`allow.discrete`, `plot.mean`, `remove.grid`, and `VQA.XLS.NO.AREA` have no reads in the
supported functions and are retained for standalone caller compatibility.

Root defaults are loaded by module location, then project, assessment, and explicit
overrides are merged and validated. Named objects merge by field; arrays replace previous
values. Validation-only assessment resolution uses the same defaults and schema.

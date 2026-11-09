# OpenVQA

[![R CI](https://github.com/OxfordRSE/OpenVQA/actions/workflows/r-tests.yml/badge.svg)](https://github.com/OxfordRSE/OpenVQA/actions/workflows/r-tests.yml)

## Run an assessment

**New to R or the command line?** Start with the [installation guide](INSTALLATION_GUIDE.md).

Install dependencies with `rv sync` from the repository root, then run:

```sh
Rscript validate.R --data-root data/vqa-demo1-min
Rscript validate.R --data-root data/vqa-demo2-min
Rscript analyse.R --data-root data/vqa-demo1-min --assessment main_001_current --seed 20260810
Rscript analyse.R --data-root data/vqa-demo2-min --assessment project_current --seed 20260810
```

`analyse.R` is the supported analysis entry point. It validates and prepares inputs,
calculates indicators, and writes CSV tables, the workbook, and figures under the selected
assessment. Add `--no-figures` for numerical output alone. Sourcing `analyse.R` defines
`run_analysis()` without executing it.

Use a [tagged release](https://github.com/OxfordRSE/OpenVQA/releases) for a named
software version. The root `VERSION` file records the application version;
run `Rscript analyse.R --version` to display it. `rproject.toml` configures R and dependencies.
See the [changelog](CHANGELOG.md) for release scope and known limitations.

The minimal demo projects contain validated inputs and declarative settings. Standalone
research, import, power-analysis, and net-quality-hectare commands retain historical
parameter-script interfaces and are outside the supported analysis workflow.

## Project data

`--data-root` selects a project directory, which may be inside or outside this repository.
`--assessment` selects one assessment subdirectory. A project can contain a baseline,
later assessments and offset assessments, each with its own inputs and outputs:

```text
<project>/
  params.yaml
  <assessment>/
    inputs/       # Prepared CSV inputs
    results/      # Generated CSV tables and workbook
    figs/         # Generated figures
    log/          # Analysis logs
```

Supply the input tables and project settings; output directories are created automatically.
Inputs include land-cover classes and their areas, plot metadata, species attributes and
the observations required by the selected indicators. The validated minimal projects show
complete examples. Preserve original field data and project-specific import/filter scripts
alongside the project for reproducibility; an optional `raw/` directory can hold source data.

Use descriptive, filesystem-friendly project and assessment names, and sortable assessment
names where chronology matters. Indicator codes such as `SR`, `PCGF` and `TD` select
calculations through `active_indicators` and prefix their output filenames. Codes are case
sensitive; callable implementations live in `indicators/`, with lowercase filenames such
as `sr.R`. PCS, CH and BAES are also supported and have regression coverage.

## Configuration contract

`schema/params.schema.json` defines parameter names, types, constraints, titles and
descriptions. It contains no defaults. Root `params.yaml` supplies OpenVQA defaults;
project `params.yaml` files supply project and assessment overrides. `pipeline/params.R`
is the single loading, resolution and validation boundary. Paths, assessment selection,
and seeds are explicit runtime arguments. Documentation and dashboard consumers should
read the schema annotations and resolved configuration, without parsing R source comments.

## Verification and coverage

```sh
Rscript tests/testthat.R
Rscript tests/refactor/testthat.R
Rscript tests/coverage.R
```

The standard-suite coverage job measures callable production functions in-process and
uploads `coverage.xml` to Codecov. Golden fixtures and subprocess launch counts do not
contribute to coverage. Add a Codecov badge only after the first successful CI upload
and verification of its badge URL.

## Developer setup

See [CONTRIBUTING.md](CONTRIBUTING.md) for dependency setup, Git hook installation,
formatting and lint checks, code organisation, and CI coverage and badge maintenance.
The [output-contract guide](tests/refactor/README.md#why-ci-is-intentionally-asymmetric)
explains platform differences, known fitting failures and the requirements for restoring
Ubuntu and Windows checks.

## What is VQA?

Vegetation Quality Assessment (VQA) is an application for estimating condition (quality) of reclaimed vegetation relative to undisturbed native vegetation, and quantifying negative and positive impacts on biodiversity in terms of quality hectares. VQA is also a loss-gain accounting workflow encompassing experimental design, field sampling, preparation of GIS inputs, statistical analysis, and reporting.

### What is quality?

Quality (Q) is an index of similarity to a benchmark of undisturbed native vegetation. Q varies between 0 (no similarity) and 1 (identical). Don’t get too hung up on the word “quality”. In the context of VQA, quality is simply shorthand for “statistical similarity to natural, undisturbed vegetation”. Restored vegetation with a quality of 1 is statistically indistinguishable (by field sampling and VQA analysis) from pristine native vegetation.

### What are quality hectares?

In addition to its use as a standalone measure of restoration progress, Q is also a discount factor used to convert actual hectares (Ha) to quality hectares (QHa), where QHa = Ha * Q (i.e., quality-discounted area).

QHa is commonly  using in loss-gain accounting as a standardized unit of comparison for balancing ecosystem losses due to industrial impacts against gains due to restoration and averted losses. QHa can be used to track progress to a Net Positive Impact (NPI), the point at which current QHa equals baseline QHa (the QHa of undisturbed native vegetation present prior to project impacts). Loss-gain accounting based on QHa has a long history of use in industry, where it also known as habitat hectares. VQA is simply a more recent, rigorous implementation of quality hectares and habitat hectares.

### What is the VQA workflow?

***Mapping***. The VQA workflow begins with a map: GIS is used to divide the project site into classes of vegetation, disturbance and—if applicable—restoration treatments. Next, sampling points for vegetation plots are randomly allocated across the landscape using a stratified-random sampling design, where the strata are the unique classes of vegetation, disturbance and reclamation. A table of all strata within the project, their attributes and area, is exported for use by the VQA application.

***Field data collection***. Vegetation plots at the project sites are established and measured at the random points assigned in the previous step. These are called the focal plots. Vegetation plots representing pristine, undisturbed examples of all vegetation types encountered at the project site are assembled. These are called the benchmark plots. Benchmark plots may be randomly sampled in undisturbed areas close to (but outside) the project site, or farther afield in the same region. In some cases, benchmark plots from existing databases may be used, as long as they were collected using the same methods as the focal plots.

***Data import and validation***. The field data are imported and validated by the VQA application. This usually involves a custom import template with transforms the company-specific data into the standard import schema required by VQA. Validation checks are performed both before and after standardization.

***Indicator quality***. Iterating through each indicator in each vegetation+disturbance+reclamation stratum, the VQA app fits separate probability density functions (PDFs) to the indicator distributions from the focal sample and the benchmark sample, using maximum likelihood. Indicator quality (Qi) is calculated as the overlap of the PDFs. Next, this process is repeated `bootstrap_reps` times, randomly sampling with replacement from the same data. This provides the bootstrap vectors which are used to calculate the 95% confidence limits of quality, and also saved for reuse when calculating derived values such as quality hectares.

VQA has other ways of calculating quality, under special circumstances. You’ll find more details in the [VQA publication](https://www.sciencedirect.com/science/article/pii/S1470160X23016527).

***Functional group quality and overall quality***. Individual Qi values for each stratum are averaged arithmetically within groups of indicators called “functional groups”, to give the functional group quality (Qf). A functional group combines related indicators that are safe to average arithmetically (meaning they are somewhat substitutable), such as percent cover of different growth forms or abundance of different size classes of trees. Next, all Qf are averaged geometrically to give the overall quality score (Q) of a stratum. Use of the geometric mean limits substitution of the quality scores in one functional group for another by dropping to zero if any component Qf drops to zero. During this process, the quality hectares of each stratum is also calculated by multiplying its Q by its area. For each derived values of Qi, 95% confidence limits are calculated by carrying forward the Qi bootstrap vectors into the calculations.

***Reporting***. Multiple, machine readable CSV results files are saved for each of the above steps. A final human-readable summary of the VQA analysis, including metadata (date, parameters values used), is prepared from these files as a formatted multi-sheet Excel workbook. In addition, one or more presentation-ready figures are generated for each indicator in each stratum showing the focal and benchmark sampling distributions, fitted PDFs and resulting quality scores.

***Net Quality Hectares***. Calculate the Net QHa of a later assessment relative to baseline QHa. Used to assess progress to NPI.

### VQA workflow optional steps

***Power analysis***. Monte Carlos power simulations to determine the minimum sample size (number of focal and benchmark plots) needed to achieve a conventional power of 0.80 at a range of effect sizes.

***NMDS***. Prepares a variety of NMDS figures, with diagnostics, using the NMDS output from indicator script `td.R` (indicator Taxonomic Distance). Useful for examining and removing outlier vegetation plots, or diagnosing causes of  excessively high variances within strata, which can sometimes be cause by poorly definited strata or benchmark vegetation classes which are mixes of different vegetation.


### Where can I learn more?

You can read more about VQA in detail in this open access [publication](https://www.sciencedirect.com/science/article/pii/S1470160X23016527).

<a name="dependencies"></a>

## Output
* Output from a single VQA analysis ("assessment") is saved to to an assessment directory of a specific project, within the data directory
* Output files are saved to `results/` and and figures are save to `figs/`

<a name="results"></a>
### Results
Results are saved as CSV files under `results/` in the assessment selected by `--data-root`
and `--assessment`. The analysis creates this directory automatically.

* Each indicator script produces the following output files:

```
SR_benchmark.csv
SR_boot.ol.csv
SR_boot.q.csv
SR_focal.csv
SR_pdf.b.csv
SR_pdf.f.csv
SR_pdf.xs.csv
SR_raw.csv
```

* The quality summary functions in `analysis/quality-summary.R` produce these files:

```
ei.summary.csv
summary_bm_ei.csv
summary_focal_ei.csv
summary_focal_fg.csv
summary_focal_fg_bm.csv
summary_focal_q.csv
summary_focal_q_all.csv
summary_focal_q_bm.csv
summary_q.ei_boot.csv
summary_q.overall_boot_bm.csv
summary_q.overall_boot_lc.csv
```

* The reporting functions in `reporting/main.R` produce these CSV summaries and the workbook:

```
vqa_summary_q.ei.csv
vqa_summary_q.fg.csv
vqa_summary_q.overall.csv
```

<a name="figures"></a>
### Figures
Figures are saved under `figs/` in the selected assessment. The analysis creates figure
directories automatically; `figure_type` selects the image format.

* Each indicator script creates the following figure subdirectories, if they do not already exist:

```
dists_fitted/
dists_fitted_bm/
dists_fitted_bm_grouped/
dists_fitted_bm_grouped_rescaled/
dists_fitted_bm_rescaled/
dists_fitted_focal/
dists_fitted_focal_grouped/
dists_fitted_focal_grouped_rescaled/
dists_fitted_focal_rescaled/
dists_fitted_grouped/
dists_fitted_grouped_rescaled/
dists_fitted_rescaled/
overlap/
```

### Figure subdirectories

Directories with names beginning `dists_fitted*` contain histograms with fitted probability density functions. Directory `overlap/` contains figures showing the overlapped focal and benchmark probability density functions only. In most cases you will use the full figures in the `dists_fitted*` directories. Directory contents are as follows:

#### Focal and benchmark distributions, combined and separate

Directories with names containing `*_focal_*` contain figures of focal distributions only. Directories with names containing `*_bm_*` contain figures of benchmark distributions only. If these strings are missing from the directory name, then the figures will contain both distributions in each figure.

#### Grouped strata figures

Directories with names containing `*_grouped_*` contain figures in which plots of different strata of the same indicator are grouped into a multi-panel figure. For example, plots of % cover herbs, % cover mosses, % cover shrubs and % cover trees would be grouped in a four panel figure with common header "Percent cover by growth form" and separate headers "Herbs", "Mosses", "Shrubs" and "Trees" over each panel.

#### Rescaled axes

Figures in directories with names ending in suffix `_rescaled` have the Y axis scaled to shown "Proportion of plots" rather than the absolute number of plots. In most cases, these are the only figures you should use. The rescaled figures allow more meaningful interpretation of the overlay of the probability density functions onto the sampling distribution histogram. Figures in directories without this suffix show absolute numbers of plots; in general you will not use the latter, but they can be helpful for visualizing differences in sample size between focal and benchmark distribution.

### Figure files
After running a VQA assessment, `dists_fitted*` directories will contain a separate figure for the results for each indicator in each sampling stratum (vegetation type + {seral stage] + [disturbance] + [reclamation]). Files are named as follows: `INDICATOR_dists_fitted_STRATUM.png`. For example, the plot for species richness in undisturbed alpine vegetation would be names `SR_dists_fitted_alpine.png`.

If an indicator consists of multiple vegetation strata, then each stratum will get its own figure. For example, if indicator "Percent cover by growth form" (PCGF) consists of strata "herbs", "moss", "shrub", and "tree", then the files generated for sampling stratum "Dry Forest, Mature" would be as follows:

```
PCGF_dists_fitted_dry_forest_mature_herb.png
PCGF_dists_fitted_dry_forest_mature_moss.png
PCGF_dists_fitted_dry_forest_mature_shrub.png
PCGF_dists_fitted_dry_forest_mature_tree.png
```

Directories for multi-panel figures of indicators with >1 stratum (i.e., directory names containing "*_grouped_*" will contain only figures for indicators with >1 stratum. For the preceding example of indicator "Percent cover by growth form" (PCGF) in sampling stratum "Dry Forest, Mature", a single four-panel figure would be generated, with file name `PCGF_dists_fitted_dry_forest_mature.png`.

<a name="references"></a>
## References

1. Boyle, B. L., R. E. Gullison, D. J. Vasiga, G. L. Luini, and W. F. Franklin. 2018. Vegetation Quality Assessment: Measuring Quality of Vegetation Communities to Support the Accounting Metrics of the Biodiversity Vision of Net Positive Impact for a Large-Scale Mining Operation. Page 41st Annual TRCR Mine Reclamation Symposium. The University of British Columbia, Fort Williams, BC, Canada.

# Vegetation Quality Assessment (VQA)

VQA is a semi-automated R pipeline for estimating quality of reclaimed vegetation relative to undisturbed native vegetation.

## Contents

- [What is VQA?](#overview)  
- [Dependencies](#dependencies)  
- [Naming conventions](#naming-conventions)  
- [Data](#data)

  - [Data directory structure](#data-directory-structure)  
  - [Data directory contents & naming conventions](#data-directory-descriptions)  
  - [Data files](#data-files)  

- [Code](#code)  
  - [Code directory structure](#code-directory-structure)
  - [Parameters](#parameters)  
  - [Import scripts](#import-scripts)  
  - [Indicator input preparation](#indicator-input-preparation)  
  - [Indicator scripts](#indicator-scripts)  
  - [Summary scripts](#summary-scripts)  
  - [Batch scripts](#batch-scripts)  
  - [Power analysis](#power-analysis)  
  - [Project-specific scripts](#project-specific-scripts)  
- [Output](#output)  
  - [Results](#results)  
  - [Figures](#figures)  
- [Usage](#usage)  
  - [1. Vegetation Quality Assessment](#vqa-pipeline)  
  - [2. Net Quality Hectares](#net-quality-hectares)  
- [References](#references)  

<a name="overview"></a>
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

***Data import and validation***. The field data are imported and validated by the VQA application. This usually involves a custom import template with transforms the company-specific data into the standard import schema required by VQA. Validation checks are performed both before and after standardization. [master script/Rscript command: `import.R`]

***Indicator quality***. Iterating through each indicator in each vegetation+disturbance+reclamation stratum, the VQA app fits separate probability density functions (PDFs) to the indicator distributions from the focal sample and the benchmark sample, using maximum likelihood. Indicator quality (Qi) is calculated as the overlap of the PDFs. Next, this process is repeated 10,000 times, randomly sampling with replacement from the same data. This provides the bootstrap vectors which are used to calculate the 95% confidence limits of quality, and also saved for reuse when calculating derived values such as quality hectares. [master script/Rscript command: `vqa.batch.R`]

VQA has other ways of calculating quality, under special circumstances. You’ll find more details in the [VQA publication](https://www.sciencedirect.com/science/article/pii/S1470160X23016527).

***Functional group quality and overall quality***. Individual Qi values for each stratum are averaged arithmetically within groups of indicators called “functional groups”, to give the functional group quality (Qf). A functional group combines related indicators that are safe to average arithmetically (meaning they are somewhat substitutable), such as percent cover of different growth forms or abundance of different size classes of trees. Next, all Qf are averaged geometrically to give the overall quality score (Q) of a stratum. Use of the geometric mean limits substitution of the quality scores in one functional group for another by dropping to zero if any component Qf drops to zero. During this process, the quality hectares of each stratum is also calculated by multiplying its Q by its area. For each derived values of Qi, 95% confidence limits are calculated by carrying forward the Qi bootstrap vectors into the calculations. [master script/Rscript command: `vqa.batch.R`]

***Reporting***. Multiple, machine readable CSV results files are saved for each of the above steps. A final human-readable summary of the VQA analysis, including metadata (date, parameters values used), is prepared from these files as a formatted multi-sheet Excel workbook. In addition, one or more presentation-ready figures are generated for each indicator in each stratum showing the focal and benchmark sampling distributions, fitted PDFs and resulting quality scores. [master script/Rscript command: `vqa.batch.R`]

***Net Quality Hectares***. Calculate the Net QHa of a later assessment relative to baseline QHa. Used to assess progress to NPI. [master script/Rscript command: `qh.net.R`]

### VQA workflow optional steps

***Power analysis***. Monte Carlos power simulations to determine the minimum sample size (number of focal and benchmark plots) needed to achieve a conventional power of 0.80 at a range of effect sizes. [master script/Rscript command: `vqa.power.R`]

***NMDS***. Prepares a variety of NMDS figures, with diagnostics, using the NMDS output from indicator script `td.R` (indicator Taxonomic Distance). Useful for examining and removing outlier vegetation plots, or diagnosing causes of  excessively high variances within strata, which can sometimes be cause by poorly definited strata or benchmark vegetation classes which are mixes of different vegetation. [master script/Rscript command: `nmds.R`]


### Where can I learn more?

You can read more about VQA in detail in this open access [publication](https://www.sciencedirect.com/science/article/pii/S1470160X23016527). 

<a name="dependencies"></a>
## Dependencies
* R >= 4.4.3   
* Multiple R packages (see script `libraries.R` for details)  

## Developer Setup

If you are working on the codebase itself, use the tracked `pre-commit` configuration to run formatting and linting automatically before commits.

1. Install the R project dependencies.
   - If you are using `rv`, run `rv sync` from the repository root.
2. Install `pre-commit` itself if it is not already available on your machine.
   - `pipx install pre-commit`
   - or `python -m pip install --user pre-commit`
3. Install the Git hook into your clone.
   - `pre-commit install`
4. If you want to check only the currently staged files before committing, run:
   - `pre-commit run --files $(git diff --cached --name-only --diff-filter=ACM)`

The local hooks run `styler` first and `lintr` second on staged `.R` and `.Rmd` files.

<a name="naming-conventions"></a>
## Naming conventions
* Pay special attention to the following naming conventions for files and directories
* These values are set in the project parameters files and are used to identify directories and files belonging to particular project and assessments, or the input/output for a single indicator.

##### Project code (parameter: PROJ)
* ***Definition***
   * The short code for a project. E.g., `via-demo`.
   * Must be globally unique
   * A project consists of a baseline assessment and one or more subsequent assessments, plus optional offsets (each of which has its own baseline and later assessments)
* ***Usage***
    * Used as the project data directory name, in the project-specific parameter file name (e.g., `params.vqa-demo.R`) and the project-specific import file name (e.g., `import.vqa-demo.R`).
* ***Structure***
   * Letters (recommend lower case), integers, hyphens, periods, underscores
   * Note that periods are used to separate the project code from other components of the directory or file name
   * Nothing else, no spaces!

##### ASSESSMENT_CODE
* ***Definition:***
	* A code for an individual assessment within a project
* ***Usage***
    * Used as the assessment data directory name (nested within the project data directory). 
    * Less commonly, different project-specific parameter files are used for different assessments by including the assessment code in the file name (e.g., `params.big-sky-mine.main-baseline.R` and `params.big-sky-mine.offset-baseline.R). This usage must be indicated by setting `PARAMS.USE.ASSESS <- TRUE` in the project-assessment parameters file (params.pa.R).
    * In the same manner, assessment codes can be included in the project-specific import file name (e.g., `import.big-sky-mine.main-baseline.R` and `import.big-sky-mine.offset-baseline.R). This usage must be indicated by setting `IMPORT.USE.ASSESS <- TRUE` in the project-assessment parameters file (params.pa.R).
* ***Structure***
	* Letters (recommend lower case), integers, underscores, hyphens
	* Nothing else, no spaces!

##### INDICATOR_CODE
* ***Definition:***
	* A short code for an indicator
	* Globally unique
* ***Usage***
   * Names of indicator-specific scripts
   * Names of indicator-specific output files and figures
* ***Structure***
	* Letters
	* Nothings else: no space, numbers or punctuation
	* Recommend lowercase. Be consistent because R is case sensitive, but avoid using case only to distinguish different codes

<a name="data"></a>
## Data

An example data directory `data/` is included in the source code base directory of VQA. However, we strongly recommend you move this directory *outside* the source code directory, and set `LOC_DATA_DIR <- “out”` in the main parameter file, `params.R`. The default location if `LOC_DATA_DIR <- “out”` is one level up from the source code directory. If you put it somewhere else, you will need to supply the custom path to parameter `DATA_BASEDIR_FINAL`.

Data includes the raw data and prepared input files, plus all VQA results files (CSVs and figures), as shown below.

<a name="data-directory-structure"></a>
### Data directory structure

This is the basic data directory structure that you MUST set up to run VQA:

```
data/
|-- PROJECT1_CODE/
|   |-- ASSESSMENT1_CODE/
|   |   `--raw/
|   `-- ASSESSMENT2_CODE/
|       `-- raw/
`-- PROJECT2_CODE/
    |-- ASSESSMENT1_CODE/
    |   `-- raw/
    |-- ASSESSMENT2_CODE/
    |   `-- raw/
    `-- ASSESSMENT3_CODE/
        `-- raw/
```

Once you run VQA, other directories will be created as needed. For example:

```
data/
|-- PROJECT1_CODE/
|   |-- ASSESSMENT1_CODE/
|   |   |-- figs/
|   |   |-- inputs/
|   |   |-- log/
|   |   |-- raw/
|   |   `-- results/
|   `-- ASSESSMENT2_CODE/
|       |-- figs/
|       |-- inputs/
|       |-- log/
|       |-- qh.net/
|       |-- raw/
|       `-- results/
`-- PROJECT2_CODE/
    |-- ASSESSMENT1_CODE/
    |   |-- figs/
    |   |-- inputs/
    |   |-- log/
    |   |-- raw/
    |   `-- results/
    |-- ASSESSMENT2_CODE/
    |   |-- figs/
    |   |-- inputs/
    |   |-- log/
    |   |-- raw/
    |   `-- results/
    |-- ASSESSMENT3_CODE/
        |-- figs/
        |-- inputs/
        |-- log/
        |-- qh.net/
        |-- raw/
        `-- results/
```

#### UNDER CONSTRUCTION BELOW THIS POINT ####

<a name="data-directory-descriptions"></a>
### Data directory contents & naming conventions

* **`data/`**: All inputs and output go here. You *MUST* create this directory, a project directory, and at least one assessment directory (`data/myproject/assessment_X/`) containing four subdirectories `raw/`, `inputs/`, `results/`, and `figs/`. All other subdirectories will be created by the application.   
* **`data/PROJECT_CODE/`**: Project directory. Contains all input data and results for a single project, across all assessments. Directory name is a unique code for the project.
* **`data/PROJECT_CODE/ASSESSMENT_CODE/`**: Assessment directory. All raw data, input files, results files and figures from a single assessment are saved here. This is `DATA_BASE_DIR`, set relative to main scripts in directory `scripts/` (e.g., `DATA_BASE_DIR="../data/myproject/assessment_001/"`) and declared in the parameters file, `params.R`. Over the course of a project, you create multiple assessment directories, beginning with the baseline (pre-project impact) assessment. Ideally, assessment folders should be named so that they sort sequentially, beginning with the baseline assessment, e.g.,: `assessment_000/`, `assessment_001/`, `assessment_002/`, or `000/`, `001/`, `002/`, etc. However, you may name them what you wish, as long as the paths are recorded in the `params.R`. Each assessment directory will have its own set of the five main subdirectories `raw/`, `inputs/`, `results/`, `figs/` and `custom_scripts`, as described below.  
* **`data/PROJECT_CODE/ASSESSMENT_CODE/figs/`**: All figures generated by VQA scripts are saved here. Subdirectories, if any, are generated automatically.  
* **`data/PROJECT_CODE/ASSESSMENT_CODE/inputs/`**: Prepared data inputs to VQA scripts. In typical workflow, raw data (in `data/PROJECT_CODE/ASSESSMENT_CODE/raw/`) are restructured by script `import.R` and saved to inputs directory in preparation for analysis; some of these files are then used by script `prepare.R` to prepare indicator-specific input files for VQA indicator scripts, which are again saved to directory `inputs/`.  
* **`data/PROJECT_CODE/ASSESSMENT_CODE/raw/`**: Raw data here. If raw files are organized in subdirectories, include these paths in `params.R`.  
* **`data/PROJECT_CODE/ASSESSMENT_CODE/results/`**: Results of VQA analysis pipeline saved here as csv file. In some cases, results files from earlier steps may serve as inputs for later steps.  
* **`data/PROJECT_CODE/ASSESSMENT_CODE/custom_scripts/`**: Back up scripts specific to a particular project here. At a minimum, include `import.R` and `params.R`.  

<a name="data-files"></a>
### Data files

***Raw data***  

* Raw data files read by import script only (import.<PROJECT_CODE>.R) and exported as generic input files after reformatting.
* **Plot data** (vegetation inventories). Each plot should be assigned to a land cover class. This may consists of a single file, or multiple files (for example, plant species cover could be in a separate file from plot metadata).
* **Land cover data**. All vegetation, disturbance and reclamation classes and their areas)
* **`species_codes.csv`**: [definition]  
* **`dist.eval.csv`**: [definition]  
* **`dist.eval.exceptions.csv`**: [definition]  

***Input files***

* In directory `inputs`
* Generic input files read directly by generic VQA scripts

***Results files (output)***

* All output from a single VQA analysis ("assessment") is saved to directories results/` and `figs/` in the assessment folder of a project
* See [Output](#output), below, for details

<a name="scripts"></a>
## Code
* The VQA code is organized as follows
* In this example, the repository base directory (i.e., the parent directory containing the contents of this repo) is called `src/`. You can call with whatever you want, but be sure to update all paths in the parameters files (`params<PROJECT_CODE>.R`) accordingly.
* 
<a name="code-directory-structure"></a>
### Code directory structure
* Only main scripts shown, including four examples of indicator 
scripts (pcgf.R, pcess.R, sr.R, td.R).

```
.
|-- data/
`-- src/
    |-- includes/
    |   |-- functions.R
    |   |-- graph.dists.R
    |   |-- overlap.R
    |   |-- quality.R
    |   `-- transformations.R
    |-- utilities/
    |   |-- access2csv/
    |   |   `-- access2csv.sh
    |   |-- access2mysql/
    |   |   `-- access2mysql.sh
    |   `-- tab2csv.R
    |-- filter.plots.R
    |-- filter.plots.project1.R
    |-- filter.plots.project2.R
    |-- import.project1.R
    |-- import.project2.R
    |-- params.R
    |-- params.project1.R
    |-- params.project2.R
    |-- pcess.R
    |-- pcgf.R
    |-- prepare.metadata.project1.R
    |-- prepare.metadata.project2.R
    |-- prepare.project1.R
    |-- prepare.project2.R
    |-- qh.net.R
    |-- qh.net.batch.project1.R
    |-- qh.net.prepare.sim.data.project1.R
    |-- sr.R
    |-- summary.R
    |-- td.R
    |-- vqa.batch.project1.R
    |-- vqa.batch.project2.R
    |-- vqa.power.R
    |-- vqa.power.ei.R
    |-- vqa.power.ei.plot.R
    |-- vqa.power.plot.R
    |-- vqa.power.plot.multi.R
    `-- vqa.summary.R

```

<a name="parameters"></a>
### Parameters

* **`params.PROJECT_CODE.R`**: Project-specific parameters file. All paths, file names and application options are set in this script. You should not need to edit any other script. Parameters are specific either to an individual assessment or NPI calculation (i.e., comparion of two assessments when calculating net Quality Hectares and progress to NPI). After running an assessment, you should back up your parameters file by saving it in the relevant assessment folder.
* **`params.R`**: Generic parameters file. This is generated automatically, for backwards-compatibility. Do not edit. This file will eventually be removed as its function is replaced completely by the project-specific parameters file.

<a name="import-scripts"></a>
### Import scripts

These script import and filter the raw data. They will be specific to particular projects and/or assessments, and should therefore be backed up in the custom_scripts directory of their respective projects. Include project code at end of file name, just before .R extension.

* **`import.PROJECT_CODE.R`**: Import and standardize raw data. Imports the raw data  from `data/PROJECT_CODE/ASSESSMENT_CODE/raw/` and produces standardized VQA input files, saving to directory `data/PROJECT_CODE/ASSESSMENT_CODE/inputs/`. As raw data structure and content may vary between assessments, requiring changes to the import file, you should back up the import file to the relevant assessment directory. 
* **`filter.plots.PROJECT_CODE.R`**: Include/exclude specific plots from the analysis. Use to exclude non-relevant data and outliers, or edit on the fly to compare the results of using different subsets of plots. Be sure to back up the final version used to the relevant assessment folder.

<a name="indicator-input-preparation"></a>
### Indicator input preparation

* **`prepare.PROJECT_CODE.R`**: Prepare indicator input files. Imports the standardized input files produced by `import.R` (from directory `data/PROJECT_CODE/ASSESSMENT_CODE/inputs/`) and generates the indicator-specific input files, saving them also to `data/PROJECT_CODE/ASSESSMENT_CODE/inputs/`. This script is specific to a particular project but not specific to a particular assessment. 

<a name="indicator-scripts"></a>
### Indicator scripts

Each indicator script performs a complete quality calculation for an individual indicator, saving the indicator-specific results files to `data/PROJECT_CODE/ASSESSMENT_CODE/results/` and figures to `data/PROJECT_CODE/ASSESSMENT_CODE/figs/`. The name of an indicator script is always the abbreviation of the indicator, follow by extention ".R": `INDICATOR_CODE.R`, e.g., `sr.R`. 

Examples:

* **`sr.R`**: Species richness (indicator "sr").  
* **`pcgf.R`**: Percent cover by growth form (indicator "pcgf"). 
* **`pces.R`**: Percent cover exotic species by stratum (indicator "pcess"). 
* **`td.R`**: NMDS taxonomic distance among plots (indicator "td"). 

Different projects may use different indicators. Output files from indicator scripts are saved to `data/PROJECT_CODE/ASSESSMENT_CODE/results/` and `data/PROJECT_CODE/ASSESSMENT_CODE/figs/`.

<a name="summary-scripts"></a>
### Summary scripts

These scripts combine the results of the individual indicator analyses to produce summaries of indicator quality, overall quality and quality hectares for a single assessment. Output is saved to `data/PROJECT_CODE/ASSESSMENT_CODE/results/` and figures to `data/PROJECT_CODE/ASSESSMENT_CODE/figs/`.

* **`prepare.metadata.PROJECT_CODE.R`**: Prepare inputs needed in addition to indicator quality results, such as summaries of land cover attributes and area. This will be specific to a particular project and shouldd be backed up in the `data/PROJECT_CODE/custom_scripts/` folder.
* **`summary.R`**: Summarize indicator quality, overall quality and quality hectares for each land cover class, including bootstrap 95% confidence limits. Inputs are the results files from in individual indicator scripts, plus land cover summary data generated by `prepare.metadata.R`.
* **`vqa.summary.R `**: Produce formatted tables, ready to paste into reports, using the output from `summary.R`.

<a name="batch-scripts"></a>
### Batch scripts

This master script runs a complete VQA analysis for a single assessment of a single project. Does not include power analysis.

* **`vqa.batch.PROJECT_CODE.R`**: Performs a complete start-to-finish analysis by sourcing the above scripts. Individual indicators can be included or omitted from the analysis by commenting/uncommenting the indicator script call. This script will usually differ among different projects (for example, in the indicators used), so be sure to back up to custom_scripts folder in data directory.
* **`qh.net.batch.PROJECT_CODE.R`**: Calculates Net Quality Hectare (QH.net) for one or more assessments relative to baseline.

<a name="power-analysis"></a>
### Power analysis

All power analysis scripts have "power" in the name. Each performs a different type of power analysis, and requires VQA analysis results files in directory `data/PROJECT_CODE/ASSESSMENT_CODE/results/`. See individual scripts for details.

<a name="project-specific-scripts"></a>
### Project-specific scripts

A subset of the scripts listed above are specific to individual projects (and sometimes assessments, although assessment-specific scripts should be avoided). You should back up each script to subdirectory `custom_scripts` in the relevant assessment directory. All scripts other than the following are generic and not specific to any particular assessment. The scripts listed below are present in most VQA pipelines; however, there may be other project-specific scripts as well. Except for file `params.R`, include project code at end of file name, just before .R extension.

* **`params.R`**
* **`import.PROJECT_CODE.R`**
* **`filter.plots.PROJECT_CODE.R`**
* **`prepare.metadata.PROJECT_CODE.R`**
* **`vqa.batch.PROJECT_CODE.R`**

<a name="output"></a>
## Output
* Output from a single VQA analysis ("assessment") is saved to to an assessment directory of a specific project, within the data directory
* Output files are saved to `results/` and and figures are save to `figs/`

<a name="results"></a>
### Results
Results are saved as comma-delimitted files (.csv) to directory `results/` in the assessment folder of a project, inside the data directory. The location of this directory is set in the main parameters file (params.R) and can be changed as needed. Directory `results/` and any subdirectories are automatically created if they do not exists already.

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

* The summary script `summary.R` produces the following output files:

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

* The summary script `vqa.summary.R` produces the following output files:

```
vqa_summary_q.ei.csv
vqa_summary_q.fg.csv
vqa_summary_q.overall.csv
```

<a name="figures"></a>
### Figures
Figures are saved as .png files to directory `figs/` in the assessment folder of a project, inside the data directory. The location of this directory is set in the main parameters file (params.R) and can be changed as needed. Directory `figs/` and any subdirectories are automatically created if they do not exists already.

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

<a name="usage"></a>
## Usage

<a name="vqa-pipeline"></a>
### 1. Vegetation Quality Assessment
Example VQA application demonstrating a single assessment of single project

#### A. File preparation

1. Place all required raw data files in the appropriate raw data directory. E.g., `data/teckev/000_baseline/raw/`.  
2. If starting from prepared input files instead of the raw data, make sure that all input files are present in the appropriate input data directory. E.g., `data/teckev/000_baseline/inputs/`.  
3. Check all parameters in the apprpropriate parameter file. For the above example, this would be `params.teckev.R`.    
4. At start of the VQA batch file, set parameter `params.file` equal to the name of the project-specific parameter file. The script will automatically make a copy of the project-specific parameter file to generic parameter file `params.R`. This is a change from the past requirement that you make the copy yourself; you no longer need to do this.   

**Important!** Make sure the indicator codes for all indicator scripts sourced in the batch file are included as indicator parameters in the parameters file. Any indicators not included as parameters will not be included in the final results, even if their scripts are run.

#### B. Analysis

**Method 1.** (Recommended). In Terminal, navigate to the VQA script base directory and run the following Rscript command:

***Syntax***

```
Rscript vqa.batch.PROJECT_CODE.R
```

***Example:***

```
Rscript vqa.batch.teck-ev.R
```
* Notes
   * Method 1 is much faster than method 2, and also provides more compact and readable progress & error message


**Method 2.** Paste code from `vqa.batch.PROJECT_CODE.R` into the R console or RStudio interface, or source as follows:

```
source("vqa.batch.PROJECT_CODE.R")
```

* Notes
   * Above example assumes your working directory is set to the same directory as the vqa.batch file.
   * This method will allow you to see files generated in real time, however, it is very slow



<a name="net-quality-hectares"></a>
### 2. Net Quality Hectares
Summarize QH.net for a one or more assessments and/or compare one or more post-impact assessments to the baseline assessment

#### A. File preparation

1. Make a copy of the project-specific parameter file and rename it `params.R`.  
2. Inspect and adjust parameters as needed in the qh.net script(s):  
     * Project-specific data preparation script (may or may not be present. E.g., `qh.net.prepare.sim.data.teckev.R`
     * Main qh.net script: `qh.net.R`: always present.

**Important!** Make sure required VQA results files (see parameters at start of `qh.net.R`) for each assessment are present in their respective assessment results directories.

#### B. Analysis

**Method 1.** Paste code from `qh.net.batch.PROJECT_CODE.R` into the R console or RStudio interface, or source as follows:

```
source("qh.net.batch.PROJECT_CODE.R")
```

**Method 2.** Run from a terminal window using the Rscript command:

```
Rscript qh.net.batch.PROJECT_CODE.R
```

<a name="references"></a>
## References

1. Boyle, B. L., R. E. Gullison, D. J. Vasiga, G. L. Luini, and W. F. Franklin. 2018. Vegetation Quality Assessment: Measuring Quality of Vegetation Communities to Support the Accounting Metrics of the Biodiversity Vision of Net Positive Impact for a Large-Scale Mining Operation. Page 41st Annual TRCR Mine Reclamation Symposium. The University of British Columbia, Fort Williams, BC, Canada.

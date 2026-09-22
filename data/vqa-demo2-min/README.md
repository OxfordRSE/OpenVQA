# VQA Demo #2

## Contents

- [Introduction](#introduction)  
- [Dependencies](#dependencies)  
- [Setup](#setup)  
- [Usage](#usage)  
- [Order of operations](#order)  
- [Inputs](#inputs)  
- [Output](#output)  

<a name="introduction"></a>
## Introduction

This example dataset demonstrates a complete VQA application, including calculation of quality scores, quality hectares and net quality hectares (NPI) for a project with one offset.

The raw data consists of an Excel spreadsheet containing separate tabs for benchmark vegetation, sampling strata (land cover), species, plots, species attributes, species cover data and structural measurements on individual trees. The dataset also tracks multiple sampling localities (sites) per stratum. 

This application demonstrates use of parameter value QH.METHOD=“assume0” to generate a baseline quality hectares assessment based on an assumption of 0% quality for all vegetation. The output from this assessment is essential for generating net quality hectares estimates by comparing a current empirical assessment to a zero-quality baseline assessment. 

This assessment also demonstrates how to calculate net quality hectares that include contributions of additional quality hectares from an offset.

<a name="dependencies"></a>
## Dependencies

* R >= 4.4.3   
* Multiple R packages, as loaded by “libraries.R” (see that script for details)  

<a name="setup"></a>
## Setup

Ensure you have the following data directory structure for this project:

```
data/
|__vqa-demo2/
   |__offset_baseline/
   |__offset_current/
   |__project_baseline/
   |__project_current/
   |__raw/
```

* Project directory `vqa-demo2` must be named exactly as shown.
* All asessment directories (e.g., `offset_baseline`) should be empty and named exactly as shown.
* Shared raw data directory `raw` must contain the following data files:
  * Demo_data_main.xlsx 
  * Demo_data_offsets.xlsx
  * Exotic_species.xlsx

### Project and assessment parameters

The only parameters you should need to set to run this demo are in the project and assessment parameters file, `params.pa.R`. Edit this file to ensure that this values of PROJ and ASSESS at the end of the file are as follows:

##### To run the main (project site) baseline assessment:

```
PROJ <- "vqa-demo2”
ASSESS <- 'project_baseline'
```

##### To run the main (project site) current assessment:

```
PROJ <- "vqa-demo2”
ASSESS <- ‘project_current'
```

…and so on…

### Other parameters

Numerous other parameters determine which raw data are used, where the data files are located, how the raw data is normalized to standard VQA indicator files, which indicators will be included in the analysis, the settings of the those indicators, the method used to calculate quality for each indicator, and how the individual indicator quality scores are averaged to obtain the final overall quality score for each type of land cover. The default values for all parameters are in params.R, and variant parameter values specific to the project being analyzed are in the project-specific parameter file, in subdirectory `params/`. Each project-specific parameter file is names strictly, as follows: params.<PROJ>.<ASSESS>.R.

For this demo, you do not need to change any of these parameters. The demos will just work. But after you get it running, you may want to inspect and tinker with these parameters to see how they behave. Be sure to save copies of the originals!

<a name="usage"></a>
## Usage

### 1. Import the raw data

##### From the command line:

```
Rscript import.R
```

Verify that the parameters displayed are correct and enter “y” to continue.

##### In RStudio:

Paste in contents of `import.R` into the console and press <enter> to run.

### 2. Run VQA

##### From the command line:

```
Rscript vqa.batch.R
```

Verify that the parameters displayed are correct and enter “y” to continue.


##### In RStudio:

Paste in contents of `vqa.batch.R` into the console and press <enter> to run.

### 3. Run Net Quality Hectares calculation

You MUST first complete all VQA analyses (i.e., by running vqa.batch.R) for all assessments before running net quality hectares, as the latter step requires VQA results from multiple VQA assessments as inputs.

##### From the command line:

First, make sure that params.pa.R is set to run the *current* assessment of the site for which you wish to calculate net quality hectares. The appropriate baseline assessment will be selected automatically based on the current assessment chosen. For example, if you are running next quality hectares for the project site, PROJ and ASSESS should be as follows:

```
PROJ <- "vqa-demo2”
ASSESS <- ‘main_current'
```

Now run qh.net:

```
Rscript qh.net.R
```

Verify that the parameters displayed are correct and enter “y” to continue.

Note: to include the offset in QH.net for the project site, you must *first* run a QH.net assessment for the offset site (relative to the offset baseline) and then set the following parameter in `params.vqa-demo2.R`:

```
INCLUDE.OFFSET <- TRUE
```

##### In RStudio:

Paste in contents of `qh.net.R` into the console and press <enter> to run.

<a name="order"></a>
## Order of operations

Both main (project site) assessments MUST be completed first, before running the offset assessments. This is because the offset assessments use the same benchmark plots as the main assessments. The offset assessment database contains offset focal plots only. Benchmark plots for the offset assessment are loaded in their normalized form from the input directory of the current main assessment. 

All VQA assessments MUST be completed (by running import.R and vqa.batch.R) BEFORE running any Net QH assessments (with qh.net.R). This is because qh.net.R compares quality hectares from a current assessment relative to its baseline assessment. In addition, if you wish to include offset quality hectares from an offset in the project site QH.net totals, you must have already completed the QH.net analysis for the offset. Including the offset in QH.net is achieved by setting parameter `INCLUDE.OFFSET <- TRUE`.  This causes the application to look for QH.net results in the offset qh.net folder.

For a complete analysis of the demo #2 dataset, including Net QH (with offset), the sequence of analysis should be as follows:

<div style="width:50px">Order</div> | <div style="width:50px">Main or offset</div> | <div style="width:90px">Assessment</div> | <div style="width:100px">Scripts\*</div> | Notes
:------: | :------ | :------ | :------ |  :------
1 | Main | Baseline | import, vqa.batch | Import & normalize raw data, calculate vegetation quality for all land cover classes
2 | Main | Current | import, vqa.batch | Import & normalize raw data, calculate vegetation quality for all land cover classes
3 | Offset | Baseline | import, vqa.batch | Import & normalize raw data, calculate vegetation quality for all land cover classes
4 | Offset | Current | import, vqa.batch | Import & normalize raw data, calculate vegetation quality for all land cover classes
5 | Offset | Current | qh.net | Net quality hectares of offset current relative to offset baseline
6 | Main | Current | qh.net | Net quality hectares of project current assessment to project baseline assessment, adding offset QH.net to final project QH.net. Must set `INCLUDE.OFFSET <- TRUE` to include offset. 

\*Script names minus ‘.R’ extension

<a name="inputs"></a>
## Inputs

### Raw data

VQA supports a diversity of plot data types and formats. One purpose of this flexibility is to allow the use of legacy plot datasets, especially as benchmark data. However, the Excel spreadsheet format use in VQA demo #1 (see raw data directory) is the more or less stable raw data format for new projects which are willing to adopt a “standard” VQA format. Note that the plot design and field sampling methods that produced the demo1 dataset was designed specifically for VQA.

Raw data are mapped and transformed to VQA standard input files by import.R, which invokes (via R command `source`) the project-specific parameter script in code subdirectory `params/` and the project-specific import script in code subdirectory `imports/`.

### VQA input files

The VQA analysis pipeline requires an input the set of standard CSV files found in data subdirectory `inputs/`. The total number of input scripts varies depending on the type of raw data and the indicators which will be calculated. For example, input file “speciesStems.csv” is created in the input/ data subdirectory of project vqa-demo2 but not vqa-demo1. This happens because the vqa-demo2 raw data contains tree mensuration data (measurements of individual tree stems), and the stem data gets normalized and dumped to “speciesStems.csv”, where is it used by vqa.batch to generate indicators “Abundance by size class” and “Basal Area”. By contrast, because project vqa-demo1 does not have stem data (only cover data), input file “speciesStems.csv” is not produced, and the indicators based on stem data are not used for the demo1 VQA.

Note that the format of an input file with a given name is completely standard. Projects that produce their raw data in the same format as the VQA input files should be able to execute vqa.batch directly, without running `import.R` first.

### Net quality hectares input files

A basic VQA net quality hectares analysis (script qh.net.R) compares current to baseline quality hectares. It requires the following VQA input and results files for each of the two assessments:

<div style="width:230px">File</div> | <div style="width:75px">Directory</div> | Contents
:------ | :------ | :------
landCover.csv | inputs/ | Land cover classes, their benchmark vegetation and area in hectares
summary_focal_qh_bm.csv | results/ | Quality hectares (QH) of each land cover class
summary_qh_boot_bm.csv | results/ | Bootstrapped QH values of each land cover class (for estimating confidence limits)

In addition, if offset quality hectares will be included in the QH.net calculations for the project site, then the following files must be present in qh.net results directory of the offset current assessment:

<div style="width:200px">File</div> | <div style="width:100px">Directory</div> | Contents
:------ | :------ | :------
qh.net_offset_current.csv | qh.net/results/ | QH.net results for the offset
qh.net_offset_current_boot.csv | qh.net/results/ | Bootstrapped QH.net values for the offset (for estimating confidence limits )

<a name="output"></a>
## Output

The results of running the above files are saved to the assessment directory set in `params.pa.R`.

### Import

Import (`import.R`) generates the standard VQA input files in directory and saves them to directory `input/`. 

### VQA

The VQA pipeline (`vqa.batch.R`) generates the files found in directory `results/`. The most important of these is `vqa_summary.R`, which provides a human-readable summary of all results, formatted as an Excel workbook. 

Figures are saved to directory `figs/`. Subdirectories within directory `figs/` group set of figures which represent the indicator distributions in various ways. The default is to output figure directory `dists_fitted_rescaled`. This is done by setting `plot.dists_fitted_rescaled <- TRUE`. The full set of figure options is found in section “Figure options” in the general parameters file, `params.R`.

### QH.net

Net quality hectares results are saved to directory `qh.net` in the assessment directory of the current assessment for the site being analysed (i.e., project vs offset). 

### Log files

All progress, warning and error message echoed to the screen are also saved to the log files in directory `log/`.





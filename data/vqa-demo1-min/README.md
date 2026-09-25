# VQA Demo #1

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

This example demonstrates a simple VQA application in which only vegetation quality is calculated for a project site and one offset site. This type of application is used by companies that perform their quality hectares calculations internally (or do not apply the quality hectares concept at all) and use VQA only to calculate vegetation quality.

The raw data consists of a database of vegetation plots for the project and offset sites. A summary file of project and offset land cover, vegetation and areas is not included. Land cover classes and benchmark vegetation types are instead extracted from plot metadata. This information is sufficient to calculate quality, but because land cover areas are unknown, quality hectares and net quality hectares cannot be determined. 

Other features of VQA demonstrated by this example include:  
* Extraction of raw data from a Microsoft Access database using R system dependency “mdb-tools” (see **Dependencies**, also **Set up**).  
* Subdivision of forested vegetation into seral stages (successional age classes). 

<a name="dependencies"></a>
## Dependencies

* R >= 4.4.3   
* R package “Hmisc”, for extracting data from Microsoft Access files  
* Hmisc dependency “mdb-tools”. This is a system dependency that runs only on unix and unix-like systems such as Mac OSX. Users running Windows will not be able to run this application directly. As a workaround, they could (a) use MS Access to dump the tables from the .accdb (MS Access) database to CSV files, and (b) modify the project-specific import script (`params.vqa-demo1.R`) to load the CSVs to same R data frames current populated by Hmisc.
* Other R packages loaded by “libraries.R” (see that script for details)  

<a name="setup"></a>
## Set up

### Data directory structure

The data directories for this demo are already set up and ready to go, but double check that they have the following structure:

```
data/
|__vqa-demo1/
   |__main_001_current/
   |__offset_001_current/
   |__raw/
```

* Project directory `vqa-demo1` must be named exactly as shown.
* Assessment directories `main_001_current` and `offset_001_current` should be empty and named exactly as shown.

### Raw data files
* Shared raw data directory `raw` must contain the compressed MS Access database file, `vqa_demo2_data.accdb.zip`. 
* Two other files in the raw data directory are `ei.stratum.include.csv` and `ei.stratum.veg.include.csv`. These files are generated automatically in the input directory during import. However, `ei.stratum.include.csv` has been modified to exclude indicators which are present in the data but not needed, and `ei.stratum.veg.include.csv` has been modified to re-include one indicator (“Ground cover: SubstrateWater”) for the “Wetland” vegetation only (i.e., the only vegetation type to which it is relevant). These modified files have be saved with the modified data to show how they can be modified to function as parameters that adjust which indicators in include/exclude. After running `import.R`, either modify the two `ei.stratum` files in directory `input` to match the equivalent files in the raw data directory (`raw/`) or simply replace them with with the latter files. You can run the demo without changing these files, but the final results (in summary spreadsheet vqa.summary.xlsx) will contain indicators which inflate the quality scores and increase their confidence limits and are therefore generally excluded.

### Parameters

### Project and assessment parameters

The only parameters you should need to set to run this demo are in the project and assessment parameters file, `params.pa.R`. Edit this file to ensure that this values of PROJ and ASSESS at the end of the file are as follows:

##### To run the main (project site) assessment:

```
PROJ <- "vqa-demo1"
ASSESS <- 'main_001_current'
```

##### To run the offset assessment:

```
PROJ <- "vqa-demo1"
ASSESS <- 'offset_001_current'
```

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

<a name="order"></a>
## Order of operations

An assessment for a project site is completed by running `import.R`, followed by `vqa.batch.R`. For this demo, the order in which you run the current and offset assessments relative to each other does not matter. In the table below, you could also run the offset current assessment first, and the project site current assessment second.

<div style="width:40px">Order</div> | <div style="width: 120px">Assessment</div> | <div style="width:80px">Scripts</div> | Operation
:------: | :------ | :------ |  :------
1 | main_001_current | import.R | Import & normalize raw data
2 | main_001_current | vqa.batch.R | Calculate vegetation quality for all land cover classes
3 | offset_001_current | import.R | Import & normalize raw data 
4 | offset_001_current | vqa.batch.R | Calculate vegetation quality for all land cover classes


<a name="inputs"></a>
## Inputs

### Raw data

VQA supports a diversity of plot data types and formats. One purpose of this flexibility is to allow the use of legacy plot datasets, especially as benchmark data. However, the Excel spreadsheet format use in VQA demo #1 (see raw data directory) is the more or less stable raw data format for new projects which are willing to adopt a “standard” VQA format. Note that the plot design and field sampling methods that produced the demo1 dataset was designed specifically for VQA.

Raw data are mapped and transformed to VQA standard input files by import.R, which invokes (via R command `source`) the project-specific parameter script in code subdirectory `params/` and the project-specific import script in code subdirectory `imports/`.

### VQA input files

The VQA analysis pipeline requires an input the set of standard CSV files found in data subdirectory `inputs/`. The total number of input scripts varies depending on the type of raw data and the indicators which will be calculated. For example, input file “speciesStems.csv” is created in the input/ data subdirectory of project vqa-demo2 but not vqa-demo1. This happens because the vqa-demo2 raw data contains tree mensuration data (measurements of individual tree stems), and the stem data gets normalized and dumped to “speciesStems.csv”, where is it used by vqa.batch to generate indicators “Abundance by size class” and “Basal Area”. By contrast, because project vqa-demo1 does not have stem data (only cover data), input file “speciesStems.csv” is not produced, and the indicators based on stem data are not used for the demo1 VQA.

Note that the format of an input file with a given name is completely standard. Projects that produce their raw data in the same format as the VQA input files should be able to execute vqa.batch directly, without running `import.R` first.

<a name="output"></a>
## Output

The results of running the above files are saved to the assessment directory set in `params.pa.R`.

Import (`import.R`) generates the standard VQA input files in directory and saves them to directory `input/`. 

The VQA pipeline (`vqa.batch.R`) generates the results files found in directory `results/`. The most important of these is vqa_summary.R, which provides a summary of all results as a human-readable Excel file. Other files in the results directory are intended to be machine-readable. Figures are saved to directory `figs/`. Subdirectories within directory `figs/` group set of figures which represent the indicator distributions in various ways. The default is to output figure directory `dists_fitted_rescaled`. This is done by setting `plot.dists_fitted_rescaled <- TRUE`. The full set of figure options is found in section “Figure options” in the general parameters file, `params.R`.

All progress, warning and error message echoed to the screen are also saved to the log files in directory `log/`.





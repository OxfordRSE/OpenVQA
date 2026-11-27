# Utility scripts

Custom software. 

`audit-distribution-inputs.R` counts original empirical beta/gamma samples with
boundary values or constant beta values in saved assessment results. It prints
totals without rerunning fits or changing results:

```sh
Rscript --vanilla utilities/audit-distribution-inputs.R tests/refactor/fixtures
```

Optionally supply a new CSV filename as the second argument for per-sample
details. The utility refuses to overwrite an existing CSV. It also accepts another
results tree containing matching `*_focal.csv` and `*_raw.csv` files.

Counts assume mixed beta scoring. Fixed scoring and gamma comparisons where both
samples are entirely zero are excluded. A unique dataset is a focal or benchmark
sample per assessment, indicator and stratum, distinguished by focal land cover
or benchmark vegetation class. Shared benchmarks count once in dataset totals.
An indicator-quality output is one land-cover/stratum comparison; either sample
can flag it.

The saved demo fixtures produce:

| Distribution | Eligible datasets | Datasets flagged for adjustment | Outputs using a flagged dataset |
| --- | --- | --- | --- |
| Beta | 339 | 211 (62.2%) | 152 / 201 (75.6%) |
| Gamma | 168 | 108 (64.3%) | 82 / 99 (82.8%) |

Beta flags cover samples containing 0 or 1, or constant samples; gamma flags cover
samples containing zero. All 49 constant beta samples in these fixtures also
contain boundary values, so they are already included in the 211.

These counts infer potential adjustments from saved samples; they do not observe
historical fitting calls, bootstrap replicates, retries or convergence. Saved raw
result tables already include input preparation, so the audit cannot measure
changes made before those tables were written. Counts describe these fixtures,
not the prevalence of statistically invalid or materially changed conclusions.
Select one results tree: including backup copies counts them separately.

Maybe be required for some variants of VQA pipeline. For example, access2mysql.sh dumps MSAccess database files to MySQL, where they can be further manipulated and prepared for import to VQA. Useful for systems such as Mac OS and Linux that can run MySQL but not Microsoft Access.

# Fixture notes

The fixtures exclude legacy import-only reports because they are not read by
the analysis, plotting, summary, or net-quality-hectare workflow:

- `siteStratumSummary.*.csv`
- `err.plots_assessCode_invalid.csv`

The first requires per-site raw-data fields absent from the minimal input; the
second is a raw-import diagnostic. Neither is part of the refactored output
contract.

# Validation checks

This directory contains the two validation stages used by `validate.R`:

- `validation/requirements.R` for the shared indicator input requirements.
- `validation/schema.R` for per-file JSON Schema validation.
- `validation/crossfile.R` for cross-file and indicator-aware checks.

The shared requirements table in `requirements.R` is used both to select files
for schema validation and to check core and indicator dependencies during
cross-file validation. Indicator requirements follow the dependency matrix in
`schema/README.md`; core requirements currently include `landCover.csv` for
quality-hectare calculations and `plotMetadata.csv` as the common plot-level
input for all indicator modules.

## `validation/schema.R`

| Check | Applied to | What it checks | Outcome |
| --- | --- | --- | --- |
| Input file exists | Each planned input file | The CSV file is present at the expected path before validation starts. | Missing files fail immediately. |
| Schema mapping exists | Each planned input file | The file name can be mapped to a JSON Schema file in `schema/inputs/`. | Unknown file names stop validation with an error. |
| JSON Schema conformance | Each existing input file | The CSV rows, converted to JSON, validate against the resolved schema using `jsonvalidate` with the `ajv` engine. | Schema violations fail. |
| Extra columns are tracked | Each existing input file | Columns in the CSV that are not declared in the resolved schema are collected in `extra_columns`. | This is reported, but it does not fail validation by itself. |

## `validation/crossfile.R`

### Indicator dependency checks

| Check | Applied to | What it checks | Outcome |
| --- | --- | --- | --- |
| Active indicator names are known | Whole assessment | Every requested indicator appears in `crossfile_manifest()`. | Unknown indicators fail. |
| Required files for each active indicator | Whole assessment | Each indicator's required inputs are present in the assessment file map. | Missing required files fail. |
| One-of species input selection | SR, TD, TS, CH | These indicators may use either `speciesCover.csv` or `speciesStems.csv`. The code records which option is present when one is available. | A present choice passes; missing choice groups are handled earlier by the file-plan logic in `validate.R`. |

### File-level crossfile checks

| Check | Applied to | What it checks | Outcome |
| --- | --- | --- | --- |
| `plotMetadata.csv` unique key | `plotMetadata.csv` | `plotCode` is unique. | Duplicate `plotCode` values fail. |
| `species.csv` lookup key | `species.csv` | If `speciesID` exists, it is unique. If not, the uniqueness check is skipped. | Duplicate `speciesID` values fail; missing `speciesID` is reported as a pass with a note. |
| `vegetation.csv` lookup key | `vegetation.csv` | `vegClass` is unique. | Duplicate `vegClass` values fail. |
| `plotMetadata.csv` vegetation lookup | `plotMetadata.csv` against `vegetation.csv` | Every `vegClass` exists in `vegetation.csv`, and the lookup merge preserves row count. | Missing lookup values or row multiplication fail. |
| `landCover.csv` vegetation lookup | `landCover.csv` against `vegetation.csv` | Every `vegClass` exists in `vegetation.csv`, and the lookup merge preserves row count. | Missing lookup values or row multiplication fail. |
| `landCover.csv` unique key | `landCover.csv` | `landCover` is unique. | Duplicate `landCover` values fail. |
| `landCover.csv` summary counts | `landCover.csv` against `plotMetadata.csv` | `focal_plots` and `bm_plots` are compared with counts derived from `plotMetadata.csv`. | Mismatches generate a warning. |
| `speciesCover.csv` unique key | `speciesCover.csv` | The pair `plotCode` + `species` is unique. | Duplicate rows fail. |
| `speciesCover.csv` foreign key | `speciesCover.csv` against `plotMetadata.csv` | Each row matches a `plotMetadata.csv` row on `plotCode`, `focalOrBenchmark`, `landCover`, and `vegClass`. | Unmatched rows fail. |
| `speciesCover.csv` merge cardinality | `speciesCover.csv` against `plotMetadata.csv` | Joining to `plotMetadata.csv` on the same four columns does not change row count. | Cardinality changes fail. |
| `speciesCover.csv` species lookup | `speciesCover.csv` against `species.csv` | Every observed `species` value appears in `species.csv` when that file is loaded. | Missing lookup values fail. |
| `speciesStems.csv` unique key | `speciesStems.csv` | `stem_id` is unique. | Duplicate `stem_id` values fail. |
| `speciesStems.csv` foreign key | `speciesStems.csv` against `plotMetadata.csv` | Each row matches a `plotMetadata.csv` row on `plotCode`, `focalOrBenchmark`, `landCover`, and `vegClass`. | Unmatched rows fail. |
| `speciesStems.csv` merge cardinality | `speciesStems.csv` against `plotMetadata.csv` | Joining to `plotMetadata.csv` on the same four columns does not change row count. | Cardinality changes fail. |
| `speciesStems.csv` species lookup | `speciesStems.csv` against `species.csv` | Every observed `species` value appears in `species.csv` when that file is loaded. | Missing lookup values fail. |
| `coverByGrowthForm.csv` unique key | `coverByGrowthForm.csv` | The pair `plotCode` + `stratum` is unique. | Duplicate rows fail. |
| `coverByGrowthForm.csv` foreign key | `coverByGrowthForm.csv` against `plotMetadata.csv` | Each row matches a `plotMetadata.csv` row on `plotCode`, `focalOrBenchmark`, `landCover`, and `vegClass`. | Unmatched rows fail. |
| `coverByGrowthForm.csv` merge cardinality | `coverByGrowthForm.csv` against `plotMetadata.csv` | Joining to `plotMetadata.csv` on the same four columns does not change row count. | Cardinality changes fail. |
| `exoticCoverByStratum.csv` unique key | `exoticCoverByStratum.csv` | The pair `plotCode` + `stratum` is unique. | Duplicate rows fail. |
| `exoticCoverByStratum.csv` foreign key | `exoticCoverByStratum.csv` against `plotMetadata.csv` | Each row matches a `plotMetadata.csv` row on `plotCode`, `focalOrBenchmark`, `landCover`, and `vegClass`. | Unmatched rows fail. |
| `exoticCoverByStratum.csv` merge cardinality | `exoticCoverByStratum.csv` against `plotMetadata.csv` | Joining to `plotMetadata.csv` on the same four columns does not change row count. | Cardinality changes fail. |
| `exoticCoverByStratum.csv` growth-form comparison | `exoticCoverByStratum.csv` against `coverByGrowthForm.csv` | Rows are compared against `coverByGrowthForm.csv` on `plotCode` and `stratum`, and cover values are checked against the growth-form totals. | Missing shared keys or cover values above the growth-form total generate warnings. |
| `groundCover.csv` unique key | `groundCover.csv` | The pair `plotCode` + `stratum` is unique. | Duplicate rows fail. |
| `groundCover.csv` foreign key | `groundCover.csv` against `plotMetadata.csv` | Each row matches a `plotMetadata.csv` row on `plotCode`, `focalOrBenchmark`, `landCover`, and `vegClass`. | Unmatched rows fail. |
| `groundCover.csv` merge cardinality | `groundCover.csv` against `plotMetadata.csv` | Joining to `plotMetadata.csv` on the same four columns does not change row count. | Cardinality changes fail. |

## Notes

- `validate.R` still decides which files are selected for validation.
- The crossfile checks assume `plotMetadata.csv` is available because several
  joins and key checks depend on it.
- Some checks are intentionally warning-level rather than hard failures, such
  as the `landCover.csv` summary comparison and the `exoticCoverByStratum.csv`
  comparison against `coverByGrowthForm.csv`.
- See [migration-checklist.md](migration-checklist.md) for a comparison of
  `import.R` against the new validation workflow.

# Modular JSON Schemas

This directory contains a modular JSON Schema design for VQA2 import outputs, focused on:

- `data/*/inputs/*.csv`

## Indicator Dependencies

This matrix summarizes the main input files each indicator module reads.
Auxiliary files like `plotMetadata.csv` and `species.csv` are shown separately.
The exact primary file can vary by project via `params.<PROJ>.R` or `params.<PROJ>.<ASSESS>.R`.

| Module | Primary input file(s) | Auxiliary file(s) | Notes |
| --- | --- | --- | --- |
| `SR` | `speciesCover.csv` or `speciesStems.csv` | `plotMetadata.csv`, `species.csv` | Species richness; species lookup used only for `is_exotic`. |
| `TD` | `speciesCover.csv` or `speciesStems.csv` | `plotMetadata.csv` | Species turnover/distance; no species lookup file. |
| `TS` | `speciesCover.csv` or `speciesStems.csv` | `plotMetadata.csv`, `species.csv` | Taxonomic similarity; species lookup used only for `is_exotic`. |
| `PCGF` | `coverByGrowthForm.csv` | `plotMetadata.csv` | Percent cover by growth form. |
| `PCS` | `coverByStratum.csv` | `plotMetadata.csv` | Percent cover by height stratum; the function merges plot metadata before filtering and completion. |
| `GC` | `groundCover.csv` | `plotMetadata.csv` | Ground cover by substrate class; the function merges plot metadata before filtering and completion. |
| `PCES` (`pces.R`) | `exoticCoverByStratum.csv` or params-driven equivalent | `plotMetadata.csv` | Percent exotic cover by plot, aggregated from stratum-level exotic cover. |
| `PCESS` | `exoticCoverByStratum.csv` | `plotMetadata.csv` | Percent exotic species cover by stratum. |
| `BA` | `speciesStems.csv` | `plotMetadata.csv`, `species.csv` | Basal area; species lookup used only for `is_exotic`. |
| `BAES` | `speciesStems.csv` | `plotMetadata.csv`, `species.csv` | Basal area exotic species; species lookup used only for `is_exotic`. |
| `ASC` | `speciesStems.csv` | `plotMetadata.csv`, `species.csv` | Abundance by size class; species lookup used only for `is_exotic`. |
| `CH` | `speciesCover.csv` or `speciesStems.csv` | `plotMetadata.csv`, `species.csv` | Same lookup pattern as SR/TS; module-specific behavior depends on params. |

This reverse view is still params-dependent, because several indicators can switch between cover and stem data.

| Input file | Possible indicator module(s) | Notes |
| --- | --- | --- |
| `speciesCover.csv` | `SR`, `TD`, `TS`, `CH` | Used when the indicator is configured to work from cover data rather than stem data. |
| `speciesStems.csv` | `SR`, `TD`, `TS`, `BA`, `BAES`, `ASC`, `CH` | Used by stem-based configurations and by indicators that always require stem measurements. |
| `coverByGrowthForm.csv` | `PCGF` | Current code path for percent cover by growth form. |
| `coverByStratum.csv` | `PCS` | The legacy name appears in comments/backwards-compatibility code, but the current PCS path uses `coverByStratum.csv`. |
| `exoticCoverByStratum.csv` | `PCES`, `PCESS` | Shared exotic-cover input for the plot-level and stratum-level exotic cover indicators. |
| `groundCover.csv` | `GC` | Direct dependency of the ground-cover indicator. |
| `plotMetadata.csv` | `SR`, `TD`, `TS`, `PCGF`, `PCS`, `PCES`, `PCESS`, `GC`, `BA`, `BAES`, `ASC`, `CH` | Common plot-level join table; used by modules that need land-cover, vegetation, or focal/benchmark context. |
| `species.csv` | `SR`, `TS`, `BA`, `BAES`, `ASC`, `CH` | Species lookup table for `is_exotic`. |

## Layout

- `base/common.defs.schema.json`: shared primitive types and enums.
- `base/row-patterns.schema.json`: reusable row object patterns.
- `inputs/*.schema.json`: one schema per input CSV file.
- `inputs/inputs.bundle.schema.json`: optional object-level bundle schema referencing all per-file schemas.

## Per-file schemas

- `coverByGrowthForm.schema.json`
- `coverByStratum.schema.json`
- `exoticCoverByStratum.schema.json`
- `groundCover.schema.json`
- `landCover.schema.json`
- `plotMetadata.schema.json`
- `species.schema.json`
- `speciesCover.schema.json`
- `speciesStems.schema.json`
- `vegetation.schema.json`

## Notes

- These schemas validate JSON row arrays converted from CSV.
- They intentionally prioritize structural and value-range checks that JSON Schema handles well.
- Cross-file checks (joins, grouped sample thresholds, pruning logic) remain runtime validation concerns.
- `ei.stratum.include.csv` and `ei.stratum.veg.include.csv` are generated settings files from the import workflow and are intentionally out of scope for this schema set.
- `coverByStratum.schema.json` is still provisional because there is no demo example file for it.

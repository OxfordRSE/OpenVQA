# Changelog

## 0.1.0 — 2026-11-09

Initial versioned release of the callable assessment backend.

- `analyse.R` validates and prepares standard inputs, calculates indicators and writes
  CSV tables, a summary workbook and figures.
- Application defaults and project/assessment settings use YAML, validated against
  `schema/params.schema.json` through `pipeline/params.R`.
- Six assessment output contracts check numerical tables, workbook contents, decoded
  figures and seeded repeatability against reviewed results.
- `VERSION` records the authoritative application version; `Rscript analyse.R --version`
  displays it.

### Support and reproducibility

The supported entry points are `analyse.R` and `validate.R`. Standalone raw import,
net-quality-hectare accounting, power analysis and NMDS diagnostics retain historical
interfaces and are outside this release's supported assessment workflow.

Reference CI uses macOS 26, R 4.5.2 and rv 0.22.2. Windows checks are disabled because
some fixture filenames are incompatible with Windows. Ubuntu numerical diagnostics
are disabled pending investigation of distribution-fitting failures on sparse,
zero-heavy and extreme-value inputs.

The root `rv.lock` is not tracked and root dependencies use moving repositories;
`rv sync` on a new installation can therefore resolve different package versions.
The parity CI environment has a separate committed dependency lock. Numerical and
decoded-image contracts apply to the reference environment; they do not promise
byte-identical workbook archives or identical floating-point results on every platform.
Workbook analysis timestamps are excluded from comparisons.

The assessment implementation is based on commit
`da144bff8e073af9f5f6a80938f7a465d362de7e`. Release metadata and CLI version reporting
were added afterwards without changing scientific calculations or fixtures.

See the [assessment quick-start](README.md#run-an-assessment),
[developer setup](CONTRIBUTING.md) and
[output-contract documentation](tests/refactor/README.md).

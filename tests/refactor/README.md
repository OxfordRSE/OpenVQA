# Assessment output contracts

These tests preserve the observable input-output contract. They run `analyse.R` against committed
minimal demo inputs and compare its complete `results` and `figs` trees with
reviewed golden fixtures:

```text
data/<project>-min
  -> analyse.R
  -> tests/refactor/fixtures/<project>/<assessment>/{results,figs}
```

Fixtures were generated from the corresponding non-`-min` legacy project.
The recorded source revision supplies provenance; these tests do not execute that code.

Run the suite locally with the already available root R environment:

```sh
Rscript tests/refactor/testthat.R
```

CI uses its frozen dependency manifest under `tests/refactor/` for reproducible numerical
contracts. Local development does not sync or activate that separate environment.

With no contract selected, the runner executes the six assessments in parallel using half
the detected logical CPUs, capped at six workers. Set `VQA_TEST_WORKERS=1` for the original
sequential behaviour or another positive integer to choose a different limit.

CSV and XLSX outputs are compared as tables, rather than raw files. The XLSX
`Meta` sheet's `Analysis date:` row is excluded because it records execution
time, not an analysis result. PNG files are decoded and compared by dimensions,
channels, and pixel values; PNG metadata and compression timestamps therefore
do not affect the result.

### Why CI is intentionally asymmetric

The golden fixtures were reviewed on a modern Apple Silicon macOS environment. They are a
regression contract for the refactor, not a claim that legacy optimizers produce identical
floating-point results on every operating system. CI therefore assigns each environment a
different, explicit responsibility:

| Environment | Current check | Reason |
| --- | --- | --- |
| Ubuntu 24.04 | Automatic, non-blocking numerical diagnostics across six assessments | Poor-quality inputs can make the legacy `fitdistr()` path abort before output comparison. |
| macOS 26 | All six contracts: strict tables, decoded figures, and repeatability | This runner reproduces the reviewed golden fixtures and is the temporary canonical parity environment. |
| Windows 2025 | Automatic numerical portability and repeatability checks across six assessments | Figure filenames use the same Windows-safe substitutions on every platform. |

Restore Ubuntu as a required portability check only after the legacy parametric-fitting
methodology has been made robust for sparse, zero-heavy, and extreme-value inputs.
The distinct-seed assertion remains active in the macOS 26
`vqa-demo2/project_current` shard.

CI selects one assessment per job with `VQA_TEST_CONTRACT=project/assessment`. The automatic
Ubuntu diagnostic uses `VQA_COMPARE_GOLDEN=false`: it requires the golden result-file
manifest, then compares two same-seed runs on Linux. macOS 26 retains strict golden
numerical and figure parity.
`VQA_NUMERIC_TOLERANCE` sets the table-comparison tolerance, and
`VQA_COMPARE_FIGURES=false` skips platform-rendered figure comparisons. Local runs keep
the strict golden comparison, compare figures, and exercise every assessment by default.
The distinct-seed check can be selected independently with `VQA_TEST_DISTINCT_SEED`.

## Creating or refreshing a fixture

Golden fixtures are reviewed scientific contracts. Refresh them only after explicit review
of an intended numerical or methodological change. Use a disposable copy of the validated
minimal project and `analyse.R` with an explicit `--seed`; never overwrite committed demo
inputs or automatically regenerate fixtures from tests or CI.

Compare new results with the recorded source revision and explain the intended differences.
After review, replace only the approved `results` and `figs` files and record the seed in
`seed.yaml`. Keep logs and temporary inputs out of the fixture set.

Figure filename components use `lt`, `leq`, `gt` and `geq` for `<`, `<=` (or `≤`),
`>` and `>=` (or `≥`), respectively, on all platforms. Other reserved symbols
(`:`, `"`, `|`, `?` and `*`) become `_`. For example, `Stems >=10 cm DBH` produces
`stems_geq10_cm_dbh`. Scientific labels in tables and figures retain their original text.

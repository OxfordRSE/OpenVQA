# Refactor compatibility tests

These tests preserve the observable input-output contract while the legacy VQA
workflow is replaced. They run the refactored workflow against committed
minimal demo inputs and compare its complete `results` and `figs` trees with
reviewed golden fixtures:

```text
data/<project>-min
  -> refactored workflow
  -> tests/refactor/fixtures/<project>/<assessment>/{results,figs}
```

Fixtures were generated from the corresponding non-`-min` legacy project.
The legacy code is not run by these tests and may be deleted once the fixtures
have been reviewed.

The suite has a temporary frozen dependency environment so that its numerical
golden fixtures do not change as CRAN packages are updated. Sync and run it from
the repository root with:

```sh
rv --config-file tests/refactor/rproject.toml sync --locked
R_LIBS_USER="$(rv --config-file tests/refactor/rproject.toml library)" \
  Rscript --vanilla tests/refactor/testthat.R
```

This uses `tests/refactor/rv.lock` and `tests/refactor/rv/library`. Normal
development and the standard CI checks continue to use the root
`rproject.toml` and `rv/library`. Remove the refactor configuration and lock
when this regression suite is retired.

CSV and XLSX outputs are compared as tables, rather than raw files. The XLSX
`Meta` sheet's `Analysis date:` row is excluded because it records execution
time, not an analysis result. PNG files are decoded and compared by dimensions,
channels, and pixel values; PNG metadata and compression timestamps therefore
do not affect the result.

## Creating or refreshing a fixture

Use the non-`-min` legacy project as the source and run it in a disposable
copy. This prevents stale outputs and changes to committed demo data:

```sh
fixture_project="<project>"
fixture_assessment="<assessment>"
fixture_seed=20260810
fixture_workdir="$(mktemp -d)"

cp -R "data/$fixture_project" "$fixture_workdir/$fixture_project"
Rscript -e 'args <- commandArgs(TRUE); for (name in c("results", "figs")) unlink(file.path(args[[1]], args[[2]], name), recursive = TRUE)' \
  "$fixture_workdir/$fixture_project" "$fixture_assessment"

printf 'y\n' | env \
  VQA_DATA_ROOT="$fixture_workdir/$fixture_project" \
  VQA_PROJECT="$fixture_project" \
  VQA_ASSESSMENT="$fixture_assessment" \
  VQA_TEST_SEED="$fixture_seed" \
  Rscript import.R

printf 'y\n' | env \
  VQA_DATA_ROOT="$fixture_workdir/$fixture_project" \
  VQA_PROJECT="$fixture_project" \
  VQA_ASSESSMENT="$fixture_assessment" \
  VQA_TEST_SEED="$fixture_seed" \
  Rscript vqa.batch.R
```

Review the temporary outputs, then replace only `results` and `figs` under
`tests/refactor/fixtures/<project>/<assessment>/`. Ensure `seed.yaml` records
the seed used. Do not copy logs, inputs, or stale files. Run the test suite and
review its diff before committing.

The minimal test input may intentionally omit legacy source data. If that
requires a reviewed fixture adjustment, document the exact change and reason
in a `README.md` beside that fixture. Do not refresh fixtures automatically
from tests or CI.

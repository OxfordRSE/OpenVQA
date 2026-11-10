# Developer setup

## Versioning and releases

The root `VERSION` file is the single source of the OpenVQA application version.
Run `Rscript analyse.R --version` from the repository root to display it.
Keep `rproject.toml` for the R environment: rv 0.22.2 and 0.23.3 do not accept an
application `version` field.

During pre-1.0 development, increment the patch version for compatible fixes and
documentation releases, and the minor version for new capabilities or deliberately
incompatible public interface/configuration/output changes. Explain changes in
`CHANGELOG.md`. A scientific behaviour change requires explicit review of its output
contract even when the interface is unchanged.

For a release, update `VERSION` and the changelog, run both minimal-project validations,
the standard suite and all six strict output contracts on the reference CI platform,
and ensure required checks pass for the exact main commit. Create an annotated
`v<version>` tag on that commit and publish matching GitHub release notes with setup
links, tested R/rv versions and known limitations. Tagged releases are immutable;
main continues development and corrections receive a new version. Never move a
published release tag to include a later fix.

## Enable checks before committing

After cloning the repository, run these commands from the repository root:

```sh
rv sync
pre-commit install
```

If the `pre-commit` command is unavailable, install it first with
`pipx install pre-commit`, or `python -m pip install --user pre-commit`.

Run `pre-commit install` once for **each clone**. The tracked configuration
does not enable the Git hook automatically; this command installs it into
`.git/hooks/pre-commit`.

On each commit, the hooks run `styler` and then `lintr` on staged `.R` and `.Rmd`
files. If `styler` changes files, review and stage the changes before retrying
the commit. `lintr` blocks commits containing lines over 100 characters;
`styler` does not automatically wrap every long line, so these may need manual
editing.

To run the checks on staged files without committing, use `pre-commit run`.

`rv sync` enforces the root manifest and can remove undeclared packages. Close R sessions
and IDE language servers before syncing packages they have loaded. Keep IDE tooling in a
separate user library or explicitly declare it if it belongs in the project environment.

## Code organisation

| Location | Responsibility |
| --- | --- |
| `analyse.R`, `validate.R` | Analysis and validation entry points. |
| `pipeline/` | File loading and resolved configuration. |
| `validation/` | Input schemas, required files and cross-file checks. |
| `preprocessing/` | Input filtering, standardisation and land-cover summaries. |
| `indicators/` | Callable calculations for individual ecological indicators. |
| `analysis/` | Analysis orchestration, fixed quality and quality aggregation. |
| `reporting/` | Summary tables and workbook generation. |
| `includes/` | Focused mathematical, plotting and output helpers. |
| `tests/testthat/`, `tests/refactor/` | Standard tests and assessment output contracts. |

## Tests and coverage

Run `Rscript tests/testthat.R` for standard checks and `Rscript tests/refactor/testthat.R`
for assessment output contracts. Use the root R environment; the test-specific dependency
manifest is for CI. `Rscript tests/coverage.R` measures production functions exercised
in-process by the standard suite and creates the ignored `coverage.xml` report.
The separate coverage CI job uploads that report using the
[Codecov Action](https://github.com/codecov/codecov-action).

## Continuous integration

[GitHub Actions](.github/workflows/r-tests.yml) runs on pull requests, pushes to `main`,
and manual dispatch. It validates both minimal demo projects, runs the standard suite,
lints selected R source and test files, and checks all six golden output contracts on
macOS 26, including same-seed repeatability and distinct-seed behaviour. A separate job
measures standard-suite coverage and uploads it to Codecov.

Ubuntu and Windows standard checks and all six Ubuntu and Windows output-contract shards run
without repository-variable gates. Ubuntu output diagnostics remain non-blocking while
parametric `fitdistr()` failures on sparse, zero-heavy and extreme inputs are investigated.
macOS retains strict golden numerical and figure comparisons; Ubuntu and Windows check
numerical portability and same-seed repeatability. See the
[platform rationale](tests/refactor/README.md#why-ci-is-intentionally-asymmetric).

The coverage job authenticates to Codecov using GitHub OIDC, with `id-token: write`
permission scoped to that job. It uploads only `coverage.xml` and does not require a
`CODECOV_TOKEN` secret.

On repository transfer, update the README's GitHub Actions badge and links to the receiving
owner. Install or authorize the Codecov GitHub App for that owner, verify an upload from
the new repository identity, then update the Codecov badge URL.
The current badge owner is `OxfordRSE/OpenVQA`. Add a coverage badge only after the first
successful CI upload and verification of its badge URL.

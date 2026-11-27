# Validation run reports

A validation run contains one independently usable report per assessment. The same
JSON Schema accepts a whole run, an extracted assessment or a stored collection. This lets a
project overview and assessment detail pages use the same evidence.

`pipeline/run-report.R` owns the common envelope and safe publication. `validate.R`
owns execution and aggregation; `validation/snapshot.R` records input/settings identity.
`validation/project-report.R` merges individual assessment updates into stored collections.
`reporting/` continues to generate scientific summary tables and workbooks.
Analysis reporting is deferred to B2.

## Invocation and selection

Run from the OpenVQA repository root, keeping `--project-root` at the project level:

```sh
# Validate every included assessment.
Rscript validate.R --project-root /work/project --report /work/project/report.json

# Validate one assessment using its project settings and assessment overrides.
Rscript validate.R --project-root /work/project --assessment baseline --report /work/reports/baseline-001.json
```

`-a NAME` is equivalent to `--assessment NAME`. Omit the flag for project execution.
The name must match the single-directory naming rule used by `analyse.R`
(letters/digits, underscores and hyphens), exist within the project and not be
excluded. Paths escaping the project, including external assessment-directory
symlinks, are rejected. Whole-project discovery retains its existing directory names.

`raw/` is excluded by default. `--exclude-dirs NAME` and trailing exclusions remain
supported. Every other immediate project subdirectory is treated as an assessment;
exclude unrelated directories explicitly. An explicitly selected excluded assessment
is an error, rather than silently overriding the exclusion.

The destination's parent directory must already exist. A file at the project root,
such as `/work/project/report.json`, needs no discovery exclusion. `--report PATH`
and `--report=PATH` work. Without `--report`, the same checks, per-assessment summaries
and exit rules apply to console-only execution; no report file is created automatically.

| Mode | Behaviour when the destination exists |
| --- | --- |
| `--report PATH` | Refuse to replace it; exit 2 before validation. |
| `--report PATH --overwrite` | Replace the entire report with this invocation's run. |
| `--report PATH --assessment NAME --update-report` | Replace or append only NAME in an existing run/collection. |

Both extra flags require `--report` and are mutually exclusive. `--update-report`
requires an explicit assessment and an existing report; it is invalid for a project
run. `--overwrite` also works for a new path. With `--assessment`, overwrite saves
**only that selected assessment**; use update to retain the other assessments.

```sh
# Rerun one assessment, retaining other stored assessment reports.
Rscript validate.R --project-root /work/project --assessment baseline --report /work/project/report.json --update-report

# Rerun the whole project and explicitly replace the previous report.
Rscript validate.R --project-root /work/project --report /work/project/report.json --overwrite
```

## Report structure

Report version **0.1** is independent of the application version in `VERSION`.
This is a new experimental reporting feature. The current schema accepts exactly
version 0.1; regenerate earlier development drafts at a fresh path or use a full
`--overwrite`. Their version numbers do not establish compatibility with this contract.

[The schema](../schema/run-report.schema.json) uses draft-07 and local definitions
only. Its root accepts `runReport`, `assessmentReport` or `collectionReport`; `assessmentReport`
is the reusable definition used inside a run's array. All three shapes can be validated
directly with the installed `jsonvalidate` AJV engine, without resolving external files.

```text
run envelope (report_type = "run")
  scope: project or explicitly selected assessment
  error: shared setup error, or null
  validation.assessments[]
    assessment envelope (report_type = "assessment")
      scope: exactly one assessment
      error: this assessment's execution error, or null
      validation.stages[]
        stage, status, reason
        checks[]: check_id, status, severity, message, file, details, extra_columns
      validation.inputs: versioned input-file manifest
      validation.settings: project params.yaml identity
  settings: comparison of stored assessment settings hashes
```

A one-assessment invocation still exports a run containing one child. Each child
has its own run ID, backend metadata, start/end times and outcome. Extracting it
requires no reconstruction or borrowed parent fields. Children are ordered like
the selected assessment inventory; checks retain scientific execution order within
each stage. Assessment context is stored once in the child scope; stage context is
stored once in its group. The independent child envelope deliberately keeps its
identity, backend and timestamps so it can be retained without its parent.

Envelope fields (scope-specific fields are labelled below):

| Field | Meaning |
| --- | --- |
| `report_version` | `major.minor`; currently `0.1`. |
| `report_type` | `run`, `assessment` or a stored `collection` (see below). |
| `operation` | `validate`; analysis is not yet supported. |
| `run_id` | Opaque execution identity, not an input fingerprint. |
| `scope.type` | Run only: `project` or explicitly selected `assessment`. |
| `scope.data_root` | Absolute normalized project path once resolved; requested path or null on early setup failure. |
| `scope.assessment` | Child only: the assessment name. |
| `scope.requested_assessment` | Run only: requested name, or null for project runs. |
| `scope.assessments` | Run only: ordered selected names, including names not reached after shared setup failure. |
| `scope.excluded_dirs` | Run only: requested exclusions. |
| `backend.version` | Application version, or null when unavailable. |
| `backend.revision` | Git HEAD when the application root has `.git` and Git works; otherwise null. Not proof of clean source. |
| `started_at`, `ended_at` | UTC timestamps with milliseconds and `Z`; validation times exclude subsequent publication. |
| `outcome` | `success`, `invalid_data` or `execution_error`. |
| `error` | Null, or `{message, stage}`. Its enclosing report supplies assessment/project context. |

Required unavailable scalars are explicit JSON `null`. Collections are arrays,
including empty and singleton collections. Readers must check the supported full
report version, operation and report type. While the report version is 0.x, the
contract is experimental and may change incompatibly. Unknown additional fields
can be ignored after the supported version has been checked.

## Checks, stages and outcomes

Each child's `validation.stages` contains the schema stage followed by the
cross-file stage. A group contains `stage`, `status`, `reason` and `checks`.
Assessment/stage names are inherited from those enclosing objects, rather than
repeated inside every check.

Each check requires `check_id`, `status`, `severity`, `message`, `file`, `details`
and `extra_columns`. File/details/extra columns may be null; extra columns retain
the existing comma-separated representation. Messages are human-readable, not a
parsing interface. Scientific conditions, check IDs, messages, warning classifications and pass/fail
results are preserved; passing error/info labels are normalized:

| Check status | Severity | Meaning |
| --- | --- | --- |
| `pass` | `null` | No issue was found. |
| `pass` | `warning` | An existing non-blocking warning/diagnostic. |
| `fail` | `error` | A blocking check failed. |

The schema rejects contradictory combinations, including `pass` with `error`.
Ordinary schema passes and `pass_result()` checks have null severity. Earlier
development drafts used error severity on passing schema checks and info severity on ordinary passes;
these now become null. Existing warning classifications, messages and counts are
preserved, including warning-labelled agreement diagnostics. A successful demo
therefore still succeeds with its warnings; warnings are never promoted to failures.

| Stage status | Meaning |
| --- | --- |
| `completed` | Returned a non-empty check collection; some checks may have failed. |
| `skipped` | Cross-file checks were blocked by schema failure; checks are empty. |
| `not_applicable` | Assumed-zero cross-file checks do not apply; checks are empty. |
| `not_run` | Processing stopped before this stage; checks are empty. |
| `error` | The stage did not finish; earlier completed check fragments may exist. |

`reason` is null for a completed stage and explanatory text otherwise. Completed
fragments are retained after each schema file and cross-file validation function
returns. A throwing function does not expose unfinished internal rows or fabricated
checks. `validation_report_rows(result)` reconstructs transient flat rows for R
callers and the existing console formatter; these rows are not duplicated in the
returned/exported report.

| Outcome | CLI exit | Meaning |
| --- | --- | --- |
| `success` | 0 | All selected assessment reports succeeded and requested export succeeded. |
| `invalid_data` | 1 | At least one child has failing checks and none has an execution error. |
| `execution_error` | 2 | Shared setup failed or any assessment has an execution error. |
| No report guaranteed | 2 | Export failed, including invalid/existing destinations. |

Within an assessment, failed checks block success and warnings remain non-blocking.
A warning is a passing check with a non-blocking diagnostic, so passing and warning
counts overlap. Missing planned files
become failing checks. Missing/ambiguous alternative inputs prevent file planning
and are execution errors, as are configuration-resolution and ordinary processing
failures. Empty scope or an unexpectedly empty applicable stage is an execution error.

Assessment-local errors are caught inside the assessment report; remaining
assessments continue. A child error does not populate the parent's `error`: the
parent outcome aggregates its children. Invalid shared YAML stops a project run;
a selected-assessment run records configuration errors inside that child so an
update can retain the failed attempt. Selection errors and other shared setup
failures stop the run and populate the parent error. Selected
names without child reports have no usable completed report and must not appear
as passes, including when report construction fails after some checks ran.

Argument-parser/dependency failures may retain native non-zero exits and leave no
report. Help exits 0 with no report. Abrupt termination has no guaranteed export.
A report records the validation outcome before export; a later publication error
is visible on stderr and exits 2 even if console validation succeeded.

## Input-file identity and validation time

Each child's `validation.inputs` contains:

| Field | Meaning |
| --- | --- |
| `algorithm` | `csv-md5-v1`. |
| `captured_at` | UTC time the initial input inventory was captured, or null if unavailable. |
| `stable` | True when before/after inventories match, false on detected change or failed recheck, null if no initial snapshot was obtained. |
| `files` | Initial inventory of `{path, hash, modified_at}` records; empty when no files or no snapshot. |

The inventory includes **all top-level files matching case-sensitive `.csv` in the
assessment's `inputs/` directory**, including present optional CSVs. This matches
`build_file_map()` and the inventory cross-file validation can load. Nested files,
non-CSV files, configuration files, logs, results, figures and report files are not
part of this algorithm. Hashing a file does not assert that every row passed a check.

Paths are relative to the assessment, use `/`, and start with `inputs/`. Names are
sorted in byte order (R radix ordering). `hash` is the lowercase 32-character MD5
of the exact file bytes. `modified_at` is the observed UTC modification time or
null if unavailable. MD5 here identifies ordinary content changes; it is not an
integrity/authentication guarantee against deliberate collisions.

For later **input-byte freshness**, compare the complete sorted `(path, hash)`
inventories. Detect additions/removals as well as changed hashes. Ignore modification
times for that comparison: touching or copying identical bytes does not change
input identity. The child's start/end timestamps show when validation was attempted;
only a successful child with stable inputs supplies successful validation evidence.

During validation, OpenVQA compares before/after names, hashes and observed file
modification times. Detected change or an unreadable recheck makes that child an
execution error and sets `stable` false, preserving returned checks. Other
assessments continue. Endpoint comparisons cannot detect all transient edits that
are reverted between observations; callers should validate immutable private copies
when stronger guarantees are needed. This does not lock or watch the source files.

A matching input inventory says nothing about changed settings, schemas, validator
code or runtime. The separate project settings identity below covers `params.yaml`
bytes only; resolved settings/code/environment fingerprints remain B3 work.

Each child also contains `validation.settings`:

| Field | Meaning |
| --- | --- |
| `path` | `params.yaml`, relative to the project root. |
| `algorithm` | `md5`. |
| `hash` | Lowercase MD5 of the exact project file bytes; null when the file was absent or the snapshot was unavailable. |
| `captured_at` | UTC time of the initial snapshot, or null if unavailable. |
| `stable` | True after matching before/after hashes (including matching absence), false after change/failed recheck, null without an initial snapshot. |

The project file is captured before resolving assessment overrides, even when
configuration resolution fails. Formatting/comments change its hash. A settings
change during validation makes that attempt an execution error without discarding
completed checks. This does not hash effective defaults, resolved overrides,
schemas, validator code or runtime. A null hash with a captured time and `stable: true`
means known absence, rather than unknown identity. Settings evidence is required
in every version-0.1 child; unavailable snapshots use explicit nulls.

## Dashboard composition and history

Use the run's selected assessment inventory as coverage and each child's grouped
checks, stage states, outcome and error for overview/detail views. A shared run error is also
relevant to assessment pages whose validation could not start.

`--update-report` converts the stored run into a `report_type: "collection"`.
A collection contains `report_version`, `operation`, `scope.data_root`, `updated_at`,
`validation.assessments` and `settings`. It has **no overall outcome, coverage,
run ID or start/end times**: these assessments did not run together. Each child's
`ended_at` records completion of its latest validation attempt, including a failed
attempt. Inspect its `outcome` to distinguish successful validation.

An update replaces the selected child, or appends it if missing. Untouched children
keep their original identity, backend, times, checks, outcomes and hashes. They are
not revalidated, rehashed, relabelled or removed based on current directories. A
failed new attempt replaces that assessment's prior attempt; this file stores the
latest attempt, not a full history or a separate last success. The update's exit
status describes the new invocation/publication, not retained assessment failures.

`settings.hashes_match` compares only stored stable project settings identities:
true for matching identities, false for different identities, null when consistency
is unknown (including attempts without a stable snapshot). A detected difference
takes precedence over missing identities. `settings.warning` supplies a small non-blocking
message for differing or unavailable identities; it does not alter check warning
counts or assessment outcomes. Normal runs also include this comparison.

Changing `params.yaml` and updating one assessment therefore leaves other passing
assessments passing, with their old hashes and completion times. The collection
warns if these stored hashes differ. **Matching stored hashes do not prove current
settings or inputs match.** A dashboard compares each child's settings hash and
complete input inventory to current files, and supplies the expected assessment
inventory. Missing assessments and stale evidence stay visible without rewriting
historical outcomes.

Updates require a matching normalized absolute project root, unique assessment
names and compatible schema-valid evidence. Relocating a project requires a fresh
full run/overwrite; absolute workspace paths are not durable project identifiers.
Earlier development reports must be regenerated with a whole-project `--overwrite`
or a fresh path; updates require a supported version-0.1 report.

## Publication and callable use

Exports are opt-in. Prefer a report file at the project root; an output subdirectory
inside the project needs an explicit discovery exclusion. Existing destinations
require `--overwrite` or the assessment-only `--update-report`. Directories and
symlinks are always refused. Overwrite requires a recognizable OpenVQA validation
report header, so accidental CSV/unrelated JSON destinations remain protected.
A wholly corrupt/unrecognizable file must be moved aside explicitly.

Completed objects are schema-validated before `run_assessment_validation()` and
`run_validation()` return, including console-only calls. The shared
`validated_run_report_json()` helper follows the existing parameter-validation
pattern (`jsonlite` plus `jsonvalidate`, AJV, strict/error mode). Internal draft
builders and check fragments are deliberately incomplete and are not public
completed reports.

`write_run_report()` and `update_assessment_report()` revalidate supplied objects
in memory before creating a file, because callers can edit previously valid results. It writes that validated
serialization rather than serializing the object again. An invalid object, missing
schema or schema-validator failure raises an error; CLI handling exits 2 and never
publishes a malformed report. If building a child fails its schema check, the project
can return a valid execution-error envelope identifying the reporting failure.

Complete UTF-8 JSON is written beside the destination. New files use checked
no-clobber hard-link publication. Replacement writers acquire a short-lived
`PATH.lock` file, then publish by checked same-directory rename; updates reload
the latest report under that lock before merging. This prevents cooperating
assessment writers from losing each other's updates. A concurrent lock causes
visible failure rather than waiting. A detected external edit before publication
also aborts; external tools must respect the lock to avoid races.

Publication failures preserve the previous report. Unsupported link/rename operations
fail visibly. Temporary files/owned locks are removed on normal/caught exits; forced
termination can leave them. Inspect a leftover lock and confirm its writer has ended
before manually removing it. No source inputs are locked or changed.
Wait for process exit, then check report existence, parse/schema/version/type/scope and
exit status. Never substitute a stale export after failure.

```r
validator <- new.env()
sys.source("validate.R", envir = validator)
result <- validator$run_validation("/work/project", assessment = "baseline")
child <- result$validation$assessments[[1]]
validator$write_run_report(child, "/work/reports/baseline-child-001.json")
# Requires an existing report for this project; returns a schema-valid collection.
stored <- validator$update_assessment_report(result, "/work/project/report.json")
```

`update_assessment_report()` returns the validated collection it saved. It requires
a one-assessment run result; plain `run_validation()` never reads or writes an
existing report. This keeps validation execution separate from persistence.

Source from the repository root. A private environment avoids entry-point
`main`/`parse_cli_args` name collisions when also sourcing `analyse.R`. Callable
validation prints progress and returns results without quitting R. `main(args)`
returns an integer exit status; direct execution and optparse's built-in help exit
handle process termination. Use `write_run_report()` for correct array/null handling.

Examples: [project success](../tests/testthat/fixtures/run-reports/success.json),
[invalid inputs](../tests/testthat/fixtures/run-reports/invalid.json),
[shared error](../tests/testthat/fixtures/run-reports/error.json),
[standalone assessment](../tests/testthat/fixtures/run-reports/assessment.json),
[mixed assessment outcomes](../tests/testthat/fixtures/run-reports/mixed.json),
[stored collection](../tests/testthat/fixtures/run-reports/collection.json).

## Verified implementation

Verified on 27 November 2026 against report version **0.1** and input-manifest
algorithm **csv-md5-v1**, using OpenVQA application version **0.1.0** at
the reviewed CLI/version revision following `daec0ac`. Runtime: R **4.5.2**,
jsonlite **2.0.0**, jsonvalidate **1.5.0** (AJV), Apple Silicon macOS.

The final standard suite passed **1,418 assertions** with no failures, warnings or
skips, including renamed CLI options and rejection of unsupported report versions.
All six strict scientific output contracts passed, including the in-process
isolation check in its designated shard. Disposable copies of both minimal demos
passed project creation, selected-assessment update and full overwrite; all exports
and extracted children passed the schema. Demo 1 retained 64 checks / 4 warnings;
demo 2 retained 124 checks / 9 warnings. Both succeed with exit 0, and no passing
check has error severity. A formatting-only settings edit changed the raw hash
and produced the collection consistency warning while preserving every untouched
child exactly. Original project files, including the user's version-1 report,
stayed unchanged.

Negative tests cover invalid severities, missing fields, unsupported versions,
required settings evidence, malformed/foreign reports, invalid flag combinations,
locked publication, external edits, failed replacement, and edited-object export
rejection. A simulated intervening assessment update verifies reload-before-merge.
Unsupported development report versions are rejected by update; explicit full
overwrite can regenerate a recognizable older report.

Changed R files passed formatting/lint. The full workflow selection retains one
unrelated pre-existing line-length failure at
`tests/testthat/test-distribution-input-audit.R:1` (102 characters; limit 100).
Cross-platform execution was not repeated locally.

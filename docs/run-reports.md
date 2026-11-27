# Validation run reports

A validation run contains one independently usable report per assessment. The same
JSON Schema accepts a whole run or an extracted assessment report. This lets a
project overview and assessment detail pages use the same evidence.

`pipeline/run-report.R` owns the common envelope and safe publication. `validate.R`
owns execution and aggregation; `validation/snapshot.R` records input-file identity.
`reporting/` continues to generate scientific summary tables and workbooks.
Analysis reporting is deferred to B2.

## Invocation and selection

Run from the OpenVQA repository root, keeping `--data-root` at the project level:

```sh
# Validate every included assessment.
Rscript validate.R --data-root /work/project --report /work/reports/project-001.json

# Validate one assessment using its project settings and assessment overrides.
Rscript validate.R --data-root /work/project --assessment baseline --report /work/reports/baseline-001.json
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

The destination's parent directory must already exist, and its filename must be
fresh. `--report PATH` and `--report=PATH` work. Without `--report`, the same checks,
per-assessment summaries and exit rules apply to console-only execution.

## Report structure

Report version **1.0** is independent of the application version in `VERSION`.
This is an unreleased revision of the earlier flat B1 draft: rows/stages now live
inside assessment reports. There is no published flat-format compatibility promise.

[The schema](../schema/run-report.schema.json) uses draft-07 and local definitions
only. Its root accepts either `runReport` or `assessmentReport`; `assessmentReport`
is the reusable definition used inside a run's array. Both shapes can be validated
directly with the installed `jsonvalidate` AJV engine, without resolving external files.

```text
run envelope (report_type = "run")
  scope: project or explicitly selected assessment
  error: shared setup error, or null
  validation.assessments[]
    assessment envelope (report_type = "assessment")
      scope: exactly one assessment
      error: this assessment's execution error, or null
      validation.rows[]
      validation.stages[]
      validation.inputs: versioned input-file manifest
```

A one-assessment invocation still exports a run containing one child. Each child
has its own run ID, backend metadata, start/end times and outcome. Extracting it
requires no reconstruction or borrowed parent fields. Children are ordered like
the selected assessment inventory; rows retain scientific execution order.

Both envelopes require:

| Field | Meaning |
| --- | --- |
| `report_version` | `major.minor`; currently `1.0`. |
| `report_type` | `run` or `assessment`. |
| `operation` | `validate`; analysis is not yet supported. |
| `run_id` | Opaque execution identity, not an input fingerprint. |
| `scope.type` | `project` or `assessment`; children always use `assessment`. |
| `scope.data_root` | Absolute normalized project path once resolved; requested path or null on early setup failure. |
| `scope.requested_assessment` | Requested name, or null for whole-project runs. |
| `scope.assessments` | Ordered selected assessment names, including names not reached after shared setup failure. |
| `scope.excluded_dirs` | Requested exclusions for a run; empty for a child. |
| `backend.version` | Application version, or null when unavailable. |
| `backend.revision` | Git HEAD when the application root has `.git` and Git works; otherwise null. Not proof of clean source. |
| `started_at`, `ended_at` | UTC timestamps with milliseconds and `Z`; validation times exclude subsequent publication. |
| `outcome` | `success`, `invalid_data` or `execution_error`. |
| `error` | Null, or `{message, assessment, stage}`. Shared run errors use null assessment. |

Required unavailable scalars are explicit JSON `null`. Collections are arrays,
including empty and singleton collections. Unknown optional fields may be added
in a minor report version; readers must ignore unknown fields. Removing/renaming
fields, changing types or changing meanings requires a new major version. Readers
must check supported major version, operation and report type.

## Checks, stages and outcomes

Each child's `validation.rows` uses the existing fields: `assessment`, `stage`,
`check_id`, `severity`, `status`, `message`, `file`, `details`, `extra_columns`.
The last three may be null. Extra columns retain their existing comma-separated
representation. Messages/details are human-readable, not a parsing interface.
Scientific check IDs, rules and severity/status meanings are unchanged.

Stage records contain `assessment`, `stage` (`schema` or `crossfile`), `status`,
and `reason` (null only for completed stages):

| Status | Meaning |
| --- | --- |
| `completed` | Returned a non-empty collection of checks; some may have failed. |
| `skipped` | Cross-file checks were blocked by schema failure. |
| `not_applicable` | Cross-file checks do not apply to an assumed-zero assessment. |
| `not_run` | Processing stopped before this stage. |
| `error` | This stage started but did not finish; earlier fragments may exist. |

Completed fragments are retained after each schema file and each cross-file
validation function returns. A function that throws does not emit fabricated checks
or expose its unfinished internal rows. Existing passing warning rows that describe
unavailable optional comparisons retain their historical status; stage completion
does not assert that every optional comparison occurred.

| Outcome | CLI exit | Meaning |
| --- | --- | --- |
| `success` | 0 | All selected assessment reports succeeded and requested export succeeded. |
| `invalid_data` | 1 | At least one child has failing checks and none has an execution error. |
| `execution_error` | 2 | Shared setup failed or any assessment has an execution error. |
| No report guaranteed | 2 | Export failed, including invalid/existing destinations. |

Within an assessment, non-passing check status blocks success independently of
severity. Severity alone never fails validation: existing passing warning rows
remain non-blocking. Passing and warning counts can overlap. Missing planned files
become failing checks. Missing/ambiguous alternative inputs prevent file planning
and are execution errors, as are configuration-resolution and ordinary processing
failures. Empty scope or an unexpectedly empty applicable stage is an execution error.

Assessment-local errors are caught inside the assessment report; remaining
assessments continue. A child error does not populate the parent's `error`: the
parent outcome aggregates its children. Invalid shared YAML, selection errors and
other shared setup failures stop the run and populate the parent error. Selected
names without children were not run; they must not appear as passes.

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

A matching input inventory says nothing about changed **settings, schemas,
validator code or runtime**. Those identities remain B3 work. Future fields can
record them alongside this manifest without changing what `csv-md5-v1` means.

## Dashboard composition and history

Use the run's selected assessment inventory as coverage and each child's rows,
stages, outcome and error for overview/detail views. A shared run error is also
relevant to assessment pages whose validation could not start.

A dashboard can retain independently valid children from project runs or
single-assessment runs, then select the latest attempt for each logical assessment.
Keep each child's run ID, original times, scope and backend metadata; do not
relabel historical reports as one new execution. A derived overview can reference
children from several invocations without changing those source reports.

The dashboard owns project/assessment identity, expected assessment inventory,
history and persistence. Absolute workspace paths alone are not durable project
identifiers. Missing reports, unvalidated assessments, changed input inventories
and unavailable settings identity must remain visible; a subset of successful
children does not prove the whole current project valid. A failed latest attempt
must not silently fall back to an earlier successful one.

## Publication and callable use

Exports are opt-in and never automatic project-side cache files. Use an output
directory outside the project, or explicitly exclude it from discovery. A destination
that already exists, including a directory or symlink, is rejected before validation
and left unchanged; it is not this invocation's report. Allocate a new path per attempt.

Complete UTF-8 JSON is written beside the destination and published by checked hard
link, atomically creating the name without overwriting a concurrent writer. Hard-link
support is required; unsupported filesystems fail visibly. Temporary files are removed
on normal/caught exits; forced termination may leave an unpublished temporary file.
Wait for process exit, then check report existence, parse/schema/version/type/scope and
exit status. Never substitute a stale export after failure.

```r
validator <- new.env()
sys.source("validate.R", envir = validator)
result <- validator$run_validation("/work/project", assessment = "baseline")
child <- result$validation$assessments[[1]]
validator$write_run_report(child, "/work/reports/baseline-child-001.json")
```

Source from the repository root. A private environment avoids entry-point
`main`/`parse_cli_args` name collisions when also sourcing `analyse.R`. Callable
validation prints progress and returns results without quitting R. `main(args)`
returns an integer exit status; direct execution and optparse's built-in help exit
handle process termination. Use `write_run_report()` for correct array/null handling.

Examples: [project success](../tests/testthat/fixtures/run-reports/success.json),
[invalid inputs](../tests/testthat/fixtures/run-reports/invalid.json),
[shared error](../tests/testthat/fixtures/run-reports/error.json),
[standalone assessment](../tests/testthat/fixtures/run-reports/assessment.json),
[mixed assessment outcomes](../tests/testthat/fixtures/run-reports/mixed.json).

## Verified implementation

Verified on 27 November 2026 using OpenVQA **0.1.0**, base revision
`06d2caada4b0259cf74232e7f9a9669fd4860d5c` plus the uncommitted B1 changes, report
version **1.0** and input-manifest algorithm **csv-md5-v1**. Runtime: R **4.5.2**,
Apple Silicon macOS, jsonlite **2.0.0**, jsonvalidate **1.5.0** (AJV).

Verified `Rscript validate.R --data-root PATH [--assessment NAME] [--report PATH]`,
including aliases, console-only execution, help, failure exits and publication
boundaries. Both minimal projects exported valid project and single-assessment
reports from disposable copies, and every extracted child independently passed
the same schema. Their 64 and 124 check rows exactly matched the preceding flat
B1 workflow. Original files stayed unchanged.

The standard suite passed 1,299 assertions and all six strict scientific output
contracts passed, including numerical/workbook/decoded-figure checks and seeded
repeatability. This retains report version 1.0 because the earlier B1 draft was
unreleased. Cross-platform execution was not repeated locally. See
`refactor/reporting.md` for the acceptance record and remaining full-lint gap.

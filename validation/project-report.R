# Collections retain independent assessment evidence without inventing a project-wide verdict.
read_validation_collection <- function(path, data_root) {
  if (!file.exists(path)) stop("--update-report requires an existing report.", call. = FALSE)
  report <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  validated_run_report_json(report)
  if (!report$report_type %in% c("run", "collection")) {
    stop("Assessment updates require a run or collection report.", call. = FALSE)
  }
  root <- normalizePath(data_root, winslash = "/", mustWork = TRUE)
  if (!identical(report$scope$data_root, root)) {
    stop("Existing report belongs to a different project location.", call. = FALSE)
  }
  children <- report$validation$assessments
  assessment_names <- vapply(children, function(child) child$scope$assessment, character(1))
  if (anyDuplicated(assessment_names) || any(vapply(children, function(child) {
    !identical(child$scope$data_root, root)
  }, logical(1)))) {
    stop("Existing report has duplicate assessments or conflicting project locations.",
      call. = FALSE
    )
  }
  report
}

update_assessment_report <- function(result, path) {
  validated_run_report_json(result)
  children <- result$validation$assessments
  if (result$report_type != "run" || result$scope$type != "assessment" || length(children) != 1L) {
    stop("Report updates require exactly one completed assessment report.", call. = FALSE)
  }
  path <- prepare_report_destination(path, overwrite = TRUE)
  with_report_lock(path, {
    # Reload under the lock so another assessment update cannot be lost during a long validation.
    existing <- read_validation_collection(path, result$scope$data_root)
    stored <- existing$validation$assessments
    name <- children[[1]]$scope$assessment
    position <- match(name, vapply(stored, function(child) child$scope$assessment, character(1)))
    if (is.na(position)) position <- length(stored) + 1L
    stored[[position]] <- children[[1]]
    collection <- list(
      report_version = result$report_version, report_type = "collection", operation = "validate",
      scope = list(data_root = result$scope$data_root), updated_at = run_report_time(),
      validation = list(assessments = stored), settings = validation_settings_summary(stored)
    )
    # Retained outcomes/timestamps stay as recorded, even when params.yaml hashes differ.
    json <- validated_run_report_json(collection)
    .publish_run_report_json(json, path, overwrite = TRUE)
    collection
  })
}

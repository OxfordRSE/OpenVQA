# Hash the same top-level CSV inventory the validator can load, including present optional files.
snapshot_validation_inputs <- function(inputs_dir) {
  if (dir.exists(inputs_dir) && file.access(inputs_dir, 5L) != 0L) {
    stop("Cannot read assessment input directory: ", inputs_dir, call. = FALSE)
  }
  files <- build_file_map(inputs_dir)
  files <- files[order(names(files), method = "radix")]
  hashes <- unname(tools::md5sum(files))
  if (anyNA(hashes)) stop("Cannot hash assessment input files.", call. = FALSE)
  list(
    captured_at = run_report_time(),
    files = data.frame(
      path = file.path("inputs", names(files)), hash = hashes,
      modified_at = run_report_time(file.info(files)$mtime), stringsAsFactors = FALSE
    )
  )
}

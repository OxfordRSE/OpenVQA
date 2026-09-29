#!/usr/bin/env Rscript

contracts <- c(
  "vqa-demo1/main_001_current",
  "vqa-demo1/offset_001_current",
  "vqa-demo2/offset_baseline",
  "vqa-demo2/offset_current",
  "vqa-demo2/project_baseline",
  "vqa-demo2/project_current"
)
selected_contract <- Sys.getenv("VQA_TEST_CONTRACT")

detect_test_workers <- function(contract_count) {
  requested <- Sys.getenv("VQA_TEST_WORKERS")
  if (nzchar(requested)) {
    if (!grepl("^[1-9][0-9]*$", requested)) {
      stop("VQA_TEST_WORKERS must be a positive integer.")
    }
    requested <- suppressWarnings(as.integer(requested))
    if (is.na(requested)) {
      stop("VQA_TEST_WORKERS is too large.")
    }
    return(min(requested, contract_count))
  }

  available <- suppressWarnings(as.integer(Sys.getenv("NUMBER_OF_PROCESSORS")))
  if (length(available) != 1L || is.na(available)) {
    available <- parallel::detectCores()
  }
  if ((length(available) != 1L || is.na(available)) &&
      nzchar(Sys.which("getconf"))) {
    available <- suppressWarnings(as.integer(system2(
      "getconf", "_NPROCESSORS_ONLN", stdout = TRUE, stderr = FALSE
    )))
  }
  if (length(available) != 1L || is.na(available) || available < 1L) {
    available <- 2L
  }

  min(max(1L, floor(available / 2)), contract_count)
}

run_contract <- function(contract, script, rscript) {
  output <- suppressWarnings(system2(
    rscript,
    shQuote(script),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("VQA_TEST_CONTRACT=", contract)
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(contract = contract, status = status, output = output)
}

run_parallel_tests <- function(workers) {
  message(
    "Running ", length(contracts), " refactor contracts with ",
    workers, " workers."
  )
  script <- normalizePath("tests/refactor/testthat.R")
  rscript <- file.path(R.home("bin"), "Rscript")
  if (.Platform$OS.type == "windows") {
    cluster <- parallel::makeCluster(workers)
    results <- tryCatch(
      parallel::parLapply(
        cluster, contracts, run_contract,
        script = script,
        rscript = rscript
      ),
      finally = parallel::stopCluster(cluster)
    )
  } else {
    results <- parallel::mclapply(
      contracts, run_contract,
      script = script,
      rscript = rscript,
      mc.cores = workers
    )
  }

  for (result in results) {
    cat("\n== ", result$contract, " ==\n", sep = "")
    cat(paste(result$output, collapse = "\n"), "\n", sep = "")
  }
  quit(status = as.integer(any(vapply(
    results, function(result) result$status != 0L, logical(1L)
  ))))
}

if (!nzchar(selected_contract)) {
  workers <- detect_test_workers(length(contracts))
  if (workers > 1L) run_parallel_tests(workers)
}

reporter <- if (nzchar(selected_contract)) "summary" else "progress"
testthat::test_dir("tests/refactor/testthat", reporter = reporter)

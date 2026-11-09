# Production functions are already instrumented by covr::file_coverage().
testthat::test_dir("tests/testthat", reporter = "summary", env = environment())

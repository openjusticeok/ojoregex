skip_if_no_test_data <- function() {
  if (!file.exists(testthat::test_path("test_data.rda"))) {
    testthat::skip("Test data not available")
  } else {
    invisible()
  }
}

test_that("Applying regex works the same", {
  skip_if_no_test_data()

  load(test_path("test_data.rda"))

  categorized_test_data <- ojo_apply_regex(
    test_data,
    col_to_clean = "description",
    .quiet = TRUE
  )

  temp_path <- tempfile(fileext = "csv")

  readr::write_csv(categorized_test_data, temp_path)
  
  expect_snapshot_file(temp_path, "categorized_test_data.csv")
})

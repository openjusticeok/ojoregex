test_that("Pre-cleaning works the same", {
  skip_if_no_test_data()

  load(test_path("test_data.rda"))

  clean_test_data <- test_data |>
    dplyr::mutate(
      clean_description = regex_pre_clean(description)
    )

  temp_path <- tempfile(fileext = "csv")

  readr::write_csv(clean_test_data, temp_path)

  expect_snapshot_file(temp_path, "clean_test_data.csv")
})

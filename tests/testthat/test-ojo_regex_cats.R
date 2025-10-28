test_that("regex cats haven't changed", {
  temp_path <- tempfile(fileext = "csv")

  readr::write_csv(ojo_regex_cats, temp_path)

  expect_snapshot_file(temp_path, "ojo_regex_cats.csv")
})

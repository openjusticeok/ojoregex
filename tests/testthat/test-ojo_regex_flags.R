test_that("regex flags haven't changed", {
  temp_path <- tempfile(fileext = "csv")

  readr::write_csv(ojo_regex_flags, temp_path)

  expect_snapshot_file(temp_path, "ojo_regex_flags.csv")
})

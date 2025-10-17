test_that("regex flags haven't changed", {
  expect_snapshot(ojoregex::ojo_regex_flags)
})

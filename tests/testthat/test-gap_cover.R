gap_tall <- demo_data("gap")

test_that("gap_cover errors when gap_tall is not a data frame", {
  expect_error(gap_cover(gap_tall = "not a data frame"))
})

test_that("gap_cover returns a list of data frames when tall = FALSE", {
  result <- gap_cover(gap_tall, tall = FALSE)
  expect_type(result, "list")
  expect_true(all(vapply(result, is.data.frame, logical(1))))
})

test_that("gap_cover returns a single tall data frame when tall = TRUE", {
  result <- gap_cover(gap_tall, tall = TRUE)
  expect_s3_class(result, "data.frame")
  expect_true("PrimaryKey" %in% names(result))
})

test_that("gap_cover by_line = TRUE includes LineKey in tall output", {
  result <- gap_cover(gap_tall, tall = TRUE, by_line = TRUE)
  expect_true("LineKey" %in% names(result))
})

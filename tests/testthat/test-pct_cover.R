lpi_tall <- demo_data("lpi")

test_that("pct_cover returns a data frame with PrimaryKey", {
  result <- pct_cover(lpi_tall, tall = FALSE, hit = "any", by_line = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true("PrimaryKey" %in% names(result))
})

test_that("pct_cover errors when lpi_tall is not a data frame", {
  expect_error(pct_cover(lpi_tall = "not a data frame"))
})

test_that("pct_cover errors on invalid hit value", {
  expect_error(pct_cover(lpi_tall, hit = "nonsense"))
})

test_that("pct_cover with by_line = TRUE adds LineKey to output", {
  result <- pct_cover(lpi_tall, tall = FALSE, by_line = TRUE)
  expect_true("LineKey" %in% names(result))
})

test_that("pct_cover tall = TRUE produces a long-format data frame", {
  wide <- pct_cover(lpi_tall, tall = FALSE, indicator_variables = "code")
  long <- pct_cover(lpi_tall, tall = TRUE, indicator_variables = "code")
  expect_s3_class(long, "data.frame")
  # Long format has one row per PrimaryKey/indicator combination, so with more
  # than one indicator type it should have more rows than the wide format.
  expect_gt(nrow(long), nrow(wide))
})

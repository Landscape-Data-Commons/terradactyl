height_tall <- demo_data("height")

test_that("mean_height returns a data frame with PrimaryKey", {
  result <- mean_height(height_tall, method = "mean", tall = FALSE,
                        indicator_variables = "type")
  expect_s3_class(result, "data.frame")
  expect_true("PrimaryKey" %in% names(result))
})

test_that("mean_height tall = TRUE produces a long-format data frame", {
  result <- mean_height(height_tall, tall = TRUE, indicator_variables = "type")
  expect_s3_class(result, "data.frame")
})

# KNOWN BUG (see R/height.R ~line 200): the tall = FALSE / method = "max"
# branch calls tidyr::pivot_longer() with pivot_wider()-style arguments
# (names_from/values_from/values_fill) and assumes a "Species" grouping
# column is always present. As written it currently errors for any call
# with tall = FALSE, so there is no working behavior to test yet. This is
# flagged in the CRAN prep task list; add a real test here once
# R/height.R is fixed.

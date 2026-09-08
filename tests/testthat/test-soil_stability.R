soil_stability_tall <- demo_data("soil_stability")

test_that("soil_stability returns a data frame with PrimaryKey", {
  result <- soil_stability(soil_stability_tall)
  expect_s3_class(result, "data.frame")
  expect_true("PrimaryKey" %in% names(result))
})

test_that("soil_stability respects the cover argument", {
  no_cover <- soil_stability(soil_stability_tall, cover = FALSE)
  with_cover <- soil_stability(soil_stability_tall, cover = TRUE)
  expect_s3_class(no_cover, "data.frame")
  expect_s3_class(with_cover, "data.frame")
})

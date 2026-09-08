test_that("demo_data returns a data frame for a valid type", {
  result <- demo_data("lpi")
  expect_s3_class(result, "data.frame")
})

test_that("demo_data errors for an unrecognized type", {
  expect_error(demo_data("not_a_real_type"))
})

test_that("demo_data errors when type is not a single character string", {
  expect_error(demo_data(type = 1))
  expect_error(demo_data(type = c("lpi", "gap")))
})

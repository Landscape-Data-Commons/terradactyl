# Placeholder: species.R functions require confirmation of exact exported
# function names/signatures (e.g. species_count(), species_join()) before
# targeted tests can be written. See the CRAN prep task list for follow-up.

test_that("species data loads for use in future species tests", {
  species_list <- demo_data("species_list")
  expect_s3_class(species_list, "data.frame")
})

# This file previously built a `testing_data` list from AIM/LMF/NRI sample
# tables (tblLines, tblPlots, etc.) that were never defined or loaded
# anywhere in the package or its data files, so every test run failed before
# any test_that() block could execute.
#
# It has been emptied out pending real fixtures. If/when sample AIM
# geodatabase or LMF/NRI text-file extracts are added under
# tests/testthat/fixtures/ (or inst/extdata/), rebuild this helper to load
# them here so individual test files can reference `testing_data`.

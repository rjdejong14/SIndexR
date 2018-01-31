test_that("SIndexR_VersionNumber.R: version number is not correct.", {
  library(data.table)
  library(testthat)
  expect_is(SIndexR_VersionNumber(), "integer")
  expect_equal(SIndexR_VersionNumber(), 152L)
})

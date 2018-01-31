test_that("SIndexR_FirstSpecies.R: first species index is not correct.", {
  library(data.table)
  library(testthat)
  expect_is(SIndexR_FirstSpecies(), "integer")
  expect_equal(SIndexR_FirstSpecies(), 0L)
})

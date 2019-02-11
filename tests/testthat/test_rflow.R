context("basic operations on Rflow")

test_that("Rflow can be crated", {
  rf1 <- Rflow::new_rflow()
  expect_true(exists("rf1"), "rflow object exists")
  expect_s3_class(rf1, "rflow")
  expect_s3_class(rf1, "environment")
})

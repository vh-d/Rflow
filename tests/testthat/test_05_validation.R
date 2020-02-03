
context("Validators")

test_that("Validators can be constructed", {
  v1 <- validator(function(x) x > 1)
  expect_s3_class(v1, "validator")
  expect_s3_class(v1, "validator_r")
  expect_identical(v1, validator(function(x) x > 1))

  v2 <- validator_r(expression_r(TRUE))
  expect_s3_class(v1, "validator")
  expect_s3_class(v1, "validator_r")
  expect_identical(v2, validator(expression_r(TRUE)))
})


test_that("Validators can be evaluated", {
  v1 <- validator_r(function(x) x > 1)

  expect_true(evaluate(v1, 2))
  expect_false(evaluate(v1, 1))
  expect_false(evaluate(v1, NA))
  expect_false(evaluate(v1, NULL))

  v2 <- validator_r(expression_r(TRUE))
  expect_true(evaluate(v2))
})

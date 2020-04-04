context("dbi")

test_that("dbi objects can be constructed", {
  d1 <- dbi(RSQLite::SQLite(), dbname = ":memory:")
  expect_s3_class(d1, "dbi")
})


test_that("dbi objects can be connected, validated and disconnected", {
  d1 <- dbi(RSQLite::SQLite(), dbname = ":memory:")
  connect(d1)
  expect_true(isValid(d1))
  disconnect(d1)
  expect_false(isValid(d1))
})

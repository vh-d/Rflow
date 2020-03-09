context("Creating jobs")

test_that("R jobs can be created", {
  rjob <- job_r({1+1})
  expect_s3_class(rjob, "job")
  expect_s3_class(rjob, "job_r")
  expect_identical(evaluate(rjob), 2)

  rjob <- job_r(function() 1+1)
  expect_s3_class(rjob, "job")
  expect_s3_class(rjob, "job_r")
  expect_identical(evaluate(rjob), 2)
})

context("Detecting dependencies")

test_that("node names are identified in R expressions", {
  rexpr1 <- expression({.RFLOW[["ENV.node1"]]})
  expect_identical("ENV.node1", detect_deps(rexpr1, "ENV.node1"))

  rexpr2 <- parse(text = '#.RFLOW[["ENV.node1"]]')
  expect_identical(character(), detect_deps(rexpr2, "ENV.node1"))
})


context("Detecting dependencies")

test_that("node names are identified in R expressions", {
  rexpr1 <- expression({.RFLOW[["ENV.node1"]]})
  expect_identical("ENV.node1", detect_deps(rexpr1, "ENV.node1"))

  rexpr2 <- parse(text = '#.RFLOW[["ENV.node1"]]')
  expect_identical(character(), detect_deps(rexpr2, "ENV.node1"))
})

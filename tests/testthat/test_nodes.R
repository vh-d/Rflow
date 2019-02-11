context("Basic operations on nodes")

test_that("nodes can be initialized", {
  expect_error(Rflow::node$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::node$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::node$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")

  expect_error(node1 <- Rflow::node$new(id = "node1"), regexp = "(folder)|(path)|(dir)", info = "either store = FALSE, or path has to be supplied")

  node2 <- Rflow::node$new(id = "node2", store = FALSE)
  expect_true(exists("node2"), info = "Nodes can be created")
  expect_s3_class(object = node2, class = "node")
  expect_s3_class(object = node2, class = "R6")

  tmp_rflow_dir <- tempdir()
  node3 <- Rflow::node$new(id = "node3", storage = tmp_rflow_dir)

})

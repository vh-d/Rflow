context("Basic operations on nodes")

test_that("generic nodes", {
  expect_error(Rflow::node$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::node$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::node$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")

  expect_error(node1 <- Rflow::node$new(id = "node1"), regexp = "(folder)|(path)|(dir)", info = "path has to be supplied")

  tmp_rflow_dir <- tempdir()
  node2 <- Rflow::node$new(id = "node2", storage = tmp_rflow_dir)
  expect_s3_class(object = node2, class = "node")
  expect_s3_class(object = node2, class = "R6")

})


test_that("r nodes", {
  expect_error(Rflow::r_node$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::r_node$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::r_node$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")

  expect_error(Rflow::r_node$new(id = "rnode1"), regexp = "(folder)|(path)|(dir)", info = "path has to be supplied")

  tmp_rflow_dir <- tempdir()
  expect_error(Rflow::r_node$new(id = "rnode1", storage = tmp_rflow_dir), regexp = "(folder)|(path)|(dir)", info = "either caching = FALSE or path has to be supplied")
  expect_warning(rnode2 <- Rflow::r_node$new(id = "rnode2", storage = tmp_rflow_dir, caching = FALSE), regexp = "R expression", "initialization without r_code or r_expression should produce warning")

  expect(exists("rnode2"))
  expect_s3_class(object = rnode2, class = "r_node")
  expect_s3_class(object = rnode2, class = "node")
  expect_s3_class(object = rnode2, class = "R6")

})

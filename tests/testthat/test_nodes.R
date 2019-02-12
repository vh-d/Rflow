
# node --------------------------------------------------------------------

context("Creating generic node object")

test_that("nodes can be initiated", {
  expect_error(Rflow::node$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::node$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::node$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")

  node1 <- Rflow::node$new(id = "node1")
  expect_s3_class(object = node1, class = "node")
  expect_s3_class(object = node1, class = "R6")

  expect_is(node1$persistence, "list")
  expect_false(node1$persistence$enabled)

  expect_null(node1$depends)
  expect_null(node1$trigger_condition)
  expect_null(node1$last_updated)

  node12 <- Rflow::node$new(name = "node12", env = "env")
  expect_equal(node12$id, "env.node12", info = "Nodes can be initialize with name/env pair instead of id")
})

test_that("nodes can be initiated with persistence", {
  tmp_rflow_dir <- tempdir()
  node2 <- Rflow::node$new(id = "node2", persistence = list(path = tmp_rflow_dir), caching = FALSE)

  expect_is(node2$persistence, "list")
  expect_true(node2$persistence$enabled)

})



# r_node ------------------------------------------------------------------


context("Creating r_node objects")

test_that("nodes can be initiated", {
  expect_error(Rflow::r_node$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::r_node$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::r_node$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")

  node1 <- Rflow::r_node$new(id = "node1")
  expect_s3_class(object = node1, class = "r_node")
  expect_s3_class(object = node1, class = "node")
  expect_s3_class(object = node1, class = "R6")

  expect_is(node1$persistence, "list")
  expect_false(node1$persistence$enabled)

  expect_null(node1$depends)
  expect_null(node1$trigger_condition)
  expect_true(is.null(node1$last_updated) || is.na(node1$last_updated))
})

test_that("nodes can be initiated with persistence", {
  tmp_rflow_dir <- tempdir()
  node2 <- Rflow::r_node$new(id = "node2", persistence = list(path = tmp_rflow_dir), caching = FALSE)
  expect_is(node2$persistence, "list")
  expect_true(node2$persistence$enabled)
})

test_that("nodes can be initiated with caching", {
  tmp_rflow_dir <- tempdir()
  expect_error(Rflow::r_node$new(id = "node3", caching = TRUE), "caching")
  node3 <- Rflow::r_node$new(id = "node3", caching = TRUE, cache_store = tmp_rflow_dir)
  node4 <- Rflow::r_node$new(id = "node3", caching = TRUE, cache_store = tmp_rflow_dir)
})


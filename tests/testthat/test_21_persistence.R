
context("Testing persistance")

test_that("nodes can be initiated with persistence", {
  tmp_rflow_dir <- tempdir()
  node2 <- Rflow::node$new(id = "node2", persistence = list(path = tmp_rflow_dir))

  expect_is(node2$persistence, "list")
  expect_true(node2$persistence$enabled)

})


test_that("r_node can be initiated with persistence", {
  tmp_rflow_dir <- tempdir()
  node2 <- Rflow::r_node$new(id = "node2", r_expr = expression_r(1), persistence = list(path = tmp_rflow_dir))
  expect_is(node2$persistence, "list")
  expect_true(node2$persistence$enabled)
  expect_equal(node2$persistence$path, tmp_rflow_dir)
  expect_true(file.exists(file.path(node2$persistence$path, node2$persistence$file)))
})

test_that("r_node can be initiated with caching", {
  tmp_rflow_dir <- tempdir()
  node3 <- Rflow::r_node$new(id = "node3", r_expr = expression_r(1), cache = list(path = tmp_rflow_dir), verbose = FALSE)
  expect_true(node3$cache$enabled)
  expect_equal(node3$cache$path, tmp_rflow_dir)
  expect_error(node3$cache_write(), info = "cache cannot be written without existing value")
})



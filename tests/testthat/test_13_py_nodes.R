context("py_nodes")

if (!requireNamespace("reticulate")) skip("reticulate package is required for testing Python nodes")
if (!reticulate::py_available(initialize = TRUE)) skip("python runtime is required for testing Python nodes")


test_that("py_nodes can be constructed", {
  pynode1 <- Rflow::py_node$new(env = "PY", name = "pyvalue1")
  expect_s3_class(pynode1, "py_node")
  expect_s3_class(pynode1, "node")
})


test_that("py_nodes' existence can be verified/detected", {
  pynode1 <- Rflow::py_node$new(env = "PY", name = "pyvalue1")
  expect_false(pynode1$exists())
  reticulate::py_run_string("pyvalue1 = 1")
  expect_true(pynode1$exists())
})


test_that("py_nodes can be fetched", {

  pynode1 <- Rflow::py_node$new(env = "PY", name = "pyvalue1")

  reticulate::py_run_string("pyvalue1 = 1")
  expect_identical(pynode1$get(), 1L)

  reticulate::py_run_string("pyvalue1 = [x for x in range(1, 5)]")
  expect_identical(pynode1$get(), 1L:4L)
})


test_that("py_nodes can be cached and restored from cache", {
  pynode1 <- Rflow::py_node$new(env = "PY", name = "pyvalue1")
  reticulate::py_run_string("pyvalue1 = [x for x in range(1, 5)]")
  pynode1$cache_setup(tempdir())
  pynode1$cache_write()
  expect_true(pynode1$remove(verbose = FALSE))
  expect_false(pynode1$exists())
  pynode1$cache_restore()
  expect_true(pynode1$exists())
  expect_identical(pynode1$get(), 1L:4L)
})

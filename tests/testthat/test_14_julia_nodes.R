context("julia_nodes")

if (!requireNamespace("JuliaCall")) skip("JuliaCall package is required for testing Julia nodes")
if (!tryCatch({julia <- JuliaCall::julia_setup();TRUE}, error = function(e) FALSE)) skip("Julia runtime is required for testing Julia nodes")


test_that("julia_nodes can be constructed", {
  node1 <- Rflow::julia_node$new(env = "JULIA", name = "juvalue1")
  expect_s3_class(node1, "julia_node")
  expect_s3_class(node1, "node")
})


test_that("julia_nodes' existence can be verified/detected", {
  node1 <- Rflow::julia_node$new(env = "JULIA", name = "juvalue1")
  expect_false(node1$exists())
  JuliaCall::julia_command("juvalue1 = 1", show_value = FALSE)
  expect_true(node1$exists())
})


test_that("julia_nodes can be fetched", {
  node1 <- Rflow::julia_node$new(env = "JULIA", name = "juvalue1")
  JuliaCall::julia_command("juvalue1 = 1", show_value = FALSE)
  expect_identical(node1$get(), 1L)
})


test_that("julia_nodes can be cached and restored from cache", {
  node1 <- Rflow::julia_node$new(env = "julia", name = "value1")
  JuliaCall::julia_command("value1 = 1:10", show_value = FALSE)
  node1$cache_setup(tempdir())
  node1$cache_write()
  # expect_true(node1$remove(verbose = FALSE))
  # expect_false(node1$exists())
  node1$cache_restore()
  expect_true(node1$exists())
  expect_identical(node1$get(), 1:10)
})

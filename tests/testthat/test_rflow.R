context("Builidng rflows")

test_that("Rflow can be crated", {
  rf1 <- Rflow::new_rflow()
  expect_true(exists("rf1"), "rflow object exists")
  expect_s3_class(rf1, "rflow")
  expect_s3_class(rf1, "environment")

})

test_that("nodes can be added", {
  rf2 <- Rflow::new_rflow()
  expect_true(Rflow::add_node(list(id = "node1"), rflow = rf2, verbose = FALSE))
  expect_is(rf2[["node1"]], "node")

  expect_true(Rflow::add_node(list(id = "node2", depends = "node1"), rflow = rf2, verbose = FALSE))
  expect_is(rf2[["node2"]], "node")
  expect_length(rf2$node2$depends, 1)
  expect_null(rf2$node2$upstream)
})


test_that("nodes can be connected", {
  rf2 <- Rflow::new_rflow()
  expect_true(Rflow::add_node(list(id = "node1"), rflow = rf2, verbose = FALSE))
  expect_true(Rflow::add_node(list(id = "node2", depends = "node1"), rflow = rf2, verbose = FALSE))

  # connecting nodes
  connect_nodes(rf2, verbose = FALSE)

  expect_length(rf2$node2$upstream,   1)
  expect_length(rf2$node1$downstream, 1)

  expect_equal(rf2$node2$upstream[[1]],   rf2$node1)
  expect_equal(rf2$node1$downstream[[1]], rf2$node2)
})

test_that("node definitions are processed correctly", {
  expect_equal("FOO", Rflow:::env_name_from_id("FOO.bar")[, env[1]])
  expect_equal("bar", Rflow:::env_name_from_id("FOO.bar")[, name[1]])
  
  obj <- list(
    env  = "FOO",
    name = "bar"
  )
  
  obj_process <- process_obj_defs(list(obj))
  expect_equal(obj_process, list("FOO.bar" = obj))
  
  obj2 <- list(
    id = "FOO.bar"
  )
  obj_process2 <- process_obj_defs(list(obj2))
  expect_equal(obj_process2, list("FOO.bar" = c(list(id = "FOO.bar"), obj)))
})

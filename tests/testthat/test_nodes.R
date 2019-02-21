
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

  node12 <- Rflow::node$new(name = "node12", env = "env")
  expect_equal(node12$id, "env.node12", info = "Nodes can be initialize with name/env pair instead of id")
})

test_that("nodes can be initiated with persistence", {
  tmp_rflow_dir <- tempdir()
  node2 <- Rflow::node$new(id = "node2", persistence = list(path = tmp_rflow_dir))

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
  expect_true(is.null(node1$last_evaluated) || is.na(node1$last_evaluated))
})

test_that("nodes can be initiated with persistence", {
  tmp_rflow_dir <- tempdir()
  node2 <- Rflow::r_node$new(id = "node2", persistence = list(path = tmp_rflow_dir))
  expect_is(node2$persistence, "list")
  expect_true(node2$persistence$enabled)
  expect_equal(node2$persistence$path, tmp_rflow_dir)
  expect_equal(node2$persistence$file, "node2.rds")
})

test_that("nodes can be initiated with caching", {
  tmp_rflow_dir <- tempdir()
  node3 <- Rflow::r_node$new(id = "node3", cache = list(path = tmp_rflow_dir))
  expect_true(node3$cache$enabled)
  expect_equal(node3$cache$path, tmp_rflow_dir)
  expect_equal(node3$cache$file, "node3.rds")
})


# file_node ---------------------------------------------------------------


context("Creating file_node objects")

test_that("nodes can be initiated", {
  expect_error(Rflow::file_node$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::file_node$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::file_node$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::file_node$new(id = "node1"),   regexp = "[Mm]issing", info = "initialization requires file path")

  tmp_file <- tempfile()

  node1 <- Rflow::file_node$new(id = "node1", path = tmp_file)
  expect_s3_class(object = node1, class = "file_node")
  expect_s3_class(object = node1, class = "node")
  expect_s3_class(object = node1, class = "R6")

  expect_is(node1$persistence, "list")
  expect_false(node1$persistence$enabled)

  expect_null(node1$depends)
  expect_null(node1$trigger_condition)
  expect_true(is.null(node1$last_evaluated) || is.na(node1$last_evaluated))
})



context("Creating excel_sheet objects")

test_that("nodes can be initiated", {
  expect_error(Rflow::excel_sheet$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::excel_sheet$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::excel_sheet$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::excel_sheet$new(id = "node1"),   regexp = "[Mm]issing", info = "initialization requires file path")

  tmp_file <- tempfile()

  node1 <- Rflow::excel_sheet$new(id = "node1", path = tmp_file)
  expect_s3_class(object = node1, class = "excel_sheet")
  expect_s3_class(object = node1, class = "node")
  expect_s3_class(object = node1, class = "R6")

  expect_is(node1$persistence, "list")
  expect_false(node1$persistence$enabled)

  expect_null(node1$depends)
  expect_null(node1$trigger_condition)
  expect_true(is.null(node1$last_evaluated) || is.na(node1$last_evaluated))
})


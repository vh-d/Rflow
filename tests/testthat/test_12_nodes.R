
# node --------------------------------------------------------------------

context("generic node object")

test_that("nodes can be initiated", {
  expect_error(Rflow::node$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::node$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::node$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")

  node1 <- Rflow::node$new(id = "node1")
  expect_s3_class(object = node1, class = "node")

  expect_null(node1$depends)
  expect_null(node1$trigger_condition)

  node12 <- Rflow::node$new(name = "node12", env = "env")
  expect_equal(node12$id, "env.node12", info = "Nodes can be initialize with name/env pair instead of id")

  expect_identical(get_id(node12), "env.node12", info = "get_id works")
})



# file_node ---------------------------------------------------------------


context("file_node objects")

test_that("nodes can be initiated", {
  expect_error(Rflow::file_node$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::file_node$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::file_node$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::file_node$new(id = "node1", r_expr = expression_r(1)),   regexp = "[Mm]issing", info = "initialization requires file path")

  tmp_file <- tempfile()

  node1 <- Rflow::file_node$new(id = "node1", r_expr = expression_r(1), path = tmp_file)
  expect_s3_class(object = node1, class = "file_node")
  expect_s3_class(object = node1, class = "node")

  expect_is(node1$persistence, "list")
  expect_false(node1$persistence$enabled)

  expect_null(node1$depends)
  expect_null(node1$trigger_condition)
  expect_true(is.null(node1$last_evaluated) || is.na(node1$last_evaluated))
})



context("Constructing excel_sheet nodes")

test_that("nodes can be initiated", {
  expect_error(Rflow::excel_sheet$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::excel_sheet$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::excel_sheet$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::excel_sheet$new(id = "node1"),   regexp = "[Mm]issing", info = "initialization requires file path")

  tmp_file <- tempfile()

  node1 <- Rflow::excel_sheet$new(id = "node1", path = tmp_file)
  expect_s3_class(object = node1, class = "excel_sheet")
  expect_s3_class(object = node1, class = "node")

  expect_false(node1$persistence$enabled)

  expect_null(node1$depends)
  expect_null(node1$trigger_condition)
  expect_true(is.null(node1$last_evaluated) || is.na(node1$last_evaluated))

  data("iris")
  iris$Species <- as.character(iris$Species)
  openxlsx::write.xlsx(iris, tmp_file)
  iris_from_file <- node1$get()
  expect_identical(iris, iris_from_file, "Data can be restored.")
})



# csv_node ----------------------------------------------------------------


context("csv file nodes")

test_that("csv nodes can be initiated", {
  expect_error(Rflow::csv_node$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::csv_node$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::csv_node$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::csv_node$new(id = "node1"),   regexp = "[Mm]issing", info = "initialization requires file path")

  tmp_file <- tempfile()

  node1 <- Rflow::csv_node$new(id = "node1", path = tmp_file)
  expect_s3_class(object = node1, class = "csv_node")
  expect_s3_class(object = node1, class = "node")

  expect_false(node1$persistence$enabled)

  expect_null(node1$depends)
  expect_null(node1$trigger_condition)
  expect_true(is.null(node1$last_evaluated) || is.na(node1$last_evaluated))

  data("iris")
  write.csv(x = iris, file = tmp_file, row.names = FALSE)
  iris_from_file <- as.data.frame(node1$get(stringsAsFactors = TRUE))
  expect_identical(iris, iris_from_file, "Data can be restored.")
})


# excel_sheet ----------------------------------------------------------------


context("Excel sheet nodes")

test_that("csv nodes can be initiated", {
  expect_error(Rflow::excel_sheet$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::excel_sheet$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::excel_sheet$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::excel_sheet$new(id = "node1"),   regexp = "[Mm]issing", info = "initialization requires file path")

  tmp_file <- tempfile()

  node1 <- Rflow::excel_sheet$new(id = "node1", path = tmp_file)
  expect_s3_class(object = node1, class = "excel_sheet")
  expect_s3_class(object = node1, class = "node")

  expect_false(node1$persistence$enabled)

  expect_null(node1$depends)
  expect_null(node1$trigger_condition)
  expect_true(is.null(node1$last_evaluated) || is.na(node1$last_evaluated))

  data("iris")
  iris$Species <- as.character(iris$Species)
  openxlsx::write.xlsx(x = iris, file = tmp_file, row.names = FALSE)
  iris_from_file <- as.data.frame(node1$get())
  expect_identical(iris, iris_from_file, "Data can be restored.")
})



# db_node -----------------------------------------------------------------

context("db nodes")

test_that("db nodes can be initiated", {
  expect_error(Rflow::db_node$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::db_node$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::db_node$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::db_node$new(id   = "node1"), regexp = "solve_connection", info = "initialization requires connectionobject")

  db1 <<- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  # on.exit(DBI::dbDisconnect(db1))

  expect_warning(node1 <- Rflow::db_node$new(env = "db1", name = "node1"), regexp = "expression")
  expect_s3_class(object = node1, class = "db_node")
  expect_s3_class(object = node1, class = "node")

  expect_false(node1$persistence$enabled)

  expect_null(node1$depends)
  expect_null(node1$trigger_condition)
  expect_true(is.null(node1$last_evaluated) || is.na(node1$last_evaluated))

  data("iris")
  iris$Species <- as.character(iris$Species)
  DBI::dbWriteTable(conn = node1$connection, value = iris, name = "node1", connection = db1)
  iris_from_db <- as.data.frame(node1$get())
  expect_identical(iris, iris_from_db, "Data can be restored.")

  DBI::dbDisconnect(db1)
})


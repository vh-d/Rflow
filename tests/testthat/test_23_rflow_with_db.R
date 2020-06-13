context("Test complex workflow with DB objectgs")

## ----dependencies--------------------------------------------------------
library(magrittr)
library(DBI)
library(data.table)
library(Rflow)

td <- tempdir(check = TRUE)
params <- list(rflow = td)

# R environment
RDATA <- new.env(parent = .GlobalEnv)

# establish SQLite connection / environment
DB <-  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
RF <- new_rflow(logging = logger(handlers = list(handler_list())))


# node defs ---------------------------------------------------------------

node_defs <-
  list(

    "RDATA.table1" =
      list(
        tags = c("table"),
        r_expr =
          expression_r({
            N = 10;
            data.table(id = 1:N, value = rnorm(N))
          })
      ),

    "DB.table1" =
      list(
        tags    = c("db", "table"),
        type    = "db_node",
        connection = DB,
        depends = "RDATA.table1",
        r_expr  =
          expression_r({
            DBI::dbWriteTable(
              conn = self$connection,
              name = self$name,
              value = .RFLOW[["RDATA.table1"]]$get(),
              overwrite = TRUE
            )
          })
      ),

    "DB.table2" =
      list(
        tags    = c("db", "table"),
        type    = "db_node",
        connection = DB,
        depends = "RDATA.table2",
        r_expr  =
          expression_r({
            DBI::dbWriteTable(
              conn = self$connection,
              name = self$name,
              value = .RFLOW[["RDATA.table2"]]$get(),
              overwrite = TRUE
            )
          })
      ),

    "DB.table3" =
      list(
        tags = c("db", "table"),
        desc = "Table is a join of ...",
        type = "db_node",
        connection = DB,
        depends = c("DB.table1", "DB.table2"),

        # SQL code is passed as a character value
        sql_code = c("
CREATE TABLE table3 AS
SELECT
  table1.id     AS \"id\",
  table1.value  AS value1,
  table2.value  AS value2
FROM
  table1, table2
WHERE
  table1.id = table2.id")
      ),

    list( # unnamed objects has to have env + name combo defined inside
      env  = "RDATA",
      name = "table2",
      tags = c("table", "R"),
      type = "r_node",
      r_code = "
{
  N = 20
  data.table(id = 1:N, value = rnorm(N))
}"
    ),

    "RDATA.table3" = list(
      depends = c("DB.table3"),
      r_expr = expression_r({
        .RFLOW[["DB.table3"]]$value
      })
    )
  )


# processing definitons ---------------------------------------------------------

node_defs <- process_obj_defs(node_defs)

test_that("Nodes' definitions can be processed", {
  expect_named(node_defs)
  expect_true(all(names(node_defs) != ""))
})



# add nodes to rflow ------------------------------------------------------

added <-
  add_nodes(
    objs        = node_defs,
    rflow       = RF,
    cache       = RF$.cache$path,
    verbose     = FALSE
  )

test_that("Nodes can be added to the an rflow", {
  expect_true(all(added))
})


test_that("Graphs can be tested for loops", {
  expect_true(is_dag.rflow(RF))

  orig_dependencies <- RF[["RDATA.table1"]]$depends
  RF[["RDATA.table1"]]$depends <- c(orig_dependencies, "RDATA.table3")
  expect_false(is_dag.rflow(RF))

  RF[["RDATA.table1"]]$depends <- orig_dependencies
  expect_true(is_dag.rflow(RF))
})


test_that("Missing dependencies can be discovered", {
  RF[["RDATA.table3"]]$depends <- NULL
  expect_warning(not_passed <- sum(!verify_dependencies(RF)), regexp = "depend")
  expect_identical(not_passed, 1L)

  RF[["RDATA.table3"]]$depends <- "DB.table3"
  expect_silent(not_passed <- sum(!verify_dependencies(RF)))
  expect_identical(not_passed, 0L)
})


test_that("Simple graph queries can be performed", {

  set1 <- get_id(depends(RF$RDATA.table3, inverse = TRUE))
  set2 <- c("DB.table3", "DB.table1", "DB.table2", "RDATA.table1", "RDATA.table2")
  expect_setequal(set1, set2)

  set1 <- get_id(depends(RF$RDATA.table3, inverse = FALSE))
  expect_length(set1, 0)

  set1 <- get_id(depends(RF$DB.table3, inverse = FALSE))
  set2 <- c("RDATA.table3")
  expect_setequal(set1, set2)

})


make(RF, tags = "db", verbose = FALSE)
test_that("Nodes to be built can be selected by tags", {

  expect_true(RF$RDATA.table1$exists()) # RDATA.table1 was made as a dependency of DB.table1
  expect_true(RF$RDATA.table2$exists()) # RDATA.table2 was made as a dependency of DB.table2

  expect_true(RF$DB.table1$exists())
  expect_true(RF$DB.table2$exists())
  expect_true(RF$DB.table3$exists())

  expect_false(RF$RDATA.table3$exists())
})


make(RF, verbose = FALSE)
test_that("All nodes (that left) can be built.", {
  expect_true(RF$RDATA.table3$exists())
  expect_s3_class(RF$RDATA.table3$value, "data.frame")
})

test_that("Make accepts character vectors.", {
  res <- make(c("DB.table1", "DB.table2"), RF, verbose = FALSE)
  expect_false(all(res))
})

test_that("Nodes can be deleted from an Rflow", {
  delete(RF[["RDATA.table3"]])
  expect_null(RF[["RDATA.table3"]])

  delete("RDATA.table2", RF)
  expect_null(RF[["RDATA.table2"]])

  delete(RF, "RDATA.table1")
  expect_null(RF[["RDATA.table1"]])
})

## ----disconnect----------------------------------------------------------
DBI::dbDisconnect(DB)






context("R nodes")


#' Core properties and funcionality of nodes:
#' * has a valid ID
#' * inherits from a generic node class
#' * has exists() method that correctly returns TRUE/FALSE
#' * has a target that just exists or became existing after a succesfull eval()
#' * is persistent (stores all properties needed for re-initialization into the same state)
#'
#' Additional properties
#'
#' * has get() method and or value active binding
#' *



# r_node ------------------------------------------------------------------

context("r_node objects")


RDATA <- new.env() # right now there has to be an existing environment to initialize r_node

test_that("nodes can be initiated", {
  expect_error(Rflow::r_node$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::r_node$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::r_node$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")

  testnode <<- Rflow::r_node$new(env = "RDATA", name = "r1", r_expr = expression_r(1))
  expect_s3_class(testnode, "r_node")
})


test_that("nodes has a valid ID", {
  expect_identical(testnode$id, "RDATA.r1")
})


test_that("nodes inherits from node", {
  expect_s3_class(testnode, class = "r_node")
  expect_s3_class(testnode, class = "node")
})


test_that("has exists() method that correctly returns TRUE/FALSE", {
  expect_false(testnode$exists())

  .GlobalEnv$RDATA[[testnode$name]] <- 1
  expect_true(testnode$exists())

  remove(list = testnode$name, envir = .GlobalEnv$RDATA)
  expect_false(testnode$exists())
})


test_that("has a target that just exists or became existing after a succesfull eval()", {
  expect_output(res <- testnode$eval(), "Evaluating.*done")
  expect_true(res)
  expect_true(testnode$exists())
})


# test_that("is persistent (stores all properties needed for re-initialization into the same state)", {
#
# })

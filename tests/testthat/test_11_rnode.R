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

# library(testthat)
# library(Rflow)



test_that("nodes can be initiated", {
  RDATA <- new.env() # right now there has to be an existing environment to initialize r_node
  expect_error(Rflow::r_node$new(),               regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::r_node$new(name = "node1"), regexp = "[Mm]issing", info = "initialization requires id or env + name")
  expect_error(Rflow::r_node$new(env  = "env1"),  regexp = "[Mm]issing", info = "initialization requires id or env + name")
  
  testnode <- Rflow::r_node$new(env = "RDATA", name = "r1", r_expr = expression_r(1))
  expect_s3_class(testnode, "r_node")
  expect_identical(testnode$id, "RDATA.r1", info = "nodes has a valid ID")
  
  expect_s3_class(testnode, class = "r_node")
  expect_s3_class(testnode, class = "node") #nodes inherits from node
  
  expect_identical(testnode$r_env, RDATA, info = "constructor should connect to the declared environment")
})


test_that("has exists() method that correctly returns TRUE/FALSE", {
  RDATA <- new.env() # right now there has to be an existing environment to initialize r_node
  testnode <- Rflow::r_node$new(env = "RDATA", name = "r1", r_expr = expression_r(1))
  
  expect_false(testnode$exists())
  
  RDATA[[testnode$name]] <- 1
  expect_true(testnode$exists())
  
  remove(list = testnode$name, envir = RDATA)
  expect_false(testnode$exists())
})


test_that("has a target that just exists or became existing after a succesfull eval()", {
  RDATA <- new.env() # right now there has to be an existing environment to initialize r_node
  testnode <- Rflow::r_node$new(env = "RDATA", name = "r1", r_expr = expression_r(1))
  
  expect_output(res <- testnode$eval(), "Evaluating.*done")
  expect_true(res)
  expect_true(testnode$exists())
  expect_identical(RDATA[["r1"]], 1)
  expect_identical(testnode$get(), 1)
  expect_identical(testnode$value, 1)
})


test_that("is persistent (stores all properties needed for re-initialization into the same state)", {
  
  RDATA <- new.env() # right now there has to be an existing environment to initialize r_node
  persist_dir <- tempdir()
  
  testnode <-
    Rflow::r_node$new(
      env = "RDATA",
      name = "r2",
      r_expr = expression_r(1:10),
      persistence = list(path = persist_dir),
      store = TRUE
    )
  
  expect_true(testnode$persistence$enabled)
  fp <- file.path(testnode$persistence$path, testnode$persistence$file)
  expect_true(file.exists(fp))
  
  # readRDS(fp)
  
  testnode_restored <- as_node(Rflow:::load_state_of_node(fp))
  # all.equal(testnode, testnode_restored)
  
  expect_identical(testnode$id,     testnode_restored$id)
  expect_identical(testnode$env,    testnode_restored$env)
  expect_identical(testnode$name,   testnode_restored$name)
  expect_identical(testnode$r_env,  testnode_restored$r_env)
  expect_identical(testnode$r_expr, testnode_restored$r_expr)
  expect_identical(testnode$hash,   testnode_restored$hash)
  
  # remove(list = testnode$name, envir = RDATA)
  expect_false(testnode$exists())
  
  remove(testnode)
  remove(testnode_restored)
  
  unlink(persist_dir, recursive = TRUE)
})



test_that("caching works", {
  
  RDATA <- new.env() # right now there has to be an existing environment to initialize r_node
  cache_dir <- tempdir(check = TRUE)
  
  expect_output(
    testnode <-
      Rflow::r_node$new(
        env    = "RDATA",
        name   = "r2",
        r_expr = expression_r(1:10),
        cache  = list(path = cache_dir)
      ), 
    regexp = "no cache found", fixed = FALSE
  )
  
  expect_true(testnode$cache$enabled)
  fp <- file.path(testnode$cache$path, testnode$cache$file)
  expect_false(file.exists(fp))
  
  # readRDS(fp)
  
  expect_false(testnode$exists())
  expect_true(testnode$eval(verbose = FALSE))
  expect_true(testnode$exists())
  expect_false(testnode$eval(verbose = FALSE))
  
  expect_output(testnode$remove(), "removing the target", fixed = TRUE)
  expect_false(testnode$exists())
  
  testnode$cache_restore()
  expect_true(testnode$exists())
  expect_identical(RDATA$r2, 1:10)
  expect_identical(testnode$get(), 1:10)
  expect_identical(testnode$value, 1:10)
})


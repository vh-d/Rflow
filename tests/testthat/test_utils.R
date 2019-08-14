context("Help functions")

test_that("IDs can be decomposed into env + name pairs", {
  tmp <- Rflow::env_name_from_id("Env1.node1")
  expect_equal(tmp$env,  "Env1")
  expect_equal(tmp$name, "node1")
  
  # expect_equal(Rflow::env_name_from_id("node2"),      data.table(env = NA_character_, name = "node2"))
  # expect_equal(Rflow::env_name_from_id(c("DB1.tab_1", "DB1.tab_2")), 
  #              data.table(env = c("DB1", "DB1"), name = c("tab_1", "tab_2")))
})


test_that("SQL code can be converted in a list object", {
  sql_statements <- c("drop tab if exists", "select * from tab")
  tmp <- sql_structure(sql_statements, ignoreErrors = FALSE)
  
  expect_equal(tmp[[1]]$code, sql_statements[1])
})


test_that("in.R6 behaves similarly as %in% for R6 objects", {
  n1 <- Rflow::node$new("Node1")
  n2 <- Rflow::node$new("Node2")
  n3 <- Rflow::node$new("Node3")
  
  expect_true(in.R6(n1, c(n1, n2)))
  expect_false(in.R6(n3, c(n1, n2)))
  expect_false(in.R6(n3, c()))
  expect_false(in.R6(n3, NULL))
  
})


test_that("expression_r strips attributes from expressions", {
  
  # test
  l1 <- list(expression_r({a = 1; b = 2}), expression_r({a = 1; b = 2}))
  l2 <- list(expression({a = 1; b = 2}), expression({a = 1; b = 2}))
  
  expect_identical(lapply(l1, eval), lapply(l2, eval))
  
  hashes <- sapply(l1, digest::digest)
  expect_equal(hashes[1], hashes[2])
})

# rexp <- expression({1+1})
# rexp_tidy <- as_r_expr(r_expr = rexp)
# str(rexp[[1]])
# str(rexp_tidy[[1]])
# cat(attr(rexp_tidy, "src"))

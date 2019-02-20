context("Help functions")

test_that("IDs can be decomposed into env + name pairs", {
  tmp <- Rflow::env_name_from_id("Env1.node1")
  expect_equal(tmp$env,  "Env1")
  expect_equal(tmp$name, "node1")
  
  # expect_equal(Rflow::env_name_from_id("node2"),      data.table(env = NA_character_, name = "node2"))
  # expect_equal(Rflow::env_name_from_id(c("DB1.tab_1", "DB1.tab_2")), 
  #              data.table(env = c("DB1", "DB1"), name = c("tab_1", "tab_2")))
})

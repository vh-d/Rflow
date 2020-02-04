
context("Validators")

test_that("Validators can be constructed", {
  v1 <- validator(function(x) x > 1)
  expect_s3_class(v1, "validator")
  expect_s3_class(v1, "validator_r")
  expect_identical(v1, validator(function(x) x > 1))
  
  v2 <- validator_r(expression_r(TRUE))
  expect_s3_class(v1, "validator")
  expect_s3_class(v1, "validator_r")
  expect_identical(v2, validator(expression_r(TRUE)))
})


test_that("Validators can be confronted with data", {
  v1 <- validator_r(function(x) x > 1)
  
  expect_true(passes(confront(v1, 2)))
  expect_false(passes(confront(v1, 1)))
  expect_false(passes(confront(v1, NA)))
  expect_false(passes(confront(v1, NULL)))
  
  v2 <- validator_r(expression_r(TRUE))
  expect_true(passes(confront(v2)))
  
  v3 <- validator_r(function(x) all(dim(x)>0))
  df1 <- data.frame(a = numeric())
  df2 <- data.frame(a = 1:10)
  expect_false(passes(confront(v3, df1)))
  expect_true(passes(confront(v3, df2)))
})


test_that("Validators can be confronted with data", {
  df0 <- c()
  df1 <- data.frame(id = numeric())
  df2 <- data.frame(value = 1:10)
  df3 <- data.frame(id = 1L:10L, value = 21:20)
  
  validators <- 
    list(
      "is_data_frame"   = validator_r(function(x) is.data.frame(x)),
      "dimensionality"  = validator_r(function(x) nrow(x)>0),
      "has_all_columns" = validator_r(function(x) all(c("id", "value") %in% colnames(x)))
    )
  
  results <- 
    data.table::rbindlist(
      list(
        "df0" = passes(confront(validators, df0)),
        "df1" = passes(confront(validators, df1)),
        "df2" = passes(confront(validators, df2)),
        "df3" = passes(confront(validators, df3))
      ), 
      idcol = "object"
    )
  
  expect_identical(results[["is_data_frame"]],   c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(results[["dimensionality"]],  c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(results[["has_all_columns"]], c(FALSE, FALSE, FALSE, TRUE))
  
})

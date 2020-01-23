
# logging to file -------------------------------------------------------------------------

context("logging to a file")

readAndClose <- function(path) {
  con <- file(path)
  res <- readLines(con)
  close(con)

  res
}

tmpf1 <- tempfile()
tmpf2 <- tempfile()

test_that("Logger and handlers can be constructed", {

  h1 <<- handler_file(tmpf1)
  expect_type(h1, "environment")
  expect_s3_class(h1, "handler")

  h2 <<- handler_file(tmpf2)


  logger_tmp <<- logger()
  expect_type(logger_tmp, "environment")
  expect_s3_class(logger_tmp, "logger")

  logger1 <<- logger(handlers = list(h1))
  expect_identical(logger1$handlers[[1]], h1)

  logger2 <<- logger(handlers = list(h2))
  expect_identical(logger2$handlers[[1]], h2)

  logger12 <<- logger(handlers = list(h1, h2))
  expect_identical(logger12$handlers[[1]], h1)
  expect_identical(logger12$handlers[[2]], h2)
})

test_that("Log messages get to the log files", {
  m1 <- "some message 3"

  log_record(logger1, m1)

  close(h1$con)
  obs_log <- readAndClose(h1$path)
  expect_length(obs_log, 1L) # Log file has exacatly one line of text
  expect_true(any(grepl(m1, obs_log, fixed = TRUE)))

  m2 <- "some other message"
  log_record(logger12, m2)

  close(h1$con)
  obs_log <- readAndClose(h1$path)
  expect_length(obs_log, 2) # the first log now received both messages
  expect_true(any(grepl(m2, obs_log, fixed = TRUE)))

  close(h2$con)
  obs_log <- readAndClose(h2$path)
  expect_length(obs_log, 1) # the second log received only the second message
  expect_false(any(grepl(m1, obs_log, fixed = TRUE)))
  expect_true( any(grepl(m2, obs_log, fixed = TRUE)))
})

test_that("Hanlders can be closed", {
  expect_identical(close(h1), 0L)
  expect_identical(close(h2), 0L)
})



# logging to file -------------------------------------------------------------------------

context("logging to a list")


test_that("Logger and handlers can be constructed", {

  h1 <- handler_list()
  expect_type(h1, "environment")
  expect_s3_class(h1, "handler_list")
  expect_s3_class(h1, "handler")

  h2 <- handler_list()

  logger_tmp <<- logger()
  expect_type(logger_tmp, "environment")
  expect_s3_class(logger_tmp, "logger")

  logger1 <- logger(handlers = list(h1))
  expect_identical(logger1$handlers[[1]], h1)

  logger2 <- logger(handlers = list(h2))
  expect_identical(logger2$handlers[[1]], h2)

  logger12 <- logger(handlers = list(h1, h2))
  expect_identical(logger12$handlers[[1]], h1)
  expect_identical(logger12$handlers[[2]], h2)
})


test_that("Log messages get to the log files", {
  h1 <- handler_list()
  h2 <- handler_list()

  logger1 <- logger(handlers = list(h1))
  logger2 <- logger(handlers = list(h1))
  logger12 <- logger(handlers = list(h1, h2))

  m1 <- "some message 3"

  log_record(logger1, m1)

  obs_log <- as.list(h1)
  expect_length(obs_log, 1L) # Log file has exacatly one line of text
  expect_true(any(grepl(m1, obs_log, fixed = TRUE)))

  m2 <- "some other message"
  log_record(logger12, m2)

  obs_log <- as.list(h1)
  expect_length(obs_log, 2) # the first log now received both messages
  expect_true(any(grepl(m2, obs_log, fixed = TRUE)))

  obs_log <- as.list(h2)
  expect_length(obs_log, 1) # the second log received only the second message
  expect_false(any(grepl(m1, obs_log, fixed = TRUE)))
  expect_true( any(grepl(m2, obs_log, fixed = TRUE)))
})



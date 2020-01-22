
context("Test logging")

readAndClose <- function(path) {
  con <- file(path)
  res <- readLines(con)
  close(con)

  res
}

tmpf1 <- tempfile()
tmpf2 <- tempfile()

test_that("Logger and handlers can be constructed", {

  fh1 <<- handler_file(tmpf1)
  expect_type(fh1, "environment")
  expect_s3_class(fh1, "handler")

  fh2 <<- handler_file(tmpf2)


  logger_tmp <<- logger()
  expect_type(logger_tmp, "environment")
  expect_s3_class(logger_tmp, "logger")

  logger1 <<- logger(handlers = list(fh1))
  expect_identical(logger1$handlers[[1]], fh1)

  logger2 <<- logger(handlers = list(fh2))
  expect_identical(logger2$handlers[[1]], fh2)

  logger12 <<- logger(handlers = list(fh1, fh2))
  expect_identical(logger12$handlers[[1]], fh1)
  expect_identical(logger12$handlers[[2]], fh2)
})

test_that("Log messages get to the log files", {
  m1 <- "some message 3"

  log_record(logger1, m1)

  close(fh1$con)
  obs_log <- readAndClose(fh1$path)
  expect_length(obs_log, 1L) # Log file has exacatly one line of text
  expect_true(any(grepl(m1, obs_log, fixed = TRUE)))

  m2 <- "some other message"
  log_record(logger12, m2)

  close(fh1$con)
  obs_log <- readAndClose(fh1$path)
  expect_length(obs_log, 2) # the first log now received both messages
  expect_true(any(grepl(m2, obs_log, fixed = TRUE)))

  close(fh2$con)
  obs_log <- readAndClose(fh2$path)
  expect_length(obs_log, 1) # the second log received only the second message
  expect_false(any(grepl(m1, obs_log, fixed = TRUE)))
  expect_true( any(grepl(m2, obs_log, fixed = TRUE)))
})

test_that("Hanlders can be closed", {
  expect_identical(close(fh1), 0L)
  expect_identical(close(fh2), 0L)
})

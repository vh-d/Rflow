# logger ------------------------------------------------------------------

#' logger constructor
#' @param name name of the logger
#' @param enabled logical; enable the logger on initialization?
#' @param handlers a list of handlers can be supplied
#'
#' @export
logger <- function(name = "ROOT", enabled = TRUE, handlers = NULL) {
  new_logger <- structure(
    class = "logger",
    .Data = list2env(
      list(
        "name"     = as.character(name),
        "enabled"  = isTRUE(enabled),
        "handlers" = as.list(handlers)
      ),
      envir = new.env(parent = emptyenv())
    )
  )

  return(new_logger)
}

#' @export
print.logger <- function(x) {
  cat("<loggger>: ", x[["name"]], "\n", sep = "")
  cat("  enabled: ", x[["enabled"]], "\n", sep = "")
  cat("  handlers: ", paste0(x[["handlers"]], collapse = ", "), "\n")
}




#' Add a handler to a logger
#' @param x logger
#' @param y handler
#'
#' @export
add_handler <- function(x, y) {
  UseMethod("add_handler", x)
}

#' @export
add_handler.logger <- function(x, y) {
  UseMethod("add_handler.logger", y)
}

#' @export
add_handler.handler <- function(x, y) {
  UseMethod("add_handler.handler", y)
}

#' @export
add_handler.logger.handler <- function(logger, handler) {
  logger[["handlers"]] <- unique(c(handler, handler[["handlers"]]))

  invisible(TRUE)
}

#' @export
add_handler.handler.logger <- function(handler, logger) {
  logger[["handlers"]] <- unique(c(handler, handler[["handlers"]]))

  invisible(TRUE)
}

#' Log a record/message
#' @param x an object of class that supprt logging
#' @param ... args passed to a logger
#' @rdname log_record
#'
#' @export
log_record <- function(x, ...) {
  UseMethod("log_record", x)
}

#' @rdname log_record
#' @export
log_record.logger <- function(logger, ...) {

  # distribute record to all handlers
  for (handler in logger[["handlers"]]) {
    log_record(handler, format(Sys.time()), logger[["name"]], ...)
  }

  invisible(TRUE)
}

#' @rdname log_record
#' @export
log_record.rflow <- function(rflow, ...) {
  log_record(rflow[[".loggers"]], "RFLOW", ...)
}

#' @rdname log_record
#' @export
log_record.list <- function(x, ...) {
  for (i in x) {
    log_record(i, ...)
  }
}


#' Add loggers to an object
#' @param x a node object
#' @param loggers a list of loggers
#'
#' @export
add_loggers <- function(x, loggers) {
  UseMethod("add_loggers", x)
}

#' @export
add_loggers.node <- function(x, loggers) {
  x$loggers <- unique(c(list(), x$loggers, loggers))
}




# handler -----------------------------------------------------------------



#' Handler constructors
#' @param path to a log file
#' @param open logical; open the connection immediately
#' @param enable logical; enable the handler
#'
#' @rdname handler_contructors
#'
#' @export
handler_file <- function(path, open = TRUE, enable = TRUE) {
  new_handler <- structure(new.env(), class = c("handler_file", "handler"))
  new_handler[["path"]] <- path
  new_handler[["enabled"]] <- isTRUE(enable)
  new_handler[["con"]] <- if (isTRUE(open)) file(path, "a", encoding = "UTF-8", blocking = FALSE)

  return(new_handler)
}

#' @export
close.handler_file <- function(x) {
  close(x[["con"]])
}


#' @export
open.handler_file <- function(x) {
  x[["con"]] <- file(x[["path"]], open = "a", encoding = "UTF-8")

  invisible(TRUE)
}

has_open_con <- function(x, ...) {
  UseMethod("has_open_con", x)
}

has_open_con.handler_file <- function(x, ...) {
  tryCatch(isTRUE(isOpen(x[["con"]])), error = function(e) FALSE)
}

#' @export
print.handler_file <- function(x) {
  cat("<log file handler>\n", sep = "")
  cat("  enabled: ", x[["enabled"]], "\n", sep = "")
  cat("  path: ", x[["path"]], "\n", sep = "")
  cat("  open: ", isOpen.handler_file(x), "\n", sep = "")
}

#' @export
log_record.handler_file <- function(handler, ...) {
  if (!has_open_con(handler)) open(handler)
  text <- paste(..., sep = ":", collapse = ", ")
  # print(text)
  writeLines(con = handler[["con"]], text = text)
}

#' @export
log_record.node <- function(x, ...) {
  if (!isFALSE(x[["logging"]])) log_record(x$loggers, x$id, ...)
}


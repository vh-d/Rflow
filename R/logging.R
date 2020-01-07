# logger ------------------------------------------------------------------


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

#' @export
log_record <- function(x, ...) {
  UseMethod("log_record", x)
}

#' @export
log_record.logger <- function(logger, ...) {
  
  # distribute record to all handlers
  for (handler in logger[["handlers"]]) {
    log_record(handler, format(Sys.time()), logger[["name"]], ...)
  }
  
  invisible(TRUE)
}

#' @export
log_record.rflow <- function(rflow, ...) {
  log_record(rflow[[".loggers"]], ...)
}

#' @export
log_record.list <- function(x, ...) {
  for (i in x) {
    log_record(i, ...)
  }
}

#' @export
add_loggers <- function(x, loggers) {
  UseMethod("add_loggers", x)
}

#' @export
add_loggers.node <- function(x, loggers) {
  x$loggers <- unique(c(list(), x$loggers, loggers))
}

# handler -----------------------------------------------------------------



#' @export
handler_file <- function(path, open = TRUE, enable = TRUE) {
  new_handler <- structure(new.env(), class = c("handler_file", "handler"))
  new_handler[["path"]] <- path
  new_handler[["enabled"]] <- isTRUE(enable)
  new_handler[["con"]] <- if (isTRUE(open)) file(path, "a+", encoding = "UTF-8", blocking = TRUE)
  
  return(new_handler)
}

#' @export
close.handler_file <- function(x) {
  close(x[["con"]])
}


#' @export
open.handler_file <- function(x) {
  x[["con"]] <- file(x[["path"]])
  
  invisible(TRUE)
}

#' @export
print.handler_file <- function(x) {
  cat("<log file handler>\n", sep = "")
  cat("  enabled: ", x$enabled, "\n", sep = "")
  cat("  path: ", x$path, "\n", sep = "")
  cat("  open: ", tryCatch(isOpen(x$con), error = function(e) FALSE), "\n", sep = "")
}

#' @export
log_record.handler_file <- function(handler, ...) {
  writeLines(con = handler[["con"]], text = paste(..., sep = ":", collapse = ", "))
}

#' @export
log_record.node <- function(x, ...) {
  if (!isFALSE(x$logging)) log_record(x$loggers, x$id, ...)
}

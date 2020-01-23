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
    log_record(handler, format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"), logger[["name"]], ...)
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




# handlers -----------------------------------------------------------------

# __ expanding list

# Courtery of Jan Kanis at https://stackoverflow.com/a/32870310
# expandingList <- function(capacity = 10) {
#     buffer <- vector('list', capacity)
#     length <- 0
# 
#     methods <- list()
# 
#     methods$double.size <- function() {
#         buffer <<- c(buffer, vector('list', capacity))
#         capacity <<- capacity * 2
#     }
# 
#     methods$add <- function(val) {
#         if(length == capacity) {
#             methods$double.size()
#         }
# 
#         length <<- length + 1
#         buffer[[length]] <<- val
#     }
# 
#     methods$as.list <- function() {
#         b <- buffer[0:length]
#         return(b)
#     }
# 
#     methods
# }

#' @export
handler_list <- function(enabled = TRUE, capacity = 10) {
  
    enabled <- enabled
    buffer <- vector('list', capacity)
    length <- 0
    
    double.capacity <- function() {
        buffer <<- c(buffer, vector('list', capacity))
        capacity <<- capacity * 2
    }
    
    add <- function(val) {
      if (!isTRUE(enabled)) return(invisible(NULL))
      
      if(length == capacity) {
        double.capacity()
      }
      
      length <<- length + 1
      buffer[[length]] <<- val
    }
    
    structure(
        class = c("handler_list", "handler"),
        environment()
    )
}

#' @export
as.list.handler_list <- function(x) {
    x[["buffer"]][1:x[["length"]]]
}

#' @export
print.handler_list <- function(x) {
  cat("<list log handler>\n", sep = "")
  cat("  enabled: ", x[["enabled"]], "\n", sep = "")
  cat("  length: ",  x[["length"]], "\n", sep = "")
}

#' @export
log_record.handler_list <- function(handler, ...) {
  text <- paste(..., sep = ":", collapse = ", ")
  # print(text)
  handler$add(text)
}

#' @export
close.handler_list <- function(x) {
  invisible(NULL)
}



# __ file -----------------------------------------------------------------

#' File handler
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
  if (has_open_con(x)) close(x[["con"]]) else invisible(0L)
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

#' Close all logs
#'
#' @param x 
#'
#' @export
close_log <- function(x) {
  UseMethod("close_log", x)
}

#' @export
close_log.rflow <- function(x) {
  close(x$.loggers)
}

#' @export
close.list <- function(x) {
  for (i in x) {
    close(i)
  }
}

#' @export
close.logger <- function(x) {
  close(x$handlers)
}

#' @export
close.handler <- function(x) {
  close(x$con)
}

#' @export
reopen.handler_file <- function(x) {
  close.handler_file(x)
  open.handler_file(x)
}


#' @export
reset_log <- function(x) {
  UseMethod("reset_log", x)
}

#' @export
reset_log.rflow <- function(x) {
  # TBA
}


# __ terminal -------------------------------------------------------------
  

#' Terminal log handler
#' @param enable logical
#'
#' @export
handler_terminal <- function(enable = TRUE) {
  new_handler <- structure(new.env(), class = c("handler_terminal", "handler"))
  new_handler[["enabled"]] <- isTRUE(enable)

  return(new_handler)
}


#' @export
print.handler_terminal <- function(x) {
  cat("<terminal log handler>\n", sep = "")
  cat("  enabled: ", x[["enabled"]], "\n", sep = "")
}


#' @export
log_record.handler_terminal <- function(handler, ...) {
  text <- paste(..., sep = ":", collapse = ", ")
  writeLines(con = stdout(), text = text)
}



# other -------------------------------------------------------------------


#' @export
log_record.node <- function(x, ...) {
  if (!isFALSE(x[["logging"]])) log_record(x$loggers, x$id, ...)
}


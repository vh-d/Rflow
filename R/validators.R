

#' @export
validator <- function(x, ...) {
  UseMethod("validator", x)
}


#' @export
print.validator <- function(x, ...) {
  warning("This an empty method!")
}


#' @export
confront <- function(x, ...) {
  UseMethod("confront", x)
}


#' @export
confront.list <- function(x, ...) {
  structure(
    lapply(x, confront, ...),
    class = c("validations", "list")
  )
}

#' @importFrom data.table as.data.table
#' @export
as.data.table.validations <- function(x, ...) {
  data.table::rbindlist(x, idcol = "validator", use.names = TRUE, fill = TRUE)
}

#' @export
passes <- function(x, ...) {
  UseMethod("passes", x)
}

#' @export
passes.list <- function(x, ...) {
  lapply(x, passes, ...)
}


# R validators ------------------------------------------------------------------


#' @export
validator.expression <- function(x, ...) {
  validator_r(x, ...)
}


#' @export
validator_r <- function(x, ...) {
  UseMethod("validator_r", x)
}

#' @export
validator_r.default <- function(x, ...) {
  validator_r(as.expression(x), ...)
}

#' @export
validator_r.function <- function(x, signal = "info", msg = NA_character_, ...) {
  structure(
    list(
      signal = signal,
      msg    = msg,
      fun    = x
    ),
    class = c("validator_r", "validator"))
}


#' @export
validator.function   <- validator_r.function


#' @export
validator_r.expression <- function(x, ...) {
  fun <- function(x) {}
  body(fun) <- x
  environment(fun) <- parent.frame()
  validator_r(fun, ...)
}


#' @export
validator.expression <- validator_r.expression


# validator_r.character <- function(x, signal = "info", msg = NULL, file = FALSE, ...) {
# 
#   if (isTRUE(file)) return(validator_r_file(x, ...))
#   # else:
# 
#   structure(
#     list(
#       signal = signal,
#       msg    = msg,
#       r_expr = parse(text = x),
#       code   = x
#     ),
#     class = c("validator_r_expr", "validator_r", "validator"))
# }


#' @export
confront.validator_r <- function(x, ...) {
  structure(
    list(
      passed = isTRUE(x[["fun"]](...)), 
      signal = x[["signal"]], 
      msg    = x[["msg"]]
    ),
    class = "validation"
  )
}

# validator_r_file <- function(x, ...) {
#   struct <- validator_file(x, ...)
#   class(struct) <- c("validator_r_file", "validator_r", "validator_file", class(struct))
# }
# 
# confront.validator_r_file <- function(x, ...) {
#   source(x$path, ...)
# }


#' @export
print.validator_r <- function(x, ...) {
  cat("<validator>", x[["name"]],    "\n", sep = "")
  cat("  signal: ", x[["signal"]],  "\n", sep = "")
  cat("  message: ", x[["message"]], "\n", sep = "")
}


#' @export
passes.validation <- function(x, ...) {
  isTRUE(x[["passed"]])
}

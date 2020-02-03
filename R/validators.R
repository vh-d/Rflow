

#' @export
validator <- function(x, ...) {
  UseMethod("validator", x)
}

#' @export
evaluate.validator <- function(x, ...) {
  warning("This an empty method!")
}

print.validator <- function(x, ...) {
  warning("This an empty method!")
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
validator_r.function <- function(x, ...) {
  structure(
    list(
      r_func = x
      # code   = deparse_nicely(x)
    ),
    class = c("validator_r_func", "validator_r", "validator"))
}

#' @export
validator.function   <- validator_r.function

#' @export
validator_r.expression <- function(x, ...) {
  structure(
    list(
      r_expr = x,
      code   = deparse_nicely(x)
    ),
    class = c("validator_r_expr", "validator_r", "validator"))
}

#' @export
validator.expression <- validator_r.expression


#' @export
validator_r.character <- function(x, file = FALSE, ...) {

  if (isTRUE(file)) return(validator_r_file(x, ...))
  # else:

  structure(
    list(
      r_expr = parse(text = x),
      code   = x
    ),
    class = c("validator_r_expr", "validator_r", "validator"))
}

#' @export
evaluate.validator_r_expr <- function(x, ...) {
  isTRUE(eval(x$r_expr, ...))
}

#' @export
evaluate.validator_r_func <- function(x, ...) {
  isTRUE(x$r_func(...))
}

#' @export
validator_r_file <- function(x, ...) {
  struct <- validator_file(x, ...)
  class(struct) <- c("validator_r_file", "validator_r", "validator_file", class(struct))
}

#' @export
evaluate.validator_r_file <- function(x, ...) {
  source(x$path, ...)
}

#' @export
print.validator_r <- function(x, ...) {
  cat("<validator> ", x[["name"]], "\n", sep = "")
}

#' @export
print.validator_r_func <- function(x, ...) {
  NextMethod()
  cat("   type: ", "function", "\n", sep = "")
}

#' @export
print.validator_r_expr <- function(x, ...) {
  NextMethod()
  cat("   type: ", "expression", "\n", sep = "")
  cat("   code: ", "\n", sep = "")
  cat_with_prefix(x[["code"]], prefix = "     ")
}

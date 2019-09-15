job <- function(x, ...) {
  UseMethod("job", x)
}

job.character <- function(x, ...) {
  do.call(paste0(x, "_job"), ...)
}

job.list <- function(x) {
  do.call(job, x$type, x)
}

job.expression <- function(x, ...) {
  job_r(r_expr = x)
}

#' @export
job_r <- function(
  r_expr = NULL,
  r_code = NULL,
  r_file = NULL
) {

  job <- structure(list(r_expr = NULL, mode = NULL), class = c("job_r", "job"))

  if (length(r_expr)) {
    job$mode <- "expression"
    job$r_expr <- r_expr

    return(job)
  }

  if (length(r_code)) {
    job$mode <- "code"
    job$code <- r_code
    job$r_expr <- parse(text = r_code)

    return(job)
  }

  if (length(r_file)) {
    job$mode <- "file"
    job$file <- r_file
    job$code <- paste0(eadLines(r_file))
    job$r_expr <- parse(file = r_file)

    return(job)
  }

  # else:
  # warning("?")
  return(NULL)

}


#' @export
job_file <- function(x, ...) {

  struct <- list(
    fp   = x,
    hash = hash_file(x)
  )

  struct <- union.list(list(...), struct)

  structure(struct, class = c("job_file", "job"))
}

read.job_file <- function(x, ...) {
  paste0(readLines(x$path, ...))
}

#' @export
job_r_file <- function(x, ...) {
  struct <- job_file(x, ...)
  class(struct) <- c("job_r_file", "job_r", class(struct))
}


job_sql_code <- function(x, mode = "execute", ...) {

}

#' @export
job_sql_file <- function(x, mode = "execute", ...) {
  struct <- job_file(x, mode = mode, ...)
  class(struct) <- c("job_sql_file", "job_sql", class(struct))
}


#' @export
evaluate_job <- function(x, ...) {
  UseMethod("evaluate_job", x)
}

#' @export
evaluate_job.job_r_expr <- function(x, ...) {
  eval(x$r_expr, ...)
}

#' @export
evaluate_job.job_r_file <- function(x, ...) {
  source(x$path, ...)
}


hash_file <- function(x, algo = "md5", ...) {
  structure(
    list(
      value = digest::digest(x, algo = algo, file = TRUE, ...),
      path = x,
      time = Sys.time()
    ),
    class = c("hash_file", "hash")
  )
}

expr2fun <- function(expr, depends, envir = NULL) {

  # this seems work better then as.function()
  f <- function() {}
  body(f) <- expr

  . <- paste0('.RFLOW[["', depends, '"]]$get()')
  . <- lapply(., str2lang)
  . <- setNames(., depends)
  . -> formals(f)

  if (is.environment(envir)) environment(f) <- envir

  return(f)
}

# .RFLOW <- new.env()
# .RFLOW[["RDATA.tab"]] <- 1
# .RFLOW[["RDATA.tab2"]] <- 10
#
# depends <- c("RDATA.tab", "RDATA.tab2")
#
# expr2fun(expression_r({RDATA.tab+RDATA.tab2}), depends, .GlobalEnv)


strip_srcrefs <- function(expr) {
  attr(expr[[1]], "srcref")      <- NULL
  attr(expr[[1]], "srcfile")     <- NULL
  attr(expr[[1]], "wholeSrcref") <- NULL

  expr
}

#' Returns R expression from either R expression or parsed R code
#'
#' @param x either character or R expression vector of R code
#'
#' @return an R expression object with src attribute containing original code that can be used for printing
#' @export
#'
#' @examples
#' as_r_expr("{\n1+1\n}")
#' as_r_expr({1+1})
as_r_expr <- function(x) {
  UseMethod("as_r_expr", x)
}

#' @export
#' @rdname as_r_expr
as_r_expr.default <- function(x) {
  warning("Coercing ", substitute(x), " to a character")
  as_r_expr(as.character(x))
}

#' @export
#' @rdname as_r_expr
as_r_expr.expression <- function(x) {

  if (length(x)) {

    if (length(attr(x, "src"))) return(x)
    # else:
    return(
      structure(
        strip_srcrefs(x),
        src = paste0(deparse(x, control = "useSource"), collapse = "\n")
      )
    )
  } # else:
  return(structure(expression(), src = ""))
}

#' @export
#' @rdname as_r_expr
as_r_expr.character <- function(x) {
  if (length(x)) {

    return(
      structure(
        parse(text = x),
        src = x
      )
    )
  } # else if (legnth(r_file))

  # else:
  return(structure(expression(), src = ""))
}


#' Expression with source code references stripped off
#'
#' @param ... expression (see \code{\link[base]{expression}})
#'
#' @return
#' `expression_r` returns expression object similar to the one returned from \code{\link[base]{expression}} except references to source code
#' @export
#' @seealso \code{\link[base]{expression}}
#' @examples
#' identical(expression(1+1), expression_r(1+1))
#' identical(expression({1+1}), expression_r({1+1}))
expression_r <- function(x){
  # simulate behaviour of expression() (but accept only one argument)
  exprs <- match.call(expand.dots = TRUE)
  exprs[1] <- expression(expression)
  exprs <- eval(exprs)

  return(as_r_expr(exprs))
}

#' Print with prefix
#'
#' @param x value to print
#' @param verbose_prefix prefix to be added after every new-line symbol
#'
#' @return
#'
#' @examples
add_prefix <- function(x, prefix = "", color_main = NULL, color_prefix = NULL) {
  . <- unlist(crayon::col_strsplit(x, split = "\n", fixed = TRUE))
  . <- paste0(prefix, .)
  .
}

# add_prefix <- function(x, prefix = "", color_main = NULL, color_prefix = NULL) {
#   . <- unlist(stringr::str_split(x, stringr::fixed("\n")))
#   if (length(color_main))   . <-  color_main(.)
#   if (length(color_prefix)) . <-  color_prefix(.)
#   . <- paste0(prefix, .)
#   .
# }



#' Print with prefix
#'
#' @param x value to print
#' @param verbose_prefix prefix to be added after every new-line symbol
#'
#' @return
#'
#' @examples
cat_with_prefix <- function(x, prefix = "", sep = "\n", ...) {
  cat(
    add_prefix(x, prefix = prefix),
    sep = sep,
    ...
  )
}

# deparse expressions (if not already deparsed in "src" attribute)
deparse_nicely <- function(x, ...) {
  UseMethod("deparse_nicely", x)
}

deparse_nicely.expression <- function(x) {
  if (length(attr(x, "src"))) return(attr(x, "src")) else paste0(deparse(x, control = "useSource"), collapse = "\n")
}

deparse_nicely.default <- function(x) {
  as.character(x)
}

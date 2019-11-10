
# Generic job class -------------------------------------------------------


#' @export
job <- function(x, ...) {
  UseMethod("job", x)
}

#' @export
evaluate <- function(x, ...) {
  UseMethod("evaluate", x)
}


#' @export
evaluate.job <- function(x, ...) {
  warning("This an empty method!")
}

print.job <- function(x, ...) {
  warning("This an empty method!")
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


#' @export
read.job_file <- function(x, ...) {
  paste0(readLines(x$path, ...))
}


# job.character <- function(x, ...) {
#   do.call(paste0(x, "_job"), ...)
# }
#
# job.list <- function(x) {
#   do.call(job, x$type, x)
# }


# R jobs ------------------------------------------------------------------


#' @export
job.expression <- function(x, ...) {
  job_r(x, ...)
}

#' @export
job_r <- function(x, ...) {
  UseMethod("job_r", x)
}

#' @export
job_r.default <- function(x, ...) {
  job_r(as.expression(x), ...)
}

#' @export
job_r.expression <- function(x, ...) {
  job <-
    structure(
      list(
        r_expr = x,
        code   = deparse_nicely(x)
      ),
      class = c("job_r_expr", "job_r", "job"))

  job
}

#' @export
job_r.character <- function(x, file = FALSE, ...) {

  if (isTRUE(file)) return(job_r_file(x, ...))
  # else:

  job <-
    structure(
      list(
        r_expr = parse(text = x),
        code   = x
      ),
      class = c("job_r_expr", "job_r", "job"))

  job
}

#' @export
evaluate.job_r_expr <- function(x, ...) {
  eval(x$r_expr, ...)
}


#' @export
job_r_file <- function(x, ...) {
  struct <- job_file(x, ...)
  class(struct) <- c("job_r_file", "job_r", "job_file", class(struct))
}

#' @export
evaluate.job_r_file <- function(x, ...) {
  source(x$path, ...)
}


# SQL jobs ----------------------------------------------------------------

#' @export
job_sql_code <- function(x, mode = "execute", ...) {

}

#' @export
job_sql_file <- function(x, mode = "execute", ...) {
  struct <- job_file(x, mode = mode, ...)
  class(struct) <- c("job_sql_file", "job_sql", "job_file", class(struct))
}



# Python jobs -------------------------------------------------------------

#' @export
job_python <- function(x, ...) {
  UseMethod("job_python", x)
}


#' @export
job_python.character <- function(x, file = FALSE, ...) {
  warning("Python is not supported yet")
}


#' @export
job_python_file <- function(x, ...) {
  struct <- job_file(x, ...)
  class(struct) <- c("job_python_file", "job_python", class(struct))
}


#' @export
evaluate.job_python <- function(x, ...) {
  result <- NULL
  reticulate::py_run_string(x$src)
  try(result <- py$result)

  return(result)
}


#' @export
evaluate.job_python_file <- function(x, ...) {
  # reticulate::source_python('add.py')
  result <- NULL
  reticulate::py_run_file(x$fp)
  try(result <- py$result)

  return(result)
}


# Misc tools --------------------------------------------------------------------


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
#' \dontrun{
#' as_r_expr("{\n1+1\n}")
#' }
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

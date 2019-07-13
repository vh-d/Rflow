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
  r_job(r_expr = x)
}

r_job <- function(
  r_expr = NULL,
  r_code = NULL,
  r_file = NULL
) {

  job <- structure(list(r_expr = NULL, mode = NULL), class = c("r_job", "job"))

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
expression_r <- function(...){
  # simulate behaviour of expression()
  exprs <- match.call(expand.dots = TRUE)
  exprs[1] <- expression(expression)
  exprs <- eval(exprs)

  # references to sourcefile would make expressions (and its hashes) context dependent
  attr(exprs[[1]], "srcref") <- NULL
  attr(exprs[[1]], "srcfile") <- NULL
  attr(exprs[[1]], "wholeSrcref") <- NULL

  return(exprs)
}

# debug(expression_r)


sql_job <- function(sql = NULL, sql_code = NULL, mode = "execute") {

}

# job(expression({1+b}))


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

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
expression_r <- function(x){
  # simulate behaviour of expression() (but accept only one argument)
  exprs <- match.call(expand.dots = TRUE)
  exprs[1] <- expression(expression)
  exprs <- eval(exprs)
  
  return(as_r_expr(r_expr = exprs))
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


strip_srcrefs <- function(expr) {
  attr(expr[[1]], "srcref")      <- NULL
  attr(expr[[1]], "srcfile")     <- NULL
  attr(expr[[1]], "wholeSrcref") <- NULL
  
  expr
}

#' Returns R expression from either R expression or parsed R code
#'
#' @param r_code character vector of R code; ignored when R expression is not NULL
#' @param r_expr optional R expression
#'
#' @return
#' @export
#'
#' @examples
as_r_expr <- function(r_code = NULL, r_expr = NULL) {
  
  if (length(r_expr)) {
    
    if (length(attr(r_expr, "src"))) return(r_expr)
    # else:
    return(
      structure(
        strip_srcrefs(r_expr), 
        src = paste0(deparse(r_expr, control = "useSource"), collapse = "\n")
      )
    )
    
  } else if (length(r_code)) {
    
    return(
      structure(
        parse(text = r_code), 
        src = r_code
      )
    )
    
  } # else if (legnth(r_file))
  
  # else:
  warning("Either R expression or R code has to be supplied! Returning empty expression.")
  return(structure(expression(), src = ""))
  
}

#' Pretty printing of R expressions
#'
#' @param r_expr
#' @param verbose_prefix
#'
#' @return
#'
#' @examples
print_with_prefix <- function(x, verbose_prefix = "") {
  eol <- paste0("\n", crayon::white(verbose_prefix))
  
  r_expr_s2 <-
    stringr::str_replace_all(
      string      = x,
      pattern     = stringr::fixed("\n"),
      replacement = eol
    )
  
  cat(verbose_prefix, crayon::cyan(r_expr_s2), "\n", sep = "")
}


deparse_nicely <- function(x, ...) {
  UseMethod("deparse_nicely", x)
}

deparse_nicely.expression <- function(x) {
  if (length(attr(x, "src"))) return(attr(x, "src")) else paste0(deparse(x, control = "useSource"), collapse = "\n")
}

deparse_nicely.NULL <- function(x) {
  "" # or pass NULL?
}

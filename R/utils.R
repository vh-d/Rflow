
#' @title `%in%` equivalent for R6 objects
#'
#' @description Help function for matching R6 objects
#'
#' @param obj R6 object
#' @param objs vector of R6 objects
#' @export
in.R6 <- function(obj, objs) {
  any(sapply(objs, identical, y = obj))
}


union.list <- function(
  x, y, ...
)
{
  if (length(x) == 0)
    return(y)
  if (length(y) == 0)
    return(x)
  i  = intersect(names(x), names(y))
  ii = setdiff(  names(y), names(x))
  # i = is.na(i)
  if (length(i))  x[i]  <- y[i]
  if (length(ii)) x[ii] <- y[ii]

  x
}


#' companion to `isTRUE()``
#' @rdname isNotTRUE
isNotTRUE  <- function(x) (!length(x) || is.na(x) || x == FALSE)
#' @rdname isNotTRUE
isNotFALSE <- function(x) (!length(x) || is.na(x) || x == TRUE)

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
    return(r_expr)
  } else {
    if (length(r_code)) {
      return(parse(text = r_code))
    } else {
      warning("Either R expression or R code has to be supplied! Returning empty expression.")
      return(expression())
    }
  }
}


#' Pretty printing of R expressions
#'
#' @param r_expr
#' @param verbose_prefix
#'
#' @return
#'
#' @examples
cat_r_expr <- function(r_expr, verbose_prefix = "") {
  eol <- paste0("\n", crayon::white(verbose_prefix))

  r_expr_s1 <-
    stringr::str_replace_all(
      string      = as.character(r_expr),
      pattern     = stringr::fixed("\\n"),
      replacement = "\n"
    )

  r_expr_s2 <-
    stringr::str_replace_all(
      string      = r_expr_s1,
      pattern     = stringr::fixed("\n"),
      replacement = eol
    )

  cat(verbose_prefix, crayon::cyan(r_expr_s2),"\n", sep = eol)
}


# Print a nice notifications
notify_update <- function(id, property) {
  cat(crayon::yellow("Definition of "), crayon::red(id), crayon::yellow(" has changed (", property, ")", sep = ""), "\n", sep = "")
}

# Print a nice notifications
notify_trigger <- function(id, trigger, verbose_prefix = "") {
  cat(verbose_prefix, crayon::red(id), crayon::yellow(" triggered by ", trigger, sep = ""), "\n", sep = "")
}


escape_quotes <- function(x) {
  # x <- stringr::str_replace_all(string = x, pattern = stringr::fixed("\\n"), replacement = "\n")
  x <- stringr::str_replace_all(string = x, pattern = stringr::fixed("'"),   replacement = "\\'")

  return(x)
}

deescape_quotes <- function(x) {
  # x <- stringr::str_replace_all(string = x, pattern = stringr::fixed("\\n"), replacement = "\n")
  x <- stringr::str_replace_all(string = x, pattern = stringr::fixed("\\'"),   replacement = "'")

  return(x)
}


sql_structure <- function(x, ...) {
  UseMethod("sql_structure", x)
}

sql_structure.list <- function(x, ignoreErrors = FALSE, ...) {
  lapply(x, union.list, x = list(ignoreErrors = ignoreErrors, ...))
}

sql_structure.character <- function(x, ignoreErrors = FALSE, ...) {
  sql_structure(lapply(x, function(y) list(list(code = y))), ...)
}

paste_sql <- function(x) {
  paste0(x, collapse = ";\n\n")
}


null2na <- function(x) if (is.null(x)) return(NA) else x


firstnotnull <- function(...) {
  i <- sapply(list(...), is.null)

  return(list(...)[[min(which(!i))]])
}


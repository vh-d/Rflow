
#' Filter with expressions
#'
#' @param x a vector of data (typically a list of similar data objects)
#' @param expr expression passed to with()
#' @param ... passed to with()
#'
#' @return
#' @export
#'
#' @examples
#' @rdname lapplyWith
FilterWith <- function(x, ...) {
  ind <- as.logical(unlist(lapply(X = x, FUN = with, ...)))
  x[which(ind)]
}


#' @export
#' @rdname lapplyWith
lapplyWith <- function(x, ...) {
  lapply(X = x, FUN = with, ...)
}

#' @export
#' @rdname lapplyWith
sapplyWith <- function(x, ...) {
  sapply(X = x, FUN = with, ...)
}


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


# Print a nice notifications
notify_update <- function(id, property) {
  cat(crayon::yellow("Definition of "), crayon::red(id), crayon::yellow(" has changed (", property, ")", sep = ""), "\n", sep = "")
}

notify_removal <- function(id, verbose_prefix = "") {
  cat(verbose_prefix, crayon::red(id), ": removing the target!\n", sep = "")
}

notify_nonexistence <- function(id, verbose_prefix = "") {
  cat(verbose_prefix, crayon::red(id), ": the target does not exist!\n", sep = "")
}

notify_trigger <- function(id, trigger, verbose_prefix = "") {
  cat(verbose_prefix, crayon::red(id), crayon::yellow(" triggered by ", trigger, sep = ""), "\n", sep = "")
}

notify_invalid <- function(id, verbose_prefix = "", validator_names, validator_signals) {
  cat(
    verbose_prefix,
    crayon::red(id), crayon::yellow(" did not passed validations! (",
                                    paste0(paste0(validator_names, "(", validator_signals, ")"), collapse = ", ") , ")"
    ),
    "\n", sep = "")
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


escape_special <- function(x) {
  x -> .
  . <- gsub("\n", "\\n", ., fixed = TRUE)
  . <- gsub("\t", "\\t", ., fixed = TRUE)

  .
}

#' @export
sys_call_formatted <- function(which = 1) {
  . <- sys.call(which = 1)
  . <- as.expression(.)
  . <- as.character(.)
  . <- paste0(., collapse = "")
  . <- escape_special(.)

  .
}


sql_structure <- function(x, ...) {
  UseMethod("sql_structure", x)
}

sql_structure.list <- function(x, ignoreErrors = FALSE, ...) {
  lapply(x, union.list, x = list(ignoreErrors = ignoreErrors, ...))
}

sql_structure.character <- function(x, ignoreErrors = FALSE, ...) {
  sql_structure(lapply(x, function(y) list(code = y)), ...)
}

paste_sql <- function(x) {
  paste0(sapply(x, `[[`, "code"), collapse = ";\n\n")
}


null2na <- function(x) if (is.null(x)) return(NA) else x


firstnotnull <- function(...) {
  # if (is.null(substitute(...))) return(NULL)
  i <- sapply(list(...), is.null)
  if (any(!i)) return(list(...)[[min(which(!i))]]) else NULL
}


methods_for <- function(x, all = TRUE) {

  if (isTRUE(all)) {
    classes <- unique(c(class(x), typeof(x)))
  } else classes <- class(x)[1]

  res <- character()
  for (i in seq_along(classes)) {
    res <- c(res, methods(class = classes[i]))
  }

  unique(res)
}


#' Substitute ... for a list of un-evaluated expressions
#'
#' @return a list of all arguments matched by ...
#' @export
substitute... <- function() {
  pcall <- sys.call(which = -1)
  pargs <- names(formals(sys.function(-1)))
  if (!"..." %in% pargs) stop("The parent function does not have ... in its formals.")
  dotsnames <- setdiff(names(pcall)[-1], c("", setdiff(pargs, "...")))
  dots <- as.list(pcall)[dotsnames]

  return(dots)
}
#
# g <- function(x = 1, ...) {
#   # print(sys.call())
#   substitute...()
# }
#
# f <- function() {
#   result <- g(a = 1+1)
#
#   result
# }
#
# f()

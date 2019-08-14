
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
  sql_structure(lapply(x, function(y) list(code = y)), ...)
}

paste_sql <- function(x) {
  paste0(sapply(x, `[[`, "code"), collapse = ";\n\n")
}


null2na <- function(x) if (is.null(x)) return(NA) else x


firstnotnull <- function(...) {
  i <- sapply(list(...), is.null)

  return(list(...)[[min(which(!i))]])
}


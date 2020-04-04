#' Wrapper for DBI connection
#' @param drv passed to DBI::dbConnect
#' @param ... optional args stored unevaluated and later evaluated just in time before passing to DBI::dbConnect
#'
#' @export
dbi <- function(drv, ...) {
  UseMethod("dbi", drv)
}

#' @export
dbi.DBIDriver <- function(drv, ...) {

  x <- new.env()
  x[["drv"]]  <- drv
  x[["args"]] <- substitute...()

  class(x) <- c("dbi", "environnent")
  x
}

#' @export
connect <- function(x, ...) {
  UseMethod("connect", x)
}

#' @export
disconnect <- function(x) {
  UseMethod("disconnect", x)
}

#' @export
isValid <- function(x) {
  UseMethod("isValid", x)
}

#' Wrapper of DBI::dbConnect
#' @param x a dbi object
#' @param ...
#' @export
connect.dbi <- function(x, ...) {
  x[["conn"]] <- do.call(DBI::dbConnect, args = c(x[["drv"]], lapply(x[["args"]], eval)))
  invisible(x)
}


#' Wrapper of DBI::dbDisconnect
#' @param x a dbi object
#'
#' @export
disconnect.dbi <- function(x) {
  DBI::dbDisconnect(x[["conn"]])
}

#' Wrapper of DBI::dbIsValid
#' @param x a dbi object
#' @export
isValid.dbi <- function(x) {
  length(x[["conn"]]) && is(x[["conn"]], "DBIObject") && DBI::dbIsValid(x[["conn"]])
}

#' @export
print.dbi <- function(x) {
  cat("<dbi>")
  print(x[["drv"]])
  cat("  connected: ", isvalid.dbi(x), "\n", sep = "")
  if (length(x[["args"]])) print(x[["args"]])
}

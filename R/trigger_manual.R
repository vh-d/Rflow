#' Obtain value/object represented by a node
#'
#' @param x a node or node's id
#' @param rflow rflow object
#'
#' @return Value/object represented by given node.
#' @export
trigger_manual <- function(x, ...) {
  if (length(x) > 1) {
    sapply(x, trigger_manual, ...)
  } else {
    UseMethod("trigger_manual", x)
  }
}

#' @method trigger_manual node
#' @export
trigger_manual.node <- function(x) {
  x$trigger_manual <- TRUE
}

#' @method trigger_manual character
#' @export
trigger_manual.character <- function(x, rflow) {
  rflow[[x$id]]$trigger_manual <- TRUE
}

#' @method trigger_manual rflow
#' @export
trigger_manual.rflow <- function(rflow, x) {
  rflow[[x$id]]$trigger_manual <- TRUE
}

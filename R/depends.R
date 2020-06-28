
#' List all dependencies recursively
#'
#' @param x a node object
#'
#' @return vector of nodes' ids
#' @export
depends <- function(x, ...) {
  UseMethod("depends", x)
}

#' List all dependencies recursively
#'
#' @param x a node object
#' @param inverse FALSE (default) => downtream, TRUE => upstream
#' @param results DO NOT USE
#' @param ... ignored
#' @return vector of nodes' ids
#' @export
depends.node <- function(x, inverse = FALSE, results = character(), ...) {
  direction <- if (isTRUE(inverse)) "upstream" else "downstream"

  if (!length(x[[direction]])) {
    return(structure(list(), class = c("node_list", "list")))
  } else {
    return(
      structure(
        c(
          results,
          x[[direction]],
          unlist(sapply(x[[direction]], depends, inverse = inverse))
        ),
        class = c("node_list", "list")
      )
    )
  }
}



`[.node_list` <- function(x, ...) {
  structure(NextMethod("[", x), class = c("node_list", "list"))
}

#' @export
c.node_list <- function(...) {
  structure(c(...), class = c("node_list", "list"))
}

#' @export
print.node_list <- function(x, ...) {
  cat("<list of nodes>\n")
  print(get_id(x), ...)
}


#' @export
as.list.node_list <- function(x) {
  y <- x
  class(y) = "list"

  y
}

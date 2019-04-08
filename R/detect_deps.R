# ns <- nodes(RF)$id
# expr <-RF$DB1.tab1$r_expr)
#
# stringr::str_extract(string = expr, pattern = stringr::fixed(ns))

which.name <- function(x) names(which(x))

detect_deps <- function(x, ...) {
  UseMethod("detect_deps", x)
}

detect_deps.character <- function(x, node_names) {
  which.name(sapply(node_names, grepl, x = x, fixed = TRUE))
}

detect_deps.expression <- function(x, node_names) {
  detect_deps(paste0(deparse(x), collapse = ""), node_names = node_names)
}


#' Verify consistency of user-declared dependencies with node's behaviour
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
verify_dependencies <- function(x) {
  UseMethod("verify_dependencies", x)
}

#' @export
verify_dependencies.node <- function(x) {
  found  <- detect_deps(x$r_expr, node_names = nodes(parent.env(x))$id)
  stated <- x$depends
  
  lacks <- setdiff(found, stated)
  extra <- setdiff(stated, found)
  
  if (!length(unique(lacks, extra))) {
    return(TRUE)
  } else {
    if (length(lacks)) 
      warning(x$id, ": might be dependent on ", paste0(lacks, collapse = ", "))
    if (length(extra)) 
      warning(x$id, ": does not seem to be dependent on ", paste0(extra, collapse = ", "))
  }
  return(FALSE)
}

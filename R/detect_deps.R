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

check_dependencies <- function(x) {
  UseMethod("check_dependencies", x)
}

check_dependencies.node <- function(x) {
  found <- detect_deps(x$r_expr, node_names = Rflow::nodes(parent.env(x))$id)
  is_ok <- found %in% x$depends

  if (!isTRUE(all(is_ok))) {
    warning(x$id, " might be dependent on ", paste0(found[!is_ok], collapse = ", "))
    return(FALSE)
  } else
    return(TRUE)
}

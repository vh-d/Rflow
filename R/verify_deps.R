# ns <- nodes(RF)$id
# expr <-RF$DB1.tab1$r_expr)
#
# stringr::str_extract(string = expr, pattern = stringr::fixed(ns))

which.name <- function(x) names(which(x))

detect_deps <- function(x, ...) {
  UseMethod("detect_deps", x)
}

detect_deps.NULL <- function(x, node_names) {
  character()
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
  found  <- if (!length(x$r_expr)) detect_deps(x$r_expr, node_names = nodes(parent.env(x))$id) else character()
  stated <- if (!length(x$r_expr)) x$depends else character()

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


#' @export
verify_dependencies.rflow <- function(x) {
  sapply(get_nodes(x), verify_dependencies.node)
}


#' Detect dependencies
#'
#' @param x
#' @param envir
#' @param space
#'
#' @return
#' @export
#'
#' @examples
detect_nodes <- function(x, envir = parent.frame(), space = "", st = 0, stmax = 20) {

  if (st > stmax) return("stop")

  if (any(class(x) %in% c("<-", "{", "(", "call", "language", "expression"))) {
    cat(space, class(x), ":\n")
    return(sapply(x, detect_nodes, envir = envir, space = paste0("   ", space), st = st + 1))
  }

  if (is.function(x) & !is.primitive(x)) {
    cat(space, class(x), ":\n")
    return(detect_nodes(body(x), envir = envir, space = paste0("   ", space), st = st + 1))
  }

  if (is.name(x)) {
    xname <- as.character(x)
    cat(space, xname, ":\n")
    if (exists(xname, where = envir) && !(xname %in% c("::"))) { # do not detect_nodes into external packages for now
      xx <- get(xname)
      if (length(xx) && !is.name(xx)) {
        return(detect_nodes(xx, envir = envir, space = paste0("   ", space), st = st + 1))
      } else return(NULL)
    }
  }

  if (is(x, "node")) return(x$id)

  cat(space, class(x), ":\n")
  return(NULL)
}


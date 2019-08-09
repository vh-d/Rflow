# ns <- nodes(RF)$id
# expr <-RF$DB1.tab1$r_expr)
#
# stringr::str_extract(string = expr, pattern = stringr::fixed(ns))

which.name <- function(x) names(which(x))

detect_deps <- function(x, ...) {
  UseMethod("detect_deps", x)
}

detect_deps.NULL <- function(x, node_ids) {
  character()
}

detect_deps.character <- function(x, node_ids) {
  which.name(sapply(node_ids, grepl, x = x, fixed = TRUE))
}

detect_deps.expression <- function(x, node_ids) {
  detect_deps(paste0(deparse(x), collapse = ""), node_ids = node_ids)
}


#' Verify consistency of user-declared dependencies with node's behaviour
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
verify_dependencies <- function(x, ...) {
  UseMethod("verify_dependencies", x)
}

#' @export
verify_dependencies.node <- function(x, method = c("ast", "grep"), maxdepth = 20L, verbose = FALSE) {

  method <- match.arg(method)

  found  <- if (length(x$r_expr))
    switch(
      method,
      grep = detect_deps(x$r_expr, node_ids = nodes(parent.env(x))$id),
      ast = detect_nodes(x$r_expr, rflow = parent.env(x), depthmax = maxdepth, verbose = verbose)
    ) else character()

  stated <- if (length(x$depends)) x$depends else character()

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
verify_dependencies.rflow <- function(x, ...) {
  sapply(get_nodes(x), verify_dependencies.node, ...)
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
detect_nodes <- function(x, rflow, found = c(), space = "", depth = 0L, depthmax = 20L, verbose = FALSE) {

  # folling convention of referencing Rflow objects
  .RFLOW = rflow

  # (poor) protection against recursion
  if (depth > depthmax) return()

  # these objects whould be recursively iterated
  if (any(class(x) %in% c("<-", "{", "(", "call", "language", "expression"))) {
    if (verbose) cat(space, class(x), ":\n")

    if (
      is.call(x)
      && (as.character(x[[1]]) %in% c("[[", "$"))
      && length(x) > 1
      && exists(as.character(x[[2]]))
    ) {
      xv <- get(as.character(x[[2]]))
      if (is.environment(xv) && identical(xv, rflow))
        if (depth == 0L) return(unique(unlist(c(found, as.character(x[[3]]))))) else return(c(found, as.character(x[[3]])))
    }

    if (depth == 0L)
      return(unique(unlist(c(found, sapply(x, detect_nodes, rflow = rflow, found = found, space = paste0("   ", space), depth = depth + 1))))) else
        return(c(found, sapply(x, detect_nodes, rflow = rflow, found = found, space = paste0("   ", space), depth = depth + 1)))
  }

  # dive into function bodies except primitives
  if (is.function(x)) {
    if (is.primitive(x)) return(NULL)

    if (verbose) cat(space, class(x), ":\n")
    if (depth == 0L)
      return(unique(unlist(c(found, sapply(x, detect_nodes, rflow = rflow, found = found, space = paste0("   ", space), depth = depth + 1))))) else
        return(c(found, sapply(x, detect_nodes, rflow = rflow, found = found, space = paste0("   ", space), depth = depth + 1)))
  }

  # existing obejcts can be subjet to further exploration
  if (is.name(x)) {
    xname <- as.character(x)
    if (verbose) cat(space, xname, ":\n")
    if (xname != "" && exists(xname) && !(xname %in% c("::", "{"))) { # do not detect_nodes into external packages for now
      xx <- get(xname)
      if (length(xx) && !is.name(xx)) {
        if (depth == 0L)
          return(unique(unlist(c(found, detect_nodes(xx, rflow = rflow, found = found, space = paste0("   ", space), depth = depth + 1))))) else
            return(c(found, detect_nodes(xx, rflow = rflow, found = found, space = paste0("   ", space), depth = depth + 1)))
      }
    } else return(NULL)
  }

  # node objects can be imediately reported as dependencies
  if (is(x, "node")) {
    if (depth == 0L)
      return(unique(unlist(c(found, x$id)))) else
        return(c(found, x$id))
  }

  if (verbose) cat(space, class(x), ":\n")
  return(NULL)
}


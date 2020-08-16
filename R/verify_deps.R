# ns <- as_data_table_nodes(RF)$id
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
#' @param x a node or rflow object
#'
#' @export
verify_dependencies <- function(x, ...) {
  UseMethod("verify_dependencies", x)
}

#' @export
verify_dependencies.node <- function(x, method = c("ast", "grep"), maxdepth = 20L, verbose = FALSE, debugging = FALSE) {

  method <- match.arg(method)

  found  <- if (length(x$r_expr))
    switch(
      method,
      grep = detect_deps(x$r_expr, node_ids = as_data_table_nodes(parent.env(x))$id),
      ast  = detect_nodes(x$r_expr, rflow = parent.env(x), noderef = x, depthmax = maxdepth, verbose = verbose, debugging = debugging)
    ) else character()

  stated <- if (length(x$depends)) x$depends else character()

  lacks <- setdiff(found, stated)
  extra <- setdiff(stated, found)

  if (!length(unique(lacks, extra)))
    return(TRUE) else
      list(lacks = lacks, extra = extra)
}


#' @export
verify_dependencies.rflow <- function(x, ...) {
  lapply(nodes(x), verify_dependencies.node, ...)
}


#' Detect dependencies
#'
#' @param x a call/language/expression/{/... object
#' @param envir R environemnt
#' @param space character; prefix to be added when printing
#' @param depth DO NOT SET THIS ARG! (counter of the level how deep the recurent algo is currently entering)
#' @param depthmax limit on the depth to dive in
#' @param verbose logical; print output?
#'
#' @return detected nodes' id
#' @export
detect_nodes <- function(x, rflow, noderef = NULL, found = c(), space = "", depth = 0L, depthmax = 20L, verbose = FALSE, debugging = FALSE) {

  # folling convention of referencing Rflow objects
  .RFLOW = rflow

  # simple protection against infinite recursion
  if (depth > depthmax) return()
  if (depth == 0L && verbose) cat("______________________________________\n")
  if (verbose) cat(space, "depth: ", depth, ":\n")

  # # node objects can be immediately reported as dependencies
  # if (methods::is(x, "node")) {
  #   if (depth == 0L)
  #     return(unique(unlist(c(found, x$id)))) else
  #       return(c(found, x$id))
  # }

  # these non-function objects will be recursively explored
  if (any(class(x) %in% c("<-", "=", "{", "(", "call", "language", "expression"))) {

    if (verbose) cat(space, as.character(class(x)), ":\n")

    # catch reserved references
    if (
      isTRUE(is.call(x))
      && isTRUE(length(x) > 1)
    ) {

      if (isTRUE(debugging)) browser()

      # .RFLOW, self, private
      if (isTRUE(as.character(x[[1]]) %in% c("[[", "$", "["))) {

        if (isTRUE(identical(as.character(x[[2]]), ".RFLOW"))) {
          node_id <- tryCatch(eval(x[[3]], envir = noderef$.__enclos_env__), error = function(e) {warnign(e);NULL})
          if (verbose) cat(space, "node: ", node_id, ":\n")
          if (depth == 0L) return(unique(unlist(c(found, node_id)))) else return(c(found, node_id))
        }

        if (identical(as.character(x[[2]]), c("$", "self", "depends")) || identical(as.character(x[[2]]), c("[[", "self", "depends"))) { # TODO: add support for `$`'s matching?
          node_id <- tryCatch(eval(x, envir = noderef$.__enclos_env__), error = function(e) {warnign(e);NULL})
          if (verbose) cat(space, "node: ", node_id, ":\n")
          if (depth == 0L) return(unique(unlist(c(found, node_id)))) else return(c(found, node_id))
        }

        if (identical(as.character(x[[2]]), c("$", "self", "upstream")) || identical(as.character(x[[2]]), c("[[", "self", "upstream"))) { # TODO: add support for `$`'s matching?
          node_id <- tryCatch(eval(x, envir = noderef$.__enclos_env__)[["id"]], error = function(e) {warnign(e);NULL})
          if (verbose) cat(space, "node: ", node_id, ":\n")
          if (depth == 0L) return(unique(unlist(c(found, node_id)))) else return(c(found, node_id))
        }
      }

      # .DATA() and .NODES()
      if (identical(as.character(x[[1]]), ".DATA") || identical(as.character(x[[1]]), ".NODE")) {
        node_id <- tryCatch(eval(x[[2]], envir = noderef$.__enclos_env__), error = function(e) {warnign(e);NULL})
        if (verbose) cat(space, "node: ", node_id, ":\n")
        if (depth == 0L) return(unique(unlist(c(found, node_id)))) else return(c(found, node_id))
      }

      # catch ::
      if (identical(as.character(x[[1]]), "::")) {
        x <- x[3]
      }
    }

    # otherwise
    if (verbose) cat(space, "special object: ", as.character(x), ":\n")
    if (depth == 0L)
      return(unique(unlist(c(found, sapply(x, detect_nodes, rflow = rflow, noderef = noderef, found = found, space = paste0("   ", space), depth = depth + 1, verbose = verbose, debugging = debugging))))) else
        return(c(found, sapply(x, detect_nodes, rflow = rflow, noderef = noderef, found = found, space = paste0("   ", space), depth = depth + 1, verbose = verbose, debugging = debugging)))

  }

  # dive into function bodies except primitives and attached packages
  if (is.function(x)) {
    if (verbose) cat(space, "function", ":\n")
    if (is.primitive(x)) {
      if (verbose) cat(space, "primitive... skipping", "\n")
      return(NULL)
    }
    if (isNamespace(environment(x))) {
      if (verbose) cat(space, "from a package... skipping", "\n")
      return(NULL)
    }
    if (identical(x, `[`) || identical(x, `$`)) {

    }
    if (depth == 0L)
      return(unique(unlist(c(found, sapply(x, detect_nodes, rflow = rflow, noderef = noderef, found = found, space = paste0("   ", space), depth = depth + 1, verbose = verbose, debugging = debugging))))) else
        return(c(found, sapply(x, detect_nodes, rflow = rflow, noderef = noderef, found = found, space = paste0("   ", space), depth = depth + 1, verbose = verbose, debugging = debugging)))
  }

  # existing objects can be subject to further exploration
  if (is.name(x)) {
    xname <- as.character(x)
    if (verbose) cat(space, " name: '", xname, "':\n", sep = "")
    if (xname != "" && exists(xname, envir = noderef$.__enclos_env__) && !(xname %in% c("::", "{"))) { # do not detect_nodes into external packages for now
      xx <- get(xname, envir = noderef$.__enclos_env__)
      if (!is.name(xx) && length(length(xx)) && length(xx)) { # what classes/types does not have a length method?
        if (depth == 0L)
          return(unique(unlist(c(found, detect_nodes(xx, rflow = rflow, noderef = noderef, found = found, space = paste0("   ", space), depth = depth + 1, verbose = verbose, debugging = debugging))))) else
            return(c(found, detect_nodes(xx, rflow = rflow, noderef = noderef, found = found, space = paste0("   ", space), depth = depth + 1, verbose = verbose, debugging = debugging)))
      }
    } else return(NULL)
  }

  if (typeof(x) %in% c("special", "builtin")) {
    if (verbose) cat(space, "special:", class(x), ":\n")
    return(NULL)
  }

  if (verbose) cat(space, "!!!: class of", class(x), "/ type of", typeof(x), ":\n")
  if (verbose) cat(space, str(x))
  if (isTRUE(debugging)) browser()

  return(NULL)
}


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
detect_nodes <- function(x, rflow, found = c(), space = "", depth = 0L, depthmax = 20L) {
  
  # folling convention of referencing Rflow objects
  .RFLOW = rflow
  
  # (poor) protection against recursion
  if (depth > depthmax) return()
  
  # these objects whould be recursively iterated
  if (any(class(x) %in% c("<-", "{", "(", "call", "language", "expression"))) {
    cat(space, class(x), ":\n")
    
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
    
    cat(space, class(x), ":\n")
    if (depth == 0L) 
      return(unique(unlist(c(found, sapply(x, detect_nodes, rflow = rflow, found = found, space = paste0("   ", space), depth = depth + 1))))) else 
        return(c(found, sapply(x, detect_nodes, rflow = rflow, found = found, space = paste0("   ", space), depth = depth + 1)))
  }
  
  # existing obejcts can be subjet to further exploration
  if (is.name(x)) {
    xname <- as.character(x)
    cat(space, xname, ":\n")
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

  cat(space, class(x), ":\n")
  return(NULL)
}



# tools -------------------------------------------------------------------


# construct/initiate a node object

#' instantiate a node
#' @export
as_node <- function(x, ...) {
  UseMethod("as_node", x)
}

#' @export
as_node.list <- function(
  x,
  type = if (!is.null(x$type)) x$type else "r_node",  # if not given type is r_node by default
  ... # other arguments to the initialize() constructor
) {
  do.call(get(type)$new, args = c(x, list(...)))
}

#' @export
as_node.node <- function(x) {
  x
}



#' extract id from a object's definition (in a list)
#'
#' @param obj object definition
#' @export
get_id <- function(obj) {
  id <- if (length(obj$id)) obj$id else paste0(obj$env, ".", obj$name)
  return(id)
}


# @param x
# @param id_old
# @param id_new
# @param ...
#
# @usage change_id.rflow(obj, id_old, id_new, ...)
# @export
set_id <- function(x, ...) {
  UseMethod("change_id", x)
}

# @export
set_id.node <- function(x, id_new, ...) {
  x$set_id(id_new)
}


#' Make a vector of all nodes' ids
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
get_nodes_ids <- function(x) {
  UseMethod("get_nodes_ids", x)
}

#' @export
get_nodes_ids.rflow <- function(x) {
  node_objs <- get_nodes(x)
  sapply(node_objs, function(x) x$id)
}

#' Returns a list with all node objects
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
get_nodes <- function(x) {
  UseMethod("get_nodes", x)
}

#' @export
get_nodes.rflow <- function(x) {
  mget(ls(x), envir = x)
}


#' Obtain value/object represented by a node
#'
#' @param x a node or node's id
#' @param rflow rflow object
#'
#' @return Value/object represented by given node.
#' @export
get_value <- function(x, ...) {
  UseMethod("get_value", x)
}

#' @method get_value character
#' @export
get_value.character <- function(x, rflow) {
  rflow[[x]]$get()
}

#' @method get_value node
#' @export
get_value.node <- function(x) {
  x$get()
}

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
  rflow[[id]]$trigger_manual <- TRUE
}

#' @method trigger_manual rflow
#' @export
trigger_manual.rflow <- function(rflow, x) {
  rflow[[id]]$trigger_manual <- TRUE
}

# evaluates/build a single node assuming requirements are ready
#' Title
#'
#' @param id node's id
#' @param ...
#'
#' @export
eval_node <- function(id, ...) {
  UseMethod("eval_node", id)
}


#' @export
eval_node.character <- function(id, rflow) {
  rflow[[id]]$eval()
}


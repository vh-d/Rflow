
#' Initialize a new DAG
#'
#' @param path path to folder with nodes' definitions
#'
#' @return A new rflow object.
#' @export
#'
#' @examples
#' RF <- new_rflow()
new_rflow <- function(
  path = NULL
) {
  result <- new.env()
  class(result) <- c("rflow", class(result))

  if (length(path)) {
    result[[".def_path"]] <- path

    persistence_path <- file.path(path, ".rflow", "persistence")
    if (!dir.exists(persistence_path)) dir.create(persistence_path, recursive = TRUE)
    .persistence <- list(
      enabled = TRUE,
      path    = persistence_path
    )

    cache_path <- file.path(path, ".rflow", "cache")
    if (!dir.exists(cache_path)) dir.create(cache_path, recursive = TRUE)
    .cache <- list(
      enabled = TRUE,
      path    = cache_path
    )

  } else {
    .persistence <- list(enabled = FALSE)
    .cache       <- list(enabled = FALSE)
  }

  result[[".persistence"]] <- .persistence
  result[[".cache"]]       <- .cache

  return(result)
}


#' convert DAG to an igraph object
#' @param x rflow object
#' @export
as_igraph <- function(x, ...) {
  UseMethod("as.igraph", x)
}

#' @export
as_igraph.rflow <- function(rflow) {
  if (!requireNamespace("igraph")) stop("igraph package required")

  E <- edges(rflow)
  G <- igraph::graph_from_data_frame(E)

  G
}

check_rflow_dag <- function(rflow) {
  G <- as_igraph.rflow(rflow)
  igraph::is_dag(G)
}

#' Remove all nodes from an rflow object
#'
#' @param rflow
#'
#' @export
clean_rflow <- function(rflow) {
  remove(list = ls(envir = rflow, all.names = FALSE), envir = rflow)
}


#' Clean cache folder
#'
#' @param x
#' @param ...
#'
#' @export
clean_cache <- function(x) {
  UseMethod("clean_cache", x)
}

#' @export
clean_cache.rflow <- function(rflow) {

  # warn if not defined and return
  if (!length(rflow$.cache$path)) {
    warning("Cache dir not defined!")
    return(TRUE)
  }

  # warn if nonexistet and return
  if (dir.exists(rflow$.cache$path)) {
    as.logical(
      1-unlink(
        file.path(rflow$.cache$path, "*"),
        recursive = FALSE)
    )
  } else {
    warning("Cache dir does not exists")
    return(TRUE)
  }
}

#' Clean stored state folder
#'
#' @param x
#' @param ...
#'
#' @export
clean_persistence <- function(x, ...) {
  UseMethod("clean_persistence", x)
}

#' Clean stored state folder
#'
#' @param rflow
#' @export
clean_persistence.rflow <- function(rflow) {

  # warn if not defined and return
  if (!length(rflow$.persistence$path)) {
    warning("Persistence storage folder not defined!")
    return(TRUE)
  }

  # warn if nonexistet and return
  if (dir.exists(rflow$.persistence$path)) {
    as.logical(
      1-unlink(
        file.path(rflow$.persistence$path, "*"),
        recursive = FALSE)
    )
  } else {
    warning("Persistence storage folder does not exists")
    return(TRUE)
  }
}

#' batch load object definitions
#' @export
load_nodes <- function(x, ...) {
  UseMethod("load_nodes", x)
}


#' @param x an rflow object
#' @param conflict logical; How to resolve conflict when an object of the same id already exists in the rflow?
#' @param verbose logical; print verbose output?
#' @rdname load_nodes
#' @export
load_nodes.rflow <- function(
  x,
  conflict = "update", # overwrite existing objects
  verbose = TRUE
) {

  obj_defs <-
    load_node_definitions(
      path = x$.def_path,
      modified_since = x$.last_updated,
      verbose = verbose)

  if (length(obj_defs)) {
    res <- add_nodes(
      objs        = obj_defs,
      rflow       = x,
      conflict    = conflict,
      cache       = x$.cache$path,
      verbose     = verbose
    )
  } else {
    res <- NULL
  }

  x$.last_updated <- Sys.time()

  return(invisible(res))
}



#' load DAG's objects' definition from TOML files
#'
#' @param path path to folder with node definitions
#' @param modified_since
#' @param verbose logical; print verbose output?
#' @export
load_node_definitions <- function(path, modified_since = NULL, verbose = TRUE) {

  if (!requireNamespace("RcppTOML")) stop("Package RcppTOML not available!")

  obj_yaml_files <-
    list.files(path       = path,
               pattern    = "*.toml",
               full.names = TRUE)
  nf <- length(obj_yaml_files)

  if (length(modified_since)) {
    # only modified-since-last-update YAML definitions are loaded
    dtYAMLFILES <- rbindlist(lapply(obj_yaml_files, function(yf) c(file = yf, file.info(yf))))
    obj_yaml_files <- dtYAMLFILES[mtime > as.POSIXct(modified_since), file]
  }

  if (!length(obj_yaml_files)) {
    if (nf > 0) cat("All up to date...\n") else
      warning("Nothing to be loaded.")

    return(invisible(NULL))
  }

  obj_list <- vector(mode = "list", length = length(obj_yaml_files))

  # loop over all files listed
  for (i in 1:length(obj_list)) {
    if (verbose) cat("Loading from ", obj_yaml_files[i], "\n", sep = "")
    obj_list[[i]] <-
      tryCatch(
        RcppTOML::parseTOML(input = obj_yaml_files[i], escape = FALSE),
        error = function(e) {
          stop("File ", obj_yaml_files[i], " could not be loaded:\n    ", e)
        }
      )
  }

  return(obj_list)
}


#' loads object definition/state from a RDS file
#'
#' @param path path to RDS file with saved state
#' @param ...
load_state_of_node <- function(path, ...) {
  state_list <- readRDS(file = path, ...)

  return(
    c(
      state_list$private,
      state_list$self
    )
  )
}

#' batch loading objects' definition/state from RDS files
#'
#' @param path path to folder containg RDS files with saved state (typically .rflow subdir of rflow config dir)
#' @param recursive passed to list.files
#' @param ignore.case passed to list.files
#' @param ...
load_state_of_nodes <- function(path, recursive = FALSE, ignore.case = TRUE, ...) {

  # list all relevant files
  persistence_files <-
    list.files(path       = path,
               pattern    = "*.rds",
               full.names = TRUE,
               recursive  = recursive,
               ignore.case = ignore.case,
               include.dirs = FALSE)

  # load it into a list and return
  nodes_list <- lapply(persistence_files, load_state_of_node, ...)

  return(nodes_list)
}

#' Initialize a node and adds it to an rflow
#'
#' @param x
#' @param rflow an rflow object
#' @param ...
#' @param conflict logical; How to resolve conflict when an object of the same id already exists in the rflow?
#' @param persistence path to folder containg RDS files with saved state (typically .rflow subdir of rflow config dir)
#' @param connect logical;
#' @param verbose logical; print verbose output?
#'
#' @export
add_node <- function(x, ...) {
  UseMethod("add_node", x)
}

#' @export
add_node.list <- function(
  x,
  rflow,
  ...,
  conflict= "update",
  connect = FALSE,
  verbose = TRUE) {

  # extract object's id
  id <- get_id(x)

  if (verbose) cat("Adding node ", crayon::red(id), "\n", sep = "")

  # default behaviour
  updated   <- FALSE # when object already exists and is only going to be updated
  recovered <- FALSE # when object is being recovered from disk

  # check whether the object exists already
  if (id %in% names(rflow))
    switch(conflict,
           "update" = {
             updated <- TRUE
           },
           "skip" = {
             warning(id, " already exists! Skipping...")
             return(FALSE)
           },
           "overwrite" = warning(id, " already exists! Overwriting!"),
           NULL = stop(id, " already exists!")
    )

  # main

  # non-existent objects need to be constructed/initialized first
  # either completely new objects or objects being recovered
  if (!updated) {

    if (rflow$.persistence$enabled) {
      fp <- file.path(rflow$.persistence$path, paste0(id, ".rds"))

      if (file.exists(fp)) {
        saved_state   <- load_state_of_node(path = fp)
        initiated_obj <- as_node(saved_state, persistence = rflow$.persistence, ...)
        recovered     <- TRUE
      } else
        initiated_obj <- as_node(x, persistence = rflow$.persistence, ...)
    } else {
      initiated_obj <- as_node(x, persistence = rflow$.persistence, ...)
    }

    # assign reference
    assign(
      x     = id,
      value = initiated_obj,
      pos   = rflow)

    # link the two environments
    parent.env(rflow[[id]]) <- rflow
  }

  if (recovered || updated) update_node(x, rflow = rflow, verbose = verbose)

  # connect to required objects
  if (connect) rflow[[id]]$connect()

  result <- setNames(TRUE, id)

  return(result)
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

# @export
set_id.rflow <- function(x, id_old, id_new, ...) {
  rflow <- x

  rflow[[id_new]] <- rflow[[id_old]] # new reference
  rm(id_old, pos = rflow)

  set_id(rflow[[id_new]], id_new)
}


#' batch init objects
#'
#' @param objs list of nodes' definitions
#' @param rflow an rflow object
#' @param connect logical;
#' @param ...
#' @param verbose logical; print verbose output?
#' @export
add_nodes <- function(objs, rflow, connect = TRUE, ..., verbose = TRUE) {

  result <-
    lapply(
      X       = objs,
      FUN     = add_node,
      rflow   = rflow,
      verbose = verbose,
      ...)

  # connect objects after all are initiated
  if (connect) connect_nodes(rflow, verbose = max(0, verbose - 1L))

  return(unlist(result))
}

#' update object (call whenever definition in config file could have potentially changed)
#'
#' @param obj list with definition of node
#' @param rflow an rflow object
#' @param verbose logical; print verbose output?
#' @export
update_node <- function(
  obj,
  rflow,
  verbose = FALSE
) {

  # extract object's id
  id <- get_id(obj)

  # find the node that is going to be updated
  if (!(id %in% names(rflow))) stop("Object ", id, " not found!")

  # update
  do.call(rflow[[id]]$update_definition, args = c(obj, verbose = verbose))
}


#' Batch update nodes
#'
#' @param objs list of nodes' definitions
#' @param rflow an rflow object
#' @param verbose logical; print verbose output?
#'
#' @export
update_nodes <- function(objs, rflow, verbose = FALSE) {

  result <-
    lapply(
      X   = objs,
      FUN = update_node,
      rflow = rflow,
      verbose = verbose)

  return(unlist(result))
}


#' batch connect all objects
#'
#' @export
connect_nodes <- function(x, ...) {
  UseMethod("connect_nodes", x)
}

#' @export
connect_nodes.rflow <- function(rflow, ...) {
  lapply(rflow, function(x) x$connect(...))
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


# generic make function
#' Make/build target or multiple targets
#'
#' @param x node's id or rflow object
#' @param ...
#' @export
make <- function(x, ...){
  if (!is.null(x)) {
    UseMethod("make", x)
  } else stop(substitute(x), " cannot be NULL!")
}

#' @export
make.node <- function(x, verbose = TRUE, verbose_prefix = "") {
  x$make(verbose = verbose, verbose_prefix = verbose_prefix)
}

# recurrent procedure
#' @export
make.character <- function(id, rflow, verbose = TRUE, verbose_prefix = "") {
  if (length(id) < 2) {
    if (length(id) == 0) return(NULL)
    make(rflow[[id]], verbose = verbose, verbose_prefix = verbose_prefix)
  } else {
    sapply(id, make.character, verbose = verbose, verbose_prefix = verbose_prefix)
  }
}

#' @param rflow rflow object
#'
#' @param leaves_only logical; Option to run make only from ending nodes. Avoids redundant visits on intermediate nodes.
#' @param verbose logical; Print verbose output?
#'
#' @export
make.rflow <- function(rflow, leaves_only = TRUE, verbose = TRUE) {
  if (verbose) cat(rep("\u2500", 3), " Make ", rep("\u2500", 25), "\n\n", sep = "")

  if (leaves_only) {
    E <- edges(rflow)
    N <- nodes(rflow)
    nodes_to_make <- N[!E, on = c("id" = "from"), id]
  } else {
    nodes_to_make <- ls(rflow)
  }

  # RUN
  res <- sapply(
    X       = nodes_to_make,
    FUN     = make,
    rflow   = rflow,
    verbose = verbose
  )

  return(invisible(res))
}

#' @export
get.rflow <- function(rflow, id) {
  rflow[[id]]$get()
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


#' @export
get_value.character <- function(x, rflow) {
  rflow[[x]]$get()
}

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

#' @export
trigger_manual.node <- function(x) {
  x$trigger_manual <- TRUE
}

#' @export
trigger_manual.character <- function(x, rflow) {
  rflow[[id]]$trigger_manual <- TRUE
}

#' @export
trigger_manual.rflow <- function(rflow, x) {
  rflow[[id]]$trigger_manual <- TRUE
}

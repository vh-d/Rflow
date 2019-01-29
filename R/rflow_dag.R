
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

    .storage_path <- file.path(path, ".rflow", "state")
    if (!dir.exists(.storage_path)) dir.create(.storage_path, recursive = TRUE)
    result[[".storage_path"]] <- .storage_path

    .cache_store_path <- file.path(path, ".rflow", "cache")
    if (!dir.exists(.cache_store_path)) dir.create(.cache_store_path, recursive = TRUE)
    result[[".cache_store_path"]] <- .cache_store_path
  }

  return(result)
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
clean_cache <- function(x, ...) {
  UseMethod("clean_cache", x)
}

#' Clean cache folder
#'
#' @param rflow
clean_cache.rflow <- function(rflow) {

  # warn if not defined and return
  if (!length(rflow$.cache_store_path)) {
    warning("Cache dir not defined!")
    return(TRUE)
  }

  # warn if nonexistet and return
  if (dir.exists(rflow$.cache_store_path)) {
    as.logical(
      1-unlink(
        file.path(rflow$.cache_store_path, "*"),
        recursive = FALSE)
    )
  } else {
    warning("Cache dir does not exists")
    return(TRUE)
  }
}

#' @export
load_nodes <- function(x, ...) {
  UseMethod("load_nodes", x)
}

#' batch load object definitions
#'
#' @param rflow an rflow object
#' @param conflict logical; How to resolve conflict when an object of the same id already exists in the rflow?
#' @param verbose logical; print verbose output?
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
      storage     = x$.storage_path,
      cache_store = x$.cache_store_path,
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
  nodes_storage_files <-
    list.files(path       = path,
               pattern    = "*.rds",
               full.names = TRUE,
               recursive  = recursive,
               ignore.case = ignore.case,
               include.dirs = FALSE)

  # load it into a list and return
  nodes_list <- lapply(nodes_storage_files, load_state_of_node, ...)

  return(nodes_list)
}

#' Initialize a node and adds it to an rflow
#'
#' @param obj
#' @param rflow an rflow object
#' @param ...
#' @param conflict logical; How to resolve conflict when an object of the same id already exists in the rflow?
#' @param storage path to folder containg RDS files with saved state (typically .rflow subdir of rflow config dir)
#' @param connect logical;
#' @param verbose logical; print verbose output?
#'
#' @export
add_node <- function(
  obj,
  rflow,
  ...,
  conflict= "update",
  storage = NULL,   # path to saved state
  connect = FALSE,
  verbose = TRUE) {

  # extract object's id
  id <- get_id(obj)

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

    if (length(storage) && {fp <- file.path(storage, paste0(id, ".rds"));file.exists(fp)}) {
      saved_state   <- load_state_of_node(path = fp)
      initiated_obj <- as_node(saved_state, storage = storage, ...)
      recovered     <- TRUE
    } else {
      initiated_obj <- as_node(obj, storage = storage, ...)
    }

    # assign reference
    assign(
      x     = id,
      value = initiated_obj,
      pos   = rflow)

    # link the two environments
    parent.env(rflow[[id]]) <- rflow
  }

  if (recovered || updated) update_node(obj = obj, rflow = rflow, verbose = verbose)

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
  if (connect) connect_nodes(rflow)

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


#' @export
connect_nodes <- function(x, ...) {
  UseMethod("connect_nodes", x)
}

#' batch connect all objects
#'
#' @param rflow an rflow object
#' @export
connect_nodes.rflow <- function(rflow) {
  lapply(rflow, function(x) x$connect())
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
  UseMethod("make", x)
}

#' @export
make.node <- function(x, verbose = TRUE, verbose_prefix = "") {
  x$make(verbose = verbose, verbose_prefix = verbose_prefix)
}

# recurrent procedure
#' @export
make.character <- function(id, rflow, verbose = TRUE, verbose_prefix = "") {
  make(rflow[[id]], verbose = verbose, verbose_prefix = verbose_prefix)
}

#' @export
make.rflow <- function(rflow, verbose = TRUE) {
  if (verbose) cat(rep("\u2500", 3), " Make ", rep("\u2500", 25), "\n\n", sep = "")
  return(invisible(sapply(X = ls(rflow), FUN = make, rflow = rflow, verbose = verbose)))
}

#' @export
get.rflow <- function(rflow, id) {
  rflow[[id]]$get()
}

#' Obtain value represented by a node
#'
#' @param id node's id
#' @param rflow rflow object
#'
#' @return Value/object represented by given node.
#' @export
get_value <- function(id, rflow) {
  rflow[[id]]$get()
}


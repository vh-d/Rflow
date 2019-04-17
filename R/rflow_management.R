
#' Initialize a new DAG
#'
#' @param path path to rflow home folder. Nodes' definitions are searched for in this folder, cache and other data is saved into...
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
clean_cache <- function(x, ...) {
  UseMethod("clean_cache", x)
}

#' @method clean_cache rflow
#' @export
clean_cache.rflow <- function(x, ...) {

  # warn if not defined and return
  if (!length(x$.cache$path)) {
    warning("Cache dir not defined!")
    return(TRUE)
  }

  # warn if nonexistet and return
  if (dir.exists(x$.cache$path)) {
    as.logical(
      1-unlink(
        file.path(x$.cache$path, "*"),
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
#' @param x
#' @method clean_persistence rflow
#' @export
clean_persistence.rflow <- function(x) {

  # warn if not defined and return
  if (!length(x$.persistence$path)) {
    warning("Persistence storage folder not defined!")
    return(TRUE)
  }

  # warn if nonexistet and return
  if (dir.exists(x$.persistence$path)) {
    as.logical(
      1-unlink(
        file.path(x$.persistence$path, "*"),
        recursive = FALSE)
    )
  } else {
    warning("Persistence storage folder does not exists")
    return(TRUE)
  }
}

#' @title batch load object definitions
#' @param x an rflow object
#' @rdname load_nodes
#' @export
load_nodes <- function(x, ...) {
  UseMethod("load_nodes", x)
}


#' @param conflict logical; How to resolve conflict when an object of the same id already exists in the rflow?
#' @param verbose logical; print verbose output?
#' @rdname load_nodes
#' @method load_nodes rflow
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

#' Initialize a node and add it to an rflow
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

#' @method add_node list
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

  if (verbose) cat("Adding/updating  ", crayon::red(id), "\n", sep = "")

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

      # list.files is needed because of case-insensitve file systems
      # TODO: prevent creating ambigous object names on Windows (and Mac?)
      if (file.exists(fp) && length(list.files(path = rflow$.persistence$path, paste0(id, ".rds")))) {
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

#' @method connect_nodes rflow
#' @export
connect_nodes.rflow <- function(rflow, ...) {
  lapply(rflow, function(x) x$connect(...))
}


# generic make function
#' Make/build target or multiple targets
#'
#' @param x node's id or rflow object
#' @param force logical; force eval()
#' @param verbose logical; switch on/off verbose output
#' @param ...
#' @export
make <- function(x, ...){
  if (!is.null(x)) {
    UseMethod("make", x)
  } else stop(substitute(x), " cannot be NULL!")
}


#' @method make node
#' @export
make.node <- function(x, force = FALSE, verbose = TRUE, verbose_prefix = "") {
  x$make(force = force, verbose = verbose, verbose_prefix = verbose_prefix)
}

# recurrent procedure
#' @method make character
#' @export
make.character <- function(
  id,
  rflow,
  force = FALSE,
  verbose = TRUE,
  verbose_prefix = ""
) {
  if (length(id) < 2) {
    if (length(id) == 0) return(NULL)
    make(rflow[[id]], force = force, verbose = verbose, verbose_prefix = verbose_prefix)
  } else {
    sapply(id, make.character, force = force, verbose = verbose, verbose_prefix = verbose_prefix)
  }
}

#' @param rflow rflow object
#'
#' @param leaves_only logical; Option to run make only from ending nodes. Avoids redundant visits on intermediate nodes.
#' @param force logical; force eval()?
#' @param verbose logical; Print verbose output?
#' @method make rflow
#' @export
make.rflow <- function(
  rflow,
  tags = NULL,
  leaves_only = TRUE,
  force = FALSE,
  verbose = TRUE
) {
  if (verbose) cat(rep("\u2500", 3), " Make ", rep("\u2500", 25), "\n\n", sep = "")

  E <- edges(rflow)
  N <- nodes(rflow)

  if (leaves_only) {
    nodes_to_make <- N[!E, on = c("id" = "from"), id]
  } else {
    # nodes_to_make <- ls(rflow)
    nodes_to_make <- N[["id"]]
  }

  if (length(tags)) {
    query_tags <- tags
    rm(tags)
    nodes_to_make <-
      N[id %in% nodes_to_make
        ][stringr::str_detect(tags, stringr::fixed(paste0("|", query_tags, "|"))), id]
  }

  # RUN
  res <- sapply(
    X       = nodes_to_make,
    FUN     = make,
    rflow   = rflow,
    force   = force,
    verbose = verbose
  )

  return(invisible(res))
}

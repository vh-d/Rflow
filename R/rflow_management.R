
#' Initialize a new DAG
#'
#' @param path path to rflow home folder. Nodes' definitions are searched for in this folder, cache and other data is saved into...
#' @param caching logical;
#' @param persistence logical;
#' @param logging logical;
#'
#' @return A new rflow object.
#' @export
#'
#' @examples
#' \dontrun{
#' RF <- new_rflow()
#' }
new_rflow <- function(
  path        = NULL,
  cache       = TRUE,
  persistence = TRUE,
  logging     = if (length(path)) TRUE else FALSE
) {
  result <- new.env()
  class(result) <- c("rflow", class(result))

  result[[".persistence"]] <- list(enabled = FALSE)
  result[[".cache"]]       <- list(enabled = FALSE)
  result[[".logging"]]     <- FALSE
  result[[".loggers"]]     <- list()

  if (length(path)) {
    result[[".def_path"]] <- path

    if (isTRUE(persistence)) {
      persistence_path <- file.path(path, ".rflow", "persistence")
      if (!dir.exists(persistence_path)) dir.create(persistence_path, recursive = TRUE)

      result[[".persistence"]] <-
        list(
          enabled = TRUE,
          path    = persistence_path
        )
    }

    if (isTRUE(cache)) {
      cache_path <- file.path(path, ".rflow", "cache")
      if (!dir.exists(cache_path)) dir.create(cache_path, recursive = TRUE)

      result[[".cache"]] <-
        list(
          enabled = TRUE,
          path    = cache_path
        )
    }
  }

  # setup logging
  loggers <- setup_logging(logging, path = path)
  result[[".logging"]] <- if (length(loggers)) TRUE else FALSE
  result[[".loggers"]] <- loggers
  log_record(result, "Rflow initialized")

  return(result)
}

setup_logging <- function(logging, ...) {
  UseMethod("setup_logging", logging)
}

setup_logging.NULL <- function(logging, ...) {
  NULL
}

# TRUE treated as "auto" mode
setup_logging.logical <- function(logging, path, ...) {
  if (isTRUE(logging)) {
    log_path <- file.path(path, ".rflow", "log")
    if (!dir.exists(log_path)) dir.create(log_path, recursive = TRUE)
    log_file_path <- file.path(log_path, "log.csv")

    setup_logging.character(log_file_path)
  }
}

# character value treated as file name
setup_logging.character <- function(logging, ...) {

  hf <- handler_file(path = logging)

  return(
    list(
      default = logger(name = "RFLOW", handlers = list(hf))
    )
  )
}

# logger object is passed in a list
setup_logging.logger <- function(logging, ...) {
  list(default = logging)
}

# for list apply setup on each and pass
setup_logging.list <- function(logging, ...) {
  loggers <- vector(mode = "list", length(logging))
  for (i in 1:length(logging)) {
    logger[[i]] <- logging[[i]]
  }
  names(loggers) <- names(logging)

  return(loggers)
}


#' @export
print.rflow <- function(x, ...) {
  cat("<rflow>\n")
  # cat("<rflow> ", crayon::red(self$id), ": ", self$name,  "\n", sep = "")
  if (length(x$.desc)) cat("  desc: ", crayon::italic(x$.desc),         "\n", sep = "")
  cat("  path: ", x$.def_path, "\n", sep = "")
  cat("  cache enabled: ",       isTRUE(x$.cache$enabled), "\n",
      "  persistence enabled: ", isTRUE(x$.persistence$enabled),"\n",
      "  logging enabled: ", isTRUE(x$.logging),"\n",
      "  nodes: ", paste0(crayon::red(utils::head(ls(x))), collapse = ", "), ", ...",
      sep = "")
}

#' Checks a graph as represented by Rflow is a acyclic (DAG)
#'
#' @param x an Rflow object
#'
#' @return TRUE is the Rflow is a DAG
#' @export
#'
#' @examples
#' \dontrun{
#' is_dag(MYRFLOW)
#' }
is_dag.rflow <- function(x) {
  # get edges
  E <- as_data_table_edges(x)

  setkey(E, from, to)

  # delete nodes with a dependency until nothing or a loop remains
  to_drop <- E[, setdiff(from, to)]
  while (nrow(E) > 0 & length(to_drop)) {
    E <- E[!.(to = to_drop), on = "to"][!.(from = to_drop), on = "from"]
    to_drop <- E[, setdiff(from, to)]
  }

  stopifnot(nrow(E) >= 0) # bug!

  return(nrow(E) == 0)
}



# a temporary tool for cleaning old cache files
cache_files <- function(x) {
  dtFILES <- file.info(list.files(x$.cache$path, full.names = TRUE))
  setDT(dtFILES, keep.rownames = TRUE)
  dtFILES[, rn := sub(basename(rn), pattern = ".rds$", replacement = "")]
  dtFILES[, c("id", "id_hash", "value_hash") := tstrsplit(rn, "[-_]")]
  setcolorder(dtFILES, c("id", "id_hash", "value_hash"))
  dtFILES[, rn := NULL]
  dtFILES[]
}


#' Remove all nodes from an rflow object
#'
#' @param rflow
#'
#' @export
clean_rflow <- function(rflow) {
  remove(list = ls(envir = rflow, all.names = FALSE), envir = rflow)
}


#' Delete/clean cache file/folder
#'
#' @param x a node or rflow object
#' @param ...
#'
#' @export
clean_cache <- function(x, ...) {
  UseMethod("clean_cache", x)
}

#' @method clean_cache rflow
#' @export
clean_cache.rflow <- function(x, ...) {

  log_record(x, "Cleaning cache")

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

#' @method clean_cache node
#' @export
clean_cache.node <- function(x, ...) {

  log_record(x, "Cleaning cache")

  # warn if not defined and return
  if (!length(x$cache$path)) {
    warning("Cache dir not defined!")
    return(TRUE)
  }

  # warn if nonexistent and return
  fp <- file.path(x$cache$path, filename_from_id(x$id, value_hash = x$hash))
  if (file.exists(fp)) {
    as.logical(
      1-unlink(fp)
    )
  } else {
    warning("Cache file does not exists")
    return(TRUE)
  }
}


#' Delete/clean stored state file/folder
#'
#' @param x a node or rflow object
#' @param ...
#'
#' @export
clean_persistence <- function(x, ...) {
  UseMethod("clean_persistence", x)
}


#' @method clean_persistence rflow
#' @export
clean_persistence.rflow <- function(x) {

  log_record(x, "Cleaning persistence storage")

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

#' @method clean_persistence node
#' @export
clean_persistence.node <- function(x) {

  log_record(x, "Cleaning persistence storage")

  # warn if not defined and return
  if (!length(x$persistence$path)) {
    warning("Cache dir not defined!")
    return(TRUE)
  }

  # warn if nonexistent and return
  fp <- file.path(x$persistence$path, x$persistence$file)
  if (file.exists(fp)) {
    as.logical(
      1-unlink(fp)
    )
  } else {
    warning("Cache file does not exists")
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


#' @param x an rflow object
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
    nodes_from_toml(
      path = x$.def_path,
      modified_since = x$.last_updated,
      verbose = verbose)

  if (length(obj_defs)) {
    res <-
      add_nodes(
        objs        = obj_defs,
        rflow       = x,
        conflict    = conflict,
        cache       = list(path = x$.cache$path),
        verbose     = verbose
      )
  } else {
    res <- NULL
  }

  x$.last_updated <- Sys.time()

  return(invisible(res))
}


#' Construct a list of nodes' definitions
#'
#' @param ... arguments packed into a list of definitions
#' @export
node_definitions <- function(...) {
  structure(list(...), class = c("node_definitions", "list"))
}


#' Load nodes' definitions from an R script.
#'
#' @param file path to R script file
#' @param encoding passed to parse()
#' @param ... args passed to source()
#'
# @seealso \code(\link(add_nodes)), \code(\link(add_node)
#' @export
nodes_from_r_script <- function(file, encoding = "UTF-8", ...) {

  tempenv <- new.env()
  result <- source(file = file, local = tempenv, encoding = encoding, ...)

  node_defs <-
    if (isTRUE(methods::is(result$value, "node_definitions"))) {
      result$value
    } else
      mget(ls(tempenv), tempenv) # could be as.list() but we would have to drop all potential hidden objects firs

  node_defs <- process_obj_defs(node_defs)

  return(node_defs)
}


#' load DAG's objects' definition from TOML files
#'
#' @param path path to folder with node definitions
#' @param modified_since a datetime object to keep only newly modified files
#' @param verbose logical; print verbose output?
#' @export
nodes_from_toml <- function(path, modified_since = NULL, verbose = TRUE) {

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


# loads object definition/state from a RDS file
#
# @param path path to RDS file with saved state
# @param ... args passed to readRDS()
load_state_of_node <- function(path, ...) {
  state_list <- readRDS(file = path, ...)

  return(
    c(
      state_list$private,
      state_list$self
    )
  )
}

# batch loading objects' definition/state from RDS files
#
# @param path path to folder containg RDS files with saved state (typically .rflow subdir of rflow config dir)
# @param recursive passed to list.files
# @param ignore.case passed to list.files
# @param ...
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
#' @param x object's definition
#' @param rflow an rflow object
#' @param ... further args passed to `as_node()`
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
  conflict = "update",
  connect  = FALSE,
  verbose  = TRUE
) {

  # extract object's id
  id <- get_id(x)
  definition_hash <- digest::digest(x)

  # default behaviour
  exists       <- isTRUE(id %in% names(rflow))
  recovering   <- FALSE # object should be recovered from disk

  if (!exists && rflow$.persistence$enabled) {
    fp <- file.path(rflow$.persistence$path, filename_from_id(id))
    recovering <- file.exists(fp)
  }

  initializing <- (!exists || conflict == "overwrite") || recovering

  # ...... initialization ......

  # non-existent objects need to be constructed/initialized first
  # either completely new objects or objects being recovered
  if (initializing) {

    if (verbose) {
      cat(crayon::red(id), "\n", sep = "")
      cat("  * initializing")
    }

    if (recovering) {
      if (verbose) cat(" from a saved state...\n")
      saved_state   <- load_state_of_node(path = fp)
      initiated_obj <-
        as_node(
          saved_state,
          persistence = rflow[[".persistence"]],
          logging     = rflow[[".logging"]],
          loggers      = rflow[[".loggers"]],
          ...
        )
    } else {
      if (verbose) cat(" as a new object...\n")
      initiated_obj <-
        as_node(
          x,
          persistence     = rflow[[".persistence"]],
          logging         = rflow[[".logging"]],
          loggers          = rflow[[".loggers"]],
          definition_hash = definition_hash,
          ...
        )
    }

    # assign reference
    assign(
      x     = id,
      value = initiated_obj,
      pos   = rflow
    )

    # link the two environments
    parent.env(rflow[[id]]) <- rflow
  }


  # ...... updating ......

  updating <- ((exists && conflict == "update") || recovering) && !identical(rflow[[id]][["definition_hash"]], definition_hash) # object already exists and is only going to be updated

  if (updating) {
    if (verbose) {
      if (!initializing) cat(crayon::red(id), "\n", sep = "")
      cat("  * updating...\n")
    }

    update_node(x, rflow = rflow, definition_hash = definition_hash, verbose = verbose)
  }

  # connect to required objects
  if (connect && (updating || initializing)) rflow[[id]]$connect()

  result <- stats::setNames(TRUE, id)

  return(result)
}


#' batch init objects
#'
#' @param objs list of nodes' definitions
#' @param rflow an rflow object
#' @param connect logical;
#' @param ... further args passed to `add_node()`
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
  definition_hash = digest::digest(obj),
  verbose = FALSE
) {

  # extract object's id
  id <- get_id(obj)

  # find the node that is going to be updated
  if (!(id %in% names(rflow))) stop("Object ", id, " not found!")

  # update
  do.call(rflow[[id]]$update_definition, args = c(obj, definition_hash = definition_hash, verbose = verbose))
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
connect_nodes.rflow <- function(x, ...) {
  lapply(x, function(x) x$connect(...))
}


# generic make function
#' Make/build target or multiple targets
#'
#' @param x node's id or rflow object
#' @param force logical; force eval()
#' @param verbose logical; switch on/off verbose output
#'
#' @details
#' There are multiple ways (S3 methods) to choose targets for building. See examples.
#' @rdname make
#' @export
make <- function(x, ...){
  UseMethod("make", x)
}

#' @export
make.NULL <- function(x, ...) {
  warning("Nothing to do with ", substitute(x), " being NULL")
  NULL
}

#' @rdname make
#' @method make node
#' @examples
#' \dontrun{
#' make(RF$mynode)
#' }
#' @export
make.node <- function(x, force = FALSE, verbose = TRUE, verbose_prefix = "") {
  x$make(force = force, verbose = verbose, verbose_prefix = verbose_prefix)
}

# recurrent procedure
#' @rdname make
#' @method make character
#' @examples
#' \dontrun{
#' make(c("mynode", "othernode"), RF)
#' }
#' @export
make.character <- function(
  x,
  rflow,
  force = FALSE,
  verbose = TRUE,
  verbose_prefix = ""
) {
  if (!length(x)) return(c())
  if (length(x) == 1) {
    make(rflow[[x]], force = force, verbose = verbose, verbose_prefix = verbose_prefix)
  } else {
    sapply(x, make.character, rflow = rflow, force = force, verbose = verbose, verbose_prefix = verbose_prefix)
  }
}

#' @param tags filter nodes by tags
#' @param leaves_only logical; Option to run make only from ending nodes. Avoids redundant visits on intermediate nodes.
#' @param force logical; force eval()?
#' @param verbose logical; Print verbose output?
#' @details
#' The tags parameter can be used to filter nodes in two modes depending on length of the argument. A character vector of lenght > 1 results in union of matches. A scalar character value is applied as a regular expression.
#' @examples
#' \dontrun{
#' make(RF, tags = "DB")
#' }
#' @method make rflow
#' @rdname make
#' @export
make.rflow <- function(
  x,
  tags = NULL,
  leaves_only = TRUE,
  force = FALSE,
  tagsMatchLogic = "all",
  verbose = TRUE
) {
  # TODO: this is ugly, make it more pipe-able usig nodes(), allow queries, etc...

  log_record(x, "Make", sys_call_formatted())
  if (verbose) cat(rep("\u2500", 3), " Make ", rep("\u2500", 25), "\n\n", sep = "")

  E <- as.data.table.rflow(x, "edges")
  N <- as.data.table.rflow(x, "nodes")

  # exit if there's nothing to be done
  if (!nrow(N)) return(invisible(NULL))

  nodes_to_make <- N[, id]

  if (length(tags)) {
    query_tags <- tags
    rm(tags) # prevent confusion with tags column in N data.table

    nodes_to_make <- N[matchtags(pattern = query_tags, logic = tagsMatchLogic, tags = tags), id]

    N <- N[id %in% nodes_to_make]
  }

  if (leaves_only) {
    nodes_to_make <- N[!E[(to %in% nodes_to_make)], on = c("id" = "from"), id]
  }

  # RUN
  res <- sapply(
    X       = nodes_to_make,
    FUN     = make,
    rflow   = x,
    force   = force,
    verbose = verbose
  )

  return(invisible(res))
}


#' @method make list
#' @rdname make
#' @examples
#' \dontrun{
#' nodes(RF) %>% FilterWith("DB" %in% tags & .last_evaluted < Sys.date()) %>% make()
#' }
#' @export
make.list <- function(x, ...) {
  sapply(X = x, FUN = make, ...)
}


#' Remove an object
#'
#' @param x node object or an id (character)
#'
#' @return
#' delete methods can only return TRUE or raise error
#' @export
delete <- function(x, ...) {
  UseMethod("delete", x)
}

#' @param cache
#' @param persistency
#' @param value
#' @param ...
#' @param completely logical; all bellow?
#' @param cache logical; remove all cache related to the object?
#' @param persistency logical; remove persistency metadata?
#' @param value logical; remove target data?
#'
#' @export
#' @method delete node
#' @rdname delete
delete.node <- function(x, completely = FALSE, cache = FALSE | completely, persistency = FALSE | completely, value = FALSE | completely, ...) {

  if (isTRUE(cache)) clean_cache(x)
  if (isTRUE(persistency)) clean_persistence(x)
  if (isTRUE(value)) x$remove()

  log_record(x, "Removing node object")
  rm(list = x$id, envir = parent.env(x))

  return(TRUE)
}

#' @export
#' @method delete character
delete.character <- function(x, rflow, completely = FALSE, ...) {
  delete.node(rflow[[x]], completely = completely)
}

#' @export
#' @method delete rflow
delete.rflow <- function(x, id, completely = FALSE, ...) {
  delete.node(x[[id]], completely = completely, ...)
}


# @export
# @method delete r_node
#' delete.r_node <- function(x, completely = FALSE, cache = FALSE | completely, persistency = FALSE | completely, value = FALSE, ...) {
#'   NextMethod("delete", x)
#' }


# node --------------------------------------------------------------------

#' @title node class
#' @description Node is a basic building block of an rflow DAG.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords node
#' @return Object of \code{\link{R6Class}} with basic functionality for rflows.
#' @format \code{\link{R6Class}} object.
#' @field id character; unique id
#' @field env character; name of a environment (container or group of objects)
#' @field name character; object's name
#' @field desc character; description
#' @field depends character; vector of upstream nodes (other nodes this node is depending on)
#'
#' @details
#' Nodes may represent different types of data targets (R objects, files, database tables), or jobs.
#' Interconnected nodes together function as basic building elements of directed acyclic graph (DAG).
#'
#' Nodes are implemented using R6 classes. There are various flavors of the basic node class (r_node, db_node, file_node) that have their specific requirements and features
#'
#'
#' Constructing nodes
#' * from stored state
#' * from lists (coming from TOML, YAML...)
#'
#' @usage
#' node$new(id = "node")
#' @section Methods:
#' \describe{
#'   \item{\code{$make()} or \code{make(node)}}{This method is used to build/make targets. It recursively solves dependencies by building all targets for nodes declared or detected as a dependency.}
#'   \item{\code{$eval()}}{Compared to `make()`, `eval()` only runs the code/job associated with the node alone. It does not try to ensure all the dependencies are ready. }
#'   \item{\code{$process()}}{`process()` is usually used internally by `eval()` and should not be called directly}
#'   \item{\code{$value}}{Returns target's value. Targets such as database tables or csv files are automatically convert into R values.}
#'   \item{\code{$exists()}}{Checks whether the target exists or not.}
#'   \item{\code{$time_to_eval()}}{Returns a record(s) of time to evaluate.}
#' }
node <- R6::R6Class(

  classname = "node",

  private = list(
    .last_evaluated = NULL, # datetime when target value was last evaluated/made
    .last_changed   = NULL, # datetime when target value changed the last time (it might be evaluated without any change)
    .time_to_eval = list(),
    .trigger_defchange  = FALSE,

    .vis_params_default = list(
      shape = "diamond"
    )
  ),

  public    = list(
    id         = NULL, # unique withing an rflow
    env        = NULL, # name of container (real such as R env or datebase schema, or abstract such as group of files)
    name       = NULL, # name is unique withing its env
    desc       = NULL, # brief describtion of the object

    tags       = NULL, # character vector of tags

    depends    = NULL, # character vector of dependencies (upstream nodes)
    upstream   = NULL, # vector of references to depencies
    downstream = NULL, # vector of references to dependants

    definition_hash = NULL,

    persistence        = NULL,
    trigger_manual     = FALSE,
    trigger_condition  = NULL,

    logging = NULL,
    loggers = list(),

    validators = NULL,

    vis_params = NULL,

    vis_params_process = function(params) {
      union.list(
        private$.vis_params_default,
        params
      )
    },

    # derive object's ID from given identificators
    set_id = function(id, name, env) {
      if (is.null(id) & (is.null(name) | is.null(env))) stop("Missing id or (env + name)!") # either 'id' or 'name' + 'env' arguments have to be provided
      self$id <- if (is.null(id)) paste0(env, ".", name) else id
    },

    # set up persistency
    set_persistence = function(persistence) {
      self$persistence <-
        if (is.list(persistence) && length(persistence$path)) {
          if (dir.exists(persistence$path)) {
            list(
              enabled = TRUE,
              path    = persistence$path,
              file    = filename_from_id(self$id)
            )
          } else stop(persistence$path, " does not exist.")
        } else {
          list(
            enabled = FALSE
          )
        }

      invisible(TRUE)
    },

    print = function(...) {
      cat("<", class(self)[1], "> ", crayon::red(self$id), ": ", self$name,  "\n", sep = "")
      if (length(self$desc)) cat("  desc: ", crayon::italic(self$desc),         "\n", sep = "")
      if (length(self$tags)) cat("  tags: ", paste0(self$tags, collapse = " "), "\n", sep = "")
      cat("  depends: ", paste0(self$depends, collapse = ", "), "\n", sep = "")
      cat("  evaluated: ", as.character(self$last_evaluated), "\n",
          "  changed:   ", as.character(self$last_changed),   "\n", sep = "")
      cat("  triggers: ", "\n", sep = "")
      cat("    definition: ", self$trigger_defchange, "\n", sep = "")
      cat("    manual:     ", self$trigger_manual,    "\n", sep = "")
      if (length(self$trigger_condition)) cat("    condition:  ", self$check_trigger_condition(), "\n", sep = "")
    },

    label = function() {
      label <- paste0(self$env, "\n", self$name)

      return(label)
    },

    title = function() {
      paste0("<b>", self$id, "</b>", " &lt;", class(self)[1], "&gt;", "<br>") -> .

      # insert description
      if (length(self$desc) && !is.na(self$desc))
        . <- paste0(
          .,
          "<em>",
          stringr::str_replace_all(
            stringr::str_wrap(self$desc, width = 80), stringr::fixed("\n"), "<br>"),
          "</em><br>"
        )

      # insert date of last evaluation
      . <- paste0(
        .,
        "<font size=\"-2\">",
        "evaluated: <em>", self$last_evaluated, "</em><br>",
        # "changed:   <em>", self$last_changed,   "</em><br>",
        "</font>"
      )

      return(.)
    },

    initialize =
      function(
        id      = NULL,
        name    = NULL,
        env     = NULL,
        desc    = NULL,
        tags    = NULL,
        depends = NULL,

        trigger_defchange = NULL,
        trigger_condition = NULL,

        definition_hash = NULL,

        persistence = list(enabled = FALSE),
        .last_evaluated = NULL,
        .last_changed   = NULL,
        .trigger_defchange = NULL,
        .time_to_eval = NULL,

        vis_params = NULL,

        validators = NULL,

        logging = FALSE,
        loggers = NULL,
        store   = TRUE,
        ...
      ) {
        self$set_id(id = id, name = name, env = env)

        self$name    <- name
        self$env     <- env
        self$desc    <- desc

        self$logging <- isTRUE(logging)
        add_loggers(self, loggers)

        log_record(self, self$id, paste0("Starting initialization for ", paste0(class(self), collapse = " - ")))
        log_record(self, self$id, "Generic node class initialization")

        self$definition_hash <- definition_hash

        self$set_persistence(persistence)

        self$tags    <- as.character(tags)

        self$validators <- validators

        self$vis_params <- self$vis_params_process(vis_params)

        depends_char <- if (is.character(depends)) depends else names(depends)
        self$depends <- depends_char

        if (length(.trigger_defchange))
          private$.trigger_defchange <- as.logical(.trigger_defchange) else
            if (length(trigger_defchange)) # TODO: drop this when all projects are migrated
              private$.trigger_defchange <- as.logical(trigger_defchange)

        if (!is.null(trigger_condition))
          self$trigger_condition <- as_r_expr(trigger_condition)

        private$.last_evaluated <- if (length(.last_evaluated)) .last_evaluated else as.POSIXct(NA)
        private$.last_changed   <- if (length(.last_changed))   .last_changed else as.POSIXct(NA)
        private$.time_to_eval  <- .time_to_eval

        other_args <- list(...)
        if (length(other_args)) warning("These unknown properties are ignored: ", paste(names(other_args), collapse = ", "), "\n")

        # this should be always at the very end
        # storing is not neccesary when e.g. update_definition() is called from ancestor's method
        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    # save function for state persistency
    # currently only file-based store backend is supported
    store_state = function(
      public_fields  = NULL,
      private_fields = NULL
    ) {

      log_record(self, self$id, "Writing to persistence storage")

      public_fields  <-
        unique(
          c(
            "id", "name", "env",
            "desc", "tags",
            "depends",
            "logging",
            "validators",
            "definition_hash",
            "trigger_condition",
            "vis_params",
            public_fields
          )
        )

      private_fields <-
        unique(
          c(
            ".last_evaluated", ".last_changed",
            ".time_to_eval",
            ".trigger_defchange",
            private_fields)
          )

      saveRDS(
        object = list(
          self    = c(type = class(self)[1],
                      if (length(public_fields))  mget(public_fields,  envir = self)    else NULL),
          private =   if (length(private_fields)) mget(private_fields, envir = private) else NULL
        ),
        file = file.path(self$persistence$path, self$persistence$file)
      )
    },

    # allows to change properties of existing nodes (after they are initialized, e.g. from persistence
    update_definition =
      function(
        id      = NULL,
        type    = NULL,
        desc    = NULL,
        tags    = NULL,
        depends = NULL,
        definition_hash = NULL,
        trigger_condition = NULL,
        vis_params = NULL,
        validators = NULL,

        ...,
        store   = TRUE,
        logging = NULL,
        loggers = NULL,
        verbose = TRUE
      ) {

        self$logging <- isTRUE(logging)
        add_loggers(self, loggers)

        log_record(self, self$id, "Updating definition")

        self$definition_hash <- definition_hash

        # graphics params need to be processed before checking
        vis_params <- self$vis_params_process(vis_params)
        if (!identical(vis_params, self$vis_params)) {
          self$vis_params <- vis_params
        }

        # changes in dependencies
        depends_char <- if (is.character(depends)) depends else names(depends) # TODO: remove this? the idea was that the argument can be a named list() where every entry/dependency with specified with some specific criterias
        if (!setequal(self$depends, depends_char)) {

          if (verbose) notify_update(self$id, "dependencies")

          self$depends <- depends_char
          private$.trigger_defchange <- TRUE
        }

        # changes in description
        if (!is.null(desc)) self$desc <- desc

        # changes in tags
        if (!identical(as.character(self$tags), as.character(tags))) {
          if (verbose) notify_update(self$id, "tags")
          self$tags <- as.character(tags)
        }

        # changes in validators
        if (!is.null(validators)) {
          if (!identical(self$validators, validators)) {
            if (verbose) notify_update(self$id, "validators")
            self$validators <- validators
            private$.trigger_defchange <- TRUE
          }
        }

        # changes in user defined trigger
        trigger_condition <- suppressWarnings(as_r_expr(trigger_condition))
        if (!identical(as.character(self$trigger_condition), as.character(trigger_condition))) {
          if (verbose) notify_update(self$id, "trigger condition")
          self$trigger_condition <- trigger_condition
        }

        # currently all the remaingn arguments are ifnored, TODO ?
        # other_args <- list(...)
        # if (length(other_args)) warning("Not updating ", paste(names(other_args), collapse = ", "), " properties.\n")

        # this should be always at the very end
        # storing is not neccesary when e.g. update_definition() is called from ancestor's method
        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    # connect object to upstream
    set_upstream = function(obj) {
      if (!in.R6(obj, self$upstream)) {
        self$upstream <- c(self$upstream, obj)
      }
    },

    # connect this object to its upsream (currently not used)
    set_downstream = function(obj) {
      if (!in.R6(self, obj$downstream)) {
        obj$downstream <- c(obj$downstream, self)
      }
    },

    # make the references to both upstream and downstream
    connect_to = function(id) {
      # connect object to upstream

      if (exists(id, where = parent.env(self))) {

        obj <- get(x = id, envir = parent.env(self))

        self$set_upstream(obj)
        self$set_downstream(obj)

      } else {
        warning("Node", crayon::red(id), " does not exists...?")
        return(FALSE)
      }

      return(TRUE)
    },

    # make the references to all relevant upstream objects
    connect = function(verbose = TRUE) {
      log_record(self, "Connecting to upstream nodes")
      if (verbose) cat("Connecting ", crayon::red(self$id), " to: ", crayon::red(self$depends, collapse = " "), "\n", sep = "")
      sapply(self$depends, self$connect_to)
    },

    get = function() {
      log_record(self, self$id, "Call to an empty remove() method.")
      warning("This is just empty method...")
    },

    remove = function() {
      log_record(self, self$id, "Call to an empty remove() method.")
      warning("This is just empty method...")
    },

    check_trigger_condition = function() {
      isTRUE(eval(self$trigger_condition, envir = self))
    },

    # run checks that triggers evaluation
    # note that the order of checking the triggers is important (short circuit)
    check_triggers = function(verbose = TRUE, verbose_prefix = "") {
      log_record(self, self$id, "Checking triggers")

      if (!isFALSE(self$trigger_defchange))         {if (verbose) notify_trigger(self$id, "change in eval. expression", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}
      if (!isFALSE(self$trigger_manual))            {if (verbose) notify_trigger(self$id, "manual trigger", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}
      if (!isFALSE(self$check_trigger_condition())) {if (verbose) notify_trigger(self$id, "custom trigger condition", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

      if (!length(self$last_evaluated) || is.na(self$last_evaluated)) {if (verbose) notify_trigger(self$id, "unknown datetime of last eval", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

      return(FALSE)
    },

    # to reset triggers (e.g. after successfull evaluation)
    reset_triggers = function() {
      log_record(self, self$id, "Resetting triggers")

      private$.trigger_defchange <- FALSE
      self$trigger_manual        <- FALSE

      # make changes persistent
      if (self$persistence$enabled) self$store_state()

      return(invisible(TRUE))
    },

    # main evaluation function
    eval = function(verbose = TRUE, verbose_prefix = "") {

      log_record(self, self$id, "Evaluation started")

      time_start <- Sys.time()
      self$process(verbose = verbose, verbose_prefix = verbose_prefix)
      private$.last_evaluated <- Sys.time()
      private$.time_to_eval[[length(private$.time_to_eval) + 1]] <- private$.last_evaluated - time_start

      log_record(self, self$id, "Evaluation finished")

      haschanged <- self$changed(verbose = verbose, verbose_prefix = verbose_prefix)

      # all triggers should be reset now
      self$reset_triggers()

      # make changes persistent
      if (self$persistence$enabled) self$store_state()

      # by default return no change (but descendant classes with some actual logic in eval() should implement detection of changes or return TRUE by default)
      return(haschanged)
    },

    process = function(verbose = TRUE, verbose_prefix = "") {
      log_record(self, self$id, "Call to an empty eval() method.")
    },

    changed = function(verbose = TRUE, verbose_prefix = "") {
      FALSE # by default signal no change
    },

    # generic make function that recursively solves all dependencies
    # it is the workhorse of the Rflow framework
    # in most cases, this is not supposed to be overloaded in custom subclasses
    make = function(force = FALSE, verbose = TRUE, verbose_prefix = "", .visited = as.environment(list(ids = character()))) {

      log_record(self, "Solving make")
      if (verbose) {
        cat(verbose_prefix, "\u250C Solving node ", crayon::red(crayon::bold(self$id)), "\n", sep = "")
      }

      # resolve/make upstream nodes ------------------
      if (length(self$upstream) != length(self$depends)) {
        log_record(self, "Connections to upstream nodes does not match dependencies.")
        self$connect() # check that it's connected to upstream
      }

      # do not evaluate unless triggered
      trigger_from_upstream <- FALSE

      # collect triggers from upstream
      log_record(self, "Resolving upstream nodes")
      upstream_changed_ids <- character()
      for (y in self[["upstream"]]) {

        # skip nodes that were already visited
        if (!isTRUE(y$id %in% .visited$ids)) {
          # upstream can trigger me if it has changed since my last eval
          y_changed <- y$make(force = force, verbose = verbose, verbose_prefix = paste0(verbose_prefix, "\u2502  "), .visited = .visited) # TODO: this might be redundant if do the next line anyawy
        } else {
          if (verbose) {
            cat(verbose_prefix, "\u250C ", crayon::red(crayon::bold(y$id)), ": skipping", "\n", sep = "")
          }
          y_changed <- NULL
        }

        y_newer <- isTRUE(y_changed) || !isFALSE(y$last_changed > self$last_evaluated)

        # increment list of changed nodes in upstream
        # there is another trigger for cases when self$last_evaluated in NULL/NA, therefore isTRUE() should be safe and is better for logging
        if (!isFALSE(y_newer) || isTRUE(y_changed))
          upstream_changed_ids <- c(upstream_changed_ids, y$id)

        trigger_from_upstream <- trigger_from_upstream || y_newer # TODO: can trigger_upstream_newer ever be different from trigger_upstream_changed?
      }

      triggered <-
        if (isTRUE(force)) { # forced evaluation
          log_record(self, "Triggered by manual trigger")
          if (verbose) notify_trigger(self$id, trigger = "manual trigger",                verbose_prefix = paste0(verbose_prefix, "\u2514 "))
          TRUE
        } else if (!length(self$last_evaluated) || is.na(self$last_evaluated)) {
          log_record(self, "Triggered by missing datetime of last evaluation")
          if (verbose) notify_trigger(self$id, trigger = "unknown datetime of last eval", verbose_prefix = paste0(verbose_prefix, "\u2514 "))
          TRUE
        } else if (!isFALSE(trigger_from_upstream)) {
          log_record(self, paste0("Triggered by changes of upstream nodes' values(", paste0(upstream_changed_ids, collapse = ", "), ")"))
          if (verbose) notify_trigger(self$id, trigger = paste0("changes in upstream nodes (", paste0(upstream_changed_ids, collapse = ", "), ")"), verbose_prefix = paste0(verbose_prefix, "\u2514 "))
          TRUE
        } else if (!isFALSE(self$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) {
          TRUE
        } else {
          log_record(self, "Not triggered")
          if (verbose) cat(verbose_prefix, "\u2514 ", crayon::silver(self$id, " not triggered.", sep = ""), "\n", sep = "")
          .visited$ids <- unique(c(.visited$ids, self$id))
          return(invisible(FALSE))
        }

      # then make the object itself
      # eval should return if the object was evaluated/changed etc...
      trigger_downstream <- !isFALSE(self$eval(verbose = verbose, verbose_prefix = paste0(verbose_prefix, "\u2502  ")))

      # validate the result
      validations <- self$validate(verbose = TRUE, verbose_prefix = "")
      if (length(validations)) {
        results <- as.data.table(validations)[(!passed)]

        if (nrow(results)) {
           notify_invalid(
             self$id,
             validator_names = results[, validator],
             validator_signals = results[, signal],
             verbose_prefix = paste0(verbose_prefix, "\u2502  ")
           )
        }

        if (nrow(results[signal == "stop"])) {
          log_record(self, "stopping the make() proccess due to invalid builds.")
          stop("Stopping the make() process!")
        }
      }

      # all triggers should be resetted now
      self$reset_triggers()

      .visited$ids <- unique(c(.visited$ids, self$id))

      # return whether dependants should be triggered or not
      return(invisible(trigger_downstream))
    },

    validate = function(verbose = TRUE, verbose_prefix = "") {
      if (length(self$validators))
        confront(self$validators, self$get())
    },

    time_to_eval = function(what = getOption("RFLOW_TIME_TO_EVAL", "LAST")) {
      switch(
        what,
        "LAST" = unlist(tail(private$.time_to_eval, 1)),
        "ALL"  = unlist(private$.time_to_eval),
        "MEAN" = mean(unlist(private$.time_to_eval)),
        NULL   = unlist(tail(private$.time_to_eval, 1))
      )
    }

  ),

  active = list(

    value = function(value) {
      if (missing(value)) {
        return(self$get())
      } else {
        stop("Can't set `$value this way. Use make() or eval() instead.")
      }
    },

    # datetime of last evaluation
    last_evaluated = function(value) {
      if (missing(value)) {
        return(private$.last_evaluated)
      } else {
        private$.last_evaluated <- value
        private$.last_changed   <- value
      }
    },

    # datetime of last change (does not have to equal to last evaluation unless the target value really changes)
    last_changed = function(value) {
      if (missing(value)) {
        return(private$.last_evaluated)
      } else {
        stop("Can't set `$last_changed")
      }
    },

    trigger_defchange = function(value) {
      if (missing(value)) {
        return(private$.trigger_defchange)
      } else {
        warning("Setting `$trigger_defchange = TRUE` in a non-standard way")
        private$.trigger_defchange <- value
      }
    }
  )
)

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
  do.call(get(type)$new, args = union.list(x, list(...)))
}

#' @export
as_node.node <- function(x) {
  x
}


#' Compose an id value from env and name
#'
#' @param obj object definition
#'
#' @return Node's id as a character value constructed from env + name fields.
#' @export
compose_id <- function(x) {
  paste0(x$env, ".", x$name)
}


#' Get node's id
#'
#' @param obj
#'
#' @return Node's id as a character scalar
#' @export
get_id <- function(x) {
  UseMethod("get_id", x)
}

#' @export
get_id.list <- function(x) {
  if (length(x$id)) x$id else compose_id(x)
}

#' @export
get_id.node <- function(x) {
  x$id
}

#' @export
get_id.rflow <- function(x) {
  ls(x)
}

#' @export
get_id.node_list <- function(x) {
  sapply(x, get_id)
}


#' Returns a list with all node objects
#'
#' @param x an rflow object
#'
#' @return
#' List of all node object from given rflow
#' @export
nodes <- function(x) {
  UseMethod("nodes", x)
}


#' @export
nodes.rflow <- function(x) {
  mget(get_id(x), envir = x)
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

#' @title Evaluates a node
#'
#' @description Evaluates a node assuming all its requirements are met
#'
#' @param id node's id
#' @param ... args passed to node's eval() method
#'
#' @details
#' As oposed to `make()`, this function is not indended for frequent use by a user.
#'
#' @export
eval_node <- function(x, ...) {
  UseMethod("eval_node", x)
}

#' @export
eval_node.node <- function(x, ...) {
  x$eval(...)
}

#' @export
eval_node.character <- function(x, rflow) {
  rflow[[x]]$eval()
}


env_name_from_id <- function(id) {

  if (!identical(base::make.names(id), id)) stop(id, "is not a valid id!")

  strm <- stringr::str_match(string = id, pattern = "^([0-9a-zA-Z_]+?\\.)?([0-9a-zA-Z]+?[0-9a-zA-Z._]*?)$")
  strm[, 2] <- stringr::str_replace(strm[, 2], "\\.$", "")

  data.table(
    id   = strm[, 1],
    env  = strm[, 2],
    name = strm[, 3]
  )
}

#' Ensure all identifiers are set up correctly
#'
#' @param obj_defs list of nodes' definitions (list of lists)
#'
#' @export
process_obj_defs <- function(obj_defs) {

  for (obj_ind in seq_along(obj_defs)) {
    obj_name <- names(obj_defs[obj_ind])

    has_id_outside  <- length(obj_name) && obj_name != ""
    has_env_inside  <- length(obj_defs[[obj_ind]]$env)
    has_name_inside <- length(obj_defs[[obj_ind]]$name)
    has_id_inside   <- length(obj_defs[[obj_ind]]$id)

    if (!(has_id_outside || (has_env_inside & has_name_inside) || has_id_inside)) stop("Either named list or id or env + name have to be specified")

    if (has_id_outside | has_id_inside) {
      obj_id_inside  <- names(obj_defs[obj_ind])
      obj_id_outside <- obj_defs[[obj_ind]][["id"]]

      obj_id <- unique(c(obj_id_outside, obj_id_inside))

      if (length(obj_id) > 1) {
        warning(stringr::str_interp("id '${obj_id_inside}' does not match with object/file name '${obj_id_outside}'. Choosing '${obj_id_inside}' as an id."))
        obj_id <- obj_id_inside
      }

      node_env  <- env_name_from_id(obj_id)[["env"]][1]
      node_name <- env_name_from_id(obj_id)[["name"]][1]

      # node_id   <- env_names[id == "obj_id", id]
      if (!has_env_inside)
        obj_defs[[obj_ind]]$env  <- node_env  else
          if (obj_defs[[obj_ind]]$env  != node_env)
            stop(stringr::str_interp("${obj_id}: env '${obj_defs[[obj_ind]]$env}' does not match"))

      if (!has_env_inside)
        obj_defs[[obj_ind]]$name <- node_name else
          if (obj_defs[[obj_ind]]$name != node_name)
            stop(stringr::str_interp("${obj_id}: name '${obj_defs[[obj_ind]]$name}' does not match"))
    } else {
      obj_id <- compose_id(obj_defs[[obj_ind]])
    }

    full_names <- names(obj_defs)
    full_names[obj_ind] <- obj_id
    names(obj_defs) <- full_names
  }

  obj_defs
}

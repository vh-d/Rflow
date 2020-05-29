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
#' @field env character; name of a environemnt (container or group of objects)
#' @field name character; object's name
#' @field desc character; description
#' @field depends character; vector of uppstream nodes (other nodes this node is depending on)
#'
#' @details
#' Nodes may represent different types of data targets (R objects, files, database tables), or jobs.
#' Interconnected nodes together function as basic building elements of directed acyclic graph (DAG).
#'
#' Nodes are implemented using R6 classes. There are various flavours of the basic node class (r_node, db_node, file_node) that have their specific requirements and features
#'
#'
#' Constructing nodes
#' * from stored state
#' * from lists (comming from TOML, YAML...)
#'
#' @usage
#' node$new(id = "node")
#' @section Methods:
#' \describe{
#'   \item{\code{$make()} or \code{make(node)}}{This method is used to build/make targets. It recursively solves dependencies by building all targets for nodes declared or detected as a dependency.}
#'   \item{\code{$eval()}}{Compared to `make()`, `eval()` only runs the code/job associated with the node alone. It does not try to ensure all the dependencies are ready. `eval()` is usually used internally by `make()`.}
#'   \item{\code{$value}}{Returns target's value. Targets such as database tables or csv files are automatically convert into R values.}
#'   \item{\code{$exists()}}{Checks whether the target exists or not.}
#' }
node <- R6::R6Class(

  classname = "node",

  private = list(
    .last_evaluated = NULL, # datetime when target value was last evaluated/made
    .last_changed   = NULL, # datetime when target value changed the last time (it might be evaluated without any change)
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
        trigger_defchange  = NULL,
        trigger_condition = NULL,
        definition_hash = NULL,
        persistence = list(enabled = FALSE),
        .last_evaluated = NULL,
        .last_changed   = NULL,
        .trigger_defchange = NULL,

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

        # other_args <- list(...)
        # if (length(other_args)) warning("Ignoring ", paste(names(other_args), collapse = ", "), " properties.\n")

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

    # allows to change properties of existing objects/instances
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

      log_record(self, self$id, "Call to an mostly empty eval() method.")

      # all triggers should be resetted now
      self$reset_triggers()

      # make changes persistent
      if (self$persistence$enabled) self$store_state()

      # by default return no change (but descendant classes with some actual logic in eval() should implement detection of changes or return TRUE by default)
      return(FALSE)
    },

    # generic make function that recursivelly solves all dependencies
    # it is the workhorse of the Rflow framework
    # in most cases, this is not supposed to be overloaded in custom subclasses
    make = function(force = FALSE, verbose = TRUE, verbose_prefix = "") {

      log_record(self, "Solving make")
      if (verbose) {
        cat(verbose_prefix, "\u250C Solving node ", crayon::red(crayon::bold(self$id)), "\n", sep = "")
      }

      # solve/make upstream nodes first
      if (length(self$upstream) != length(self$depends)) {
        log_record(self, "Connections to upstream nodes does not match dependencies.")
        self$connect() # check that it's connected to upstream
      }

      # do not evaluate unless triggered
      results <- FALSE

      # collect triggers from upstream
      log_record(self, "Resolving upstream nodes")
      upstream_changed_ids <- character()
      for (y in self[["upstream"]]) {
        # upsream can trigger me if it has changed since my last eval
        trigger_upstream_changed <- y$make(force = force, verbose = verbose, verbose_prefix = paste0(verbose_prefix, "\u2502  ")) # TODO: this might be redundant if do the next line anyawy
        trigger_upstream_newer   <- y$last_changed > self$last_evaluated

        # increment list of changed nodes in upstream
        # there is another trigger for cases when self$last_evaluated in NULL/NA, therefore isTRUE() should be safe and is better for logging
        if (!isFALSE(trigger_upstream_newer) || isTRUE(trigger_upstream_changed))
          upstream_changed_ids <- c(upstream_changed_ids, y$id)

        results <- results || trigger_upstream_newer || trigger_upstream_changed # TODO: can trigger_upstream_newer ever be different from trigger_upstream_changed?
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
        } else if (!isFALSE(results)) {
          log_record(self, paste0("Triggered by changes of upstream nodes' values(", paste0(upstream_changed_ids, collapse = ", "), ")"))
          if (verbose) notify_trigger(self$id, trigger = paste0("changes in upstream nodes (", paste0(upstream_changed_ids, collapse = ", "), ")"), verbose_prefix = paste0(verbose_prefix, "\u2514 "))
          TRUE
        } else if (!isFALSE(self$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) {
          TRUE
        } else {
          log_record(self, "Not triggered")
          if (verbose) cat(verbose_prefix, "\u2514 ", crayon::silver(self$id, " not triggered.", sep = ""), "\n", sep = "")
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

      # return whether dependants should be triggered or not
      return(invisible(trigger_downstream))
    },

    validate = function(verbose = TRUE, verbose_prefix = "") {
      if (length(self$validators))
        confront(self$validators, self$get())
    }
  ) ,

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


# r_node ------------------------------------------------------------------

#' @export
r_node <- R6::R6Class(
  classname = "r_node",
  inherit = node,

  private = list(
    .vis_params_default = list(
      shape = "triangle"
    )
  ),

  public = list(

    r_env    = NULL,  # reference to R envinronment
    r_expr   = NULL,  # R expression

    hash     = NULL,  # hash of the represented R object from digest(), hashing enables checking for changes of the R objects
    cache    = list(enabled = FALSE),

    # triggers  = NULL, # every node has a list of triggers that are checked before evaluation

    cache_setup = function(cache) {

      log_record(self, self$id, "Setting up cache")

      self$cache <-
        if (is.list(cache) && length(cache$path)) {
          if (dir.exists(cache$path)) {
            list(
              enabled = TRUE,
              path    = cache$path
            )
          } else stop("Cannot setup a cache file: ", cache$path, " does not exist.")
        } else if (is.character(cache)) {
          if (dir.exists(cache)) {
            list(
              enabled = TRUE,
              path    = cache
            )
          } else stop(cache, " does not exist.")
        } else {
          list(
            enabled = FALSE
          )
        }

      invisible(TRUE)
    },

    cache_exists = function() {
      file.exists(file.path(self$cache$path, filename_from_id(self$id, value_hash = self$hash$hash))) # TODO: precompute ID hash
    },

    cache_write = function() {
      log_record(self, self$id, "Writing cache")
      saveRDS(object = self$get(), file = file.path(self$cache$path, filename_from_id(self$id, value_hash = self$hash$hash)))
    },

    cache_restore = function(delayed = getOption("RFLOW_DELAYED_CACHE_LOAD", default = TRUE)) {

      if (isTRUE(delayed)) {
        log_record(self, self$id, "Restoring value from cache (immediate)")
        delayedAssign(
          x     = self$name,
          value = readRDS(file.path(self$cache$path, filename_from_id(self$id, value_hash = self$hash$hash))),
          assign.env = self$r_env
        )
      } else {
        log_record(self, self$id, "Restoring value from cache (delayed)")
        assign(
          x     = self$name,
          value = readRDS(file.path(self$cache$path, filename_from_id(self$id, value_hash = self$hash$hash))),
          pos   = self$r_env
        )
      }
    },

    print = function(...) {
      super$print()
      cat("  cache: \n")
      cat("    enabled: ", self$cache$enabled,  "\n", sep = "")
      cat("    exists:  ", self$cache_exists(), "\n", sep = "")
      cat("  expression: \n")
      cat_with_prefix(
        crayon::cyan(
          deparse_nicely(self$r_expr)
        ),
        prefix = "    "
      )
    },

    title = function() {
      title <- super$title()

      title <- paste0(
        title,
        "<p>",
        "R:<br><font size=\"-2\" face = \"monospace\">",
        stringr::str_replace_all(
          stringr::str_replace_all(
            stringr::str_c(deparse_nicely(self$r_expr), collapse = "\n"),
            stringr::fixed("\n"), "<br>"),
          stringr::fixed(" "), "&nbsp;"),
        "</font></p>"
      )

      return(title)
    },

    initialize =
      function(
        ...,
        r_code  = NULL,
        r_expr  = NULL,
        type    = NULL,
        store   = TRUE,
        cache   = list(enabled = FALSE),
        hash    = NULL,

        verbose = TRUE
      ) {
        super$initialize(..., store = FALSE)
        log_record(self, self$id, "r_node class initialization")

        self$r_expr <- as_r_expr(firstnotnull(r_expr, r_code))

        if (!length(self$r_expr) && length(self$depends))
          warning(self$id, " is not a leaf node but has no R expression to evaluate")

        # caching properties
        self$cache_setup(cache)

        # hash
        if (length(hash) && isTRUE(self$cache$enabled)) {
          self$hash <- hash
        }

        # connect to R environment (.GlobalEnv if not specified othewise)
        if (is.null(self$env)) {
          self$r_env <- .GlobalEnv
        } else {
          r_env <-
            tryCatch(
              get(self$env, mode = "environment"),
              error = function(e)
                dynGet(self$env, inherits = TRUE)
            )
          if (!is.environment(r_env)) stop(paste(self$env, "is not an R environment object")) # dynGet does not check mode
          self$r_env <- r_env
        }

        # try restoring the object from cache
        if (self$cache$enabled)
          if (self$cache_exists()) {
            tryCatch(
              {
                self$cache_restore()
                # self$check_hash()
              },
              error = function(e) {
                warning("Cache for ", self$id, " could not be recovered.\n")
                self$hash <- NULL # any hash loaded from stored state is meaningless now
              }
            )
          } else {
            if (verbose) cat(crayon::red(self$id), ": no cache found\n", sep = "")
          }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    store_state = function(public_fields = NULL, private_fields = NULL) {
      super$store_state(
        public_fields  = unique(c(public_fields, c("r_expr", "hash"))),
        private_fields = private_fields
      )
    },

    update_definition =
      function(
        ...,
        r_code  = NULL,
        r_expr  = NULL,
        store   = TRUE,
        verbose = TRUE
      ) {
        super$update_definition(..., verbose = verbose, store = FALSE)

        r_expr <- as_r_expr(firstnotnull(r_expr, r_code))
        if (!identical(as.character(self$r_expr), as.character(r_expr))) {
          if (verbose) notify_update(self$id, "R expression")
          private$.trigger_defchange <- TRUE
        }
        self$r_expr <- r_expr # overwrite in case the srouce code has changed

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    eval = function(verbose = TRUE, verbose_prefix = "") {
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": Evaluating R expression:\n", sep = "")
        cat_with_prefix(
          crayon::cyan(
            deparse_nicely(self$r_expr)
          ),
          prefix = verbose_prefix
        )
      }

      # for referencing other objects in rflow
      .RFLOW <- parent.env(self)

      log_record(self, self$id, "Evaluation started")
      assign(self$name, eval(self$r_expr), pos = self$r_env)
      private$.last_evaluated <- Sys.time()

      log_record(self, self$id, "Evaluation finished")

      # checking hash before signalling change to parent
      changed <- self$check_hash()

      if (changed) log_record(self, self$id, "Value has changed")
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": done", if (changed) crayon::yellow(" (value has changed)"), ".\n", sep = "")
      }

      if (self$cache$enabled && (changed || !isTRUE(self$cache_exists()))) self$cache_write()

      # all triggers should be resetted now
      self$reset_triggers()

      # make changes persistent
      if (self$persistence$enabled) self$store_state()

      return(changed)
    },

    exists = function() {
      exists(self$name, where = self$r_env)
    },

    check_hash = function() {
      if (!self$exists()) return(NA) # TODO: or NULL?

      log_record(self, self$id, "Computing hash")
      hash <- digest::digest(object = self$get(), file = FALSE, algo = "md5")
      changed <- !isTRUE(self$hash$hash == hash)

      if (changed)
        self$hash <- list(
          hash = hash,
          time = Sys.time()
        )

      log_record(self, self$id, "hash changed:", changed)
      if (self$persistence$enabled) self$store_state()

      return(changed)
    },

    check_triggers = function(verbose = TRUE, verbose_prefix = "") {

      if (!isFALSE(super$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) return(TRUE)
      if (!isTRUE(self$exists())) {if (verbose) notify_trigger(self$id, "missing target/value", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

      return(FALSE)
    },

    get = function() {
      if (self$exists()) {
        return(get(self$name, pos = self$r_env))
      } else {
        warning("Object ", self$id, " (", self$name, ")", " does not exist.")
        return(NULL)
      }
    },

    remove = function(verbose = TRUE, verbose_prefix = "") {
      if (self$exists()) {
        if (verbose) notify_removal(self$id, verbose_prefix = verbose_prefix)
        return(invisible(rm(list = self$name, pos = self$r_env)))
      } else {
        if (verbose) notify_nonexistence(self$id, verbose_prefix = verbose_prefix)
        return(invisible(FALSE))
      }
    }
  ),

  active = list(

    last_evaluated = function(value) {
      if (missing(value)) {
        return(private$.last_evaluated)
      } else {
        private$.last_evaluated <- value
      }
    },

    last_changed = function(value) {
      if (missing(value)) {
        # file might have been modified but the content stayed the same
        self$check_hash()
        private$.last_changed <- self$hash$time
        return(private$.last_changed)
      } else {
        stop("Can't set `$last_changed")
      }
    }

  )
)

# db_node -----------------------------------------------------------------
solve_connection <- function(con) {
  UseMethod("solve_connection", con)
}

solve_connection.character <- function(con) {
  return(solve_connection(get(con)))
}

solve_connection.DBIConnection <- function(con) {
  return(con)
}

#' @export
db_node <- R6::R6Class(
  classname = "db_node",
  inherit   = node,

  private = list(
    .vis_params_default = list(
      shape = "square"
    )
  ),

  public    = list(

    env        = NULL,

    mode       = NULL,
    driver     = NULL,
    connection = NULL,
    read_args  = NULL,
    sql        = NULL,
    r_expr     = NULL,

    auto_remove = NULL,

    initialize =
      function(
        ...,
        auto_remove = TRUE,
        sql_code = NULL,
        sql      = NULL,
        r_code   = NULL,
        r_expr   = NULL,  # R expression
        connection = NULL,
        read_args = NULL,
        .last_evaluated = NULL,
        type     = NULL,
        store    = TRUE
      ) {
        super$initialize(..., store = FALSE)
        log_record(self, self$id, "db_node class initialization")

        # TODO:
        # * how to handle storage of connection when DBI connection object is given?
        # * tryCatch connection to avoid failure during update
        if (!length(connection)) {
          connection <- self$env
        }

        self$connection <- solve_connection(connection)
        self$read_args <- read_args
        self$auto_remove <- auto_remove

        # TODO: we need to handle situations when node is modified from R job to SQL job
        if (length(r_expr) || length(r_code)) {
          self$mode <- "R"
          self$r_expr <- as_r_expr(firstnotnull(r_expr, r_code))
        } else if (length(sql)) {
          self$mode <- "SQL"
          self$sql <- sql_structure(sql)
        } else if (length(sql_code)) {
          self$mode <- "SQL"
          self$sql <- sql_structure(sql_code)
        } else
          warning(self$id, ": no R expression/code or SQL code!")

        private$.last_evaluated <- if (length(.last_evaluated)) .last_evaluated else as.POSIXct(NA)

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    store_state = function(public_fields = NULL, private_fields = NULL) {
      super$store_state(
        public_fields  = unique(c(public_fields, c("read_args", "r_expr", "sql", "mode", "auto_remove"))),
        private_fields = private_fields
      )
    },

    update_definition =
      function(
        ...,
        auto_remove = NULL,
        sql_code = NULL,
        sql      = NULL,
        r_code   = NULL,
        r_expr   = NULL,  # R expression (from r_code)
        connection = NULL,
        read_args = NULL,
        store    = TRUE,
        verbose  = FALSE
      ) {
        super$update_definition(..., verbose = verbose, store = FALSE)

        if (!length(connection)) {
          connection <- self$env
        }

        self$connection <- solve_connection(connection)
        self$read_args  <- read_args

        if (length(auto_remove) && !identical(self$auto_remove, auto_remove)) {
          self$auto_remove <- auto_remove
        }

        # TODO: we need to handle situations when node is modified from R job to SQL job
        if (length(r_expr) || length(r_code)) {
          mode <- "R"
          r_expr <- as_r_expr(firstnotnull(r_expr, r_code))
        } else if (length(sql)) {
          mode <- "SQL"
          sql <- sql_structure(sql)
        } else if (length(sql_code)) {
          mode <- "SQL"
          sql <- sql_structure(sql_code)
        } else
          warning(id, ": no R expression/code or SQL code!")

        if (!identical(self$mode, mode)) {
          if (verbose) notify_update(self$id, "R/SQL mode")
          self$mode <- mode
          private$.trigger_defchange <- TRUE
        }

        if (!identical(as.character(self$r_expr), as.character(r_expr))) {
          if (verbose) notify_update(self$id, "R expression")
          private$.trigger_defchange <- TRUE
        }
        self$r_expr <- r_expr # overwrite in case the srouce code has changed

        if (!identical(self$sql,sql)) {
          if (verbose) notify_update(self$id, "SQL expressions")
          self$sql <- sql
          private$.trigger_defchange <- TRUE
        }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    execute_sql = function(verbose = TRUE, verbose_prefix = "") {

      log_record(self, self$id, "Executing SQL statement")
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": Executing SQL statements:\n", sep = "")
      }

      sapply(self$sql,
             function(sql_statement) {
               if (verbose) {
                 cat_with_prefix(
                   crayon::cyan(
                     sql_statement$code
                   ),
                   prefix = paste0(verbose_prefix, "  ")
                 )
               }
               tryCatch(
                 DBI::dbExecute(self$connection, sql_statement$code),
                 error = function(e) if (isTRUE(sql_statement$ignoreErrors)) return(NULL) else stop(e))
             }
      )
    },

    eval = function(verbose = TRUE, verbose_prefix = "") {
      verbose_prefix_inc <- paste0(verbose_prefix, "  ")

      exists_check <- self$exists()

      # for referencing other objects in rflow
      .RFLOW <- parent.env(self)

      # remove target object before rebuilding it
      if (self$auto_remove) self$remove(verbose = verbose, verbose_prefix = verbose_prefix_inc)

      if (self$mode == "SQL") {
        self$execute_sql(verbose = verbose, verbose_prefix = verbose_prefix_inc)
      } else {
        if (verbose) {
          # if (!is.null(self$sql_code)) cat(verbose_prefix, "SQL: ", self$sql_code, sep = "")
          cat(verbose_prefix_inc, crayon::red(self$id), ": Evaluating R expression:\n", sep = "")
          cat_with_prefix(
            crayon::cyan(
              deparse_nicely(self$r_expr)
            ),
            prefix = verbose_prefix_inc
          )
        }
        eval(self$r_expr) # TODO: explicitly specify some other envir for evaluation?
      }

      private$.last_evaluated <- Sys.time()

      # all triggers should be resetted now
      self$reset_triggers()

      # make changes persistent
      if (self$persistence$enabled) self$store_state()

      return(TRUE)
    },

    # check that the target object exists in the database
    exists = function() {
      log_record(self, self$id, "Checking existence of target DB object.")
      DBI::dbExistsTable(conn = self$connection, name = self$name)
    },

    check_triggers = function(verbose = TRUE, verbose_prefix = "") {

      if (!isFALSE(super$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) return(TRUE)
      if (!isTRUE(self$exists())) {if (verbose) notify_trigger(self$id, "missing target/value", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

      return(FALSE)
    },

    print = function(...) {
      super$print()
      cat("  DBI: ", class(self$connection)[1], "\n", sep = "")
      if (length(self$sql)) {
        cat("  SQL code:\n   ")
        self$print_sql()
      } else {
        cat("  R expression:\n   ", crayon::cyan(utils::head(deparse_nicely(self$r_expr))), "\n", sep = "")
      }
    },

    print_sql = function(head = NULL, prefix = "") {
      head <- as.integer(head)
      text <- paste_sql(self$sql)
      if (length(head)) text <- substr(text, 1, head)
      text <- crayon::cyan(text)
      if (length(prefix) && prefix != "") text <- add_prefix(x = text, prefix = prefix)
      cat(text)
    },

    title = function() {

      title <- super$title()

      switch(
        self$mode,

        "R" =
          paste0(
            title,
            "<p>",
            "R:<br><font size=\"-2\" face = \"monospace\">",
            stringr::str_replace_all(
              stringr::str_replace_all(
                stringr::str_c(deparse_nicely(self$r_expr), collapse = "\n"),
                stringr::fixed("\n"), "<br>"),
              stringr::fixed(" "), "&nbsp;"),
            "</font></p>"),

        "SQL" =
          paste0(
            title,
            "<p>",
            "SQL:<br><font size=\"-2\" face = \"monospace\">",
            stringr::str_replace_all(
              stringr::str_replace_all(
                deescape_quotes(paste_sql(self$sql)),
                stringr::fixed("\n"), "<br>"),
              stringr::fixed(" "), "&nbsp;"),
            "</font></p>"
          ),

        # else:
        title
      )
    },

    get = function(...) {
      if (self$exists()) {
        log_record(self, "Fetching data from DB.")
        do.call(
          DBI::dbReadTable,
          args = union.list(
            self$read_args,
            list(
              conn = self$connection,
              name = self$name,
              ...
            )
          )
        )
      } else {
        warning("Object ", self$id, " (", self$name, ")", " does not exist.")
        return(NULL)
      }
    },

    remove = function(verbose = TRUE, verbose_prefix = "") {

      if (self$exists()) {

        if (verbose) notify_removal(self$id, verbose_prefix = verbose_prefix)

        log_record(self, "Attempting to remove the target table from DB.")
        msg <-
          tryCatch(
            expr  = list(result = DBI::dbRemoveTable(conn = self$connection, name = self$name)),
            error = function(e) list(result = -1L, error1 = e)
          )

        # try as if it was a VIEW
        if (msg$result == -1L) {
          log_record(self, "Removing not succesfull. Trying removing a view.")
          msg <-
            tryCatch(
              expr  = list(result = DBI::dbExecute(conn = self$connection, statement = paste0("DROP VIEW ", self$name))),
              error = function(e) list(result = -1L, error1 = msg$error, error2 = e)
            )
        }

        if (msg$result == -1L) {
          stop(msg$error1, msg$error2)
        } # else:

        log_record(self, "Removing seems succesfull.")

        return(invisible(TRUE))

      } else {
        if (verbose) notify_nonexistence(self$id, verbose_prefix = verbose_prefix)
        return(invisible(FALSE))
      }
    }

  )
)


# accdb_node --------------------------------------------------------------

solve_connection.odbc32 <- function(con) {
  return(con)
}


#' @export
accdb_node <- R6::R6Class(
  classname = "accdb_node",
  inherit   = db_node,

  public    = list(

    exists = function() {
      log_record(self, self$id, "Checking existence of target DB object.")
      isTRUE(self$name %in% odbc32::sqlTables(self$connection)$TABLE_NAME)
    },

    get = function(...) {
      if (self$exists()) {
        log_record(self, "Fetching data from DB.")
        do.call(
          odbc32::sqlFetch,
          args = union.list(
            self$read_args,
            list(
              con = self$connection,
              name = self$name,
              ...
            )
          )
        )
      } else {
        warning("Object ", self$id, " (", self$name, ")", " does not exist.")
        return(NULL)
      }
    },

    remove = function(verbose = TRUE, verbose_prefix = "") {
      if (self$exists()) {
        log_record(self, "Attempting to remove the target table from DB.")
        if (verbose) notify_removal(self$id, verbose_prefix = verbose_prefix)
        return(invisible(odbc32::sqlDrop(con = self$connection, name = self$name)))
      } else {
        log_record(self, "Attempt to remove a missing target from DB.")
        if (verbose) notify_nonexistence(self$id, verbose_prefix = verbose_prefix)
        return(invisible(FALSE))
      }
    },

    execute_sql = function(verbose = TRUE, verbose_prefix = "") {
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": Evaluating SQL statements:\n", sep = "")
      }
      sapply(self$sql,
             function(sql_statement) {
               if (verbose) {
                 cat_with_prefix(
                   crayon::cyan(
                     sql_statement$code
                   ),
                   prefix = paste0(verbose_prefix, "  ")
                 )
               }
               tryCatch(
                 odbc32::sqlQuery(con = self$connection, query = sql_statement$code),
                 error = function(e) if (isTRUE(sql_statement$ignoreErrors)) return(NULL) else stop(e))
             }
      )
    }
  )
)


# excel sheet node ---------------------------------------------------------
#' @export
excel_sheet <- R6::R6Class(

  classname = "excel_sheet",
  inherit   = node,

  public    = list(

    path   = NULL,
    sheet  = NULL,
    hash   = NULL,
    read_args = NULL, # TODO!

    initialize =
      function(
        ...,
        path      = NULL,
        sheet     = 1L, # not in file_node
        read_args = NULL, # TODO!
        hash      = NULL,
        type      = NULL,
        store     = TRUE
      ) {
        if (!requireNamespace("openxlsx", quietly = TRUE)) stop(self$id, " requires openxlsx package.")

        super$initialize(..., store = FALSE)
        log_record(self, self$id, "excel_sheet class initialization")

        if (length(path)) {
          self$path <- as.character(path[1])
        } else {
          stop(self$id, ": missing file path.")
        }

        self$sheet <- if (length(sheet)) sheet else 1L

        if (length(hash)) {
          self$hash <- hash
        }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    update_definition =
      function(
        ...,
        path    = NULL,
        sheet   = 1L, # not in file_node
        store   = TRUE,
        verbose = TRUE
      ) {
        super$update_definition(..., verbose = verbose, store = FALSE)

        if (!identical(self$path, path)) {
          if (verbose) notify_update(self$id, "file path")
          self$path <- path
          private$.trigger_defchange <- TRUE
        }

        if (!identical(self$sheet, sheet)) {
          if (verbose) notify_update(self$id, "sheet name/index")
          self$sheet <- sheet
          private$.trigger_defchange <- TRUE
        }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    store_state = function(public_fields = NULL, private_fields = NULL) {
      super$store_state(
        public_fields  = unique(c(public_fields, c("path", "sheet", "read_args", "hash"))),
        private_fields = private_fields
      )
    },

    exists = function() {
      if (file.exists(self$path)) {
        if (is.numeric(self$sheet)) {
          return(isTRUE(as.integer(self$sheet) %in% seq_along(openxlsx::getSheetNames(self$path))))
        } else
          return(isTRUE(self$sheet %in% openxlsx::getSheetNames(self$path)))
      } else return(FALSE)
    },

    check_hash = function() {
      if (!self$exists()) return(NA) # TODO: or NULL?

      log_record(self, self$id, "Computing hash")
      hash <- digest::digest(object = self$get(), algo = "md5")
      changed <- !isTRUE(self$hash$hash == hash)

      if (changed) {
        self$hash <- list(
          hash = hash,
          time = Sys.time()
        )

        log_record(self, self$id, "hash changed:", changed)
        if (self$persistence$enabled) self$store_state()
      } else {
        log_record(self, self$id, "hash not changed", changed)
      }

      return(changed)
    },

    check_triggers = function(verbose = TRUE, verbose_prefix = "") {

      if (!isFALSE(super$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) return(TRUE)
      if (!isTRUE(self$exists())) {if (verbose) notify_trigger(self$id, "missing target/value", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

      return(FALSE)
    },

    get = function(...) {
      if (self$exists()) {
        log_record(self, "Fetching data from excel sheet.")
        do.call(
          openxlsx::read.xlsx,
          args = union.list(
            self$read_args,
            list(
              xlsxFile = self$path,
              sheet    = self$sheet,
              ...
            ),
          )
        )
      } else stop(self$id, ": sheet '", self$sheet, "' does not exists in ", self$path)
    },

    title = function() {
      title <- super$title()

      title <- paste0(title, "<font size=\"-2\">", "path: ", self$path, " (", self$sheet, ")", "</font><br>")

      return(title)
    },

    print = function(...) {
      super$print()
      cat("  path: ", self$path, " (", self$sheet, ")\n", sep = "")
    }

  ),

  active = list(

    last_evaluated = function(value) {
      if (missing(value)) {
        return(file.mtime(self$path))
      } else {
        stop("Can't set `$last_evaluated")
      }
    },

    last_changed = function(value) {
      if (missing(value)) {
        # file might have been modified but the content stayed the same
        self$check_hash()
        time_changed  <- self$hash$time
        time_modified <- file.mtime(self$path)
        return(min(time_changed, time_modified, na.rm = TRUE)) # TODO: na.rm = ?
      } else {
        stop("Can't set `$last_changed")
      }
    }

  )
)

# file_node ---------------------------------------------------------------

#' @export
file_node <- R6::R6Class(

  classname = "file_node",
  inherit   = node,

  private = list(
    .vis_params_default = list(
      shape = "star"
    )
  ),

  public    = list(

    path    = NULL,
    r_expr  = NULL,
    hash    = NULL,

    initialize =
      function(
        ...,
        path     = NULL,
        r_code   = NULL,
        r_expr   = NULL,  # R expression (from r_code)
        hash     = NULL,
        type     = NULL,
        store    = TRUE
      ) {
        super$initialize(..., store = FALSE)
        log_record(self, self$id, "file class initialization")

        self$r_expr <- as_r_expr(firstnotnull(r_expr, r_code))

        if (is.null(self$r_expr) && length(self$depends))
          warning(self$id, " is not a leaf node but has no R expression to evaluate")

        if (length(path)) {
          self$path <- as.character(path[1])
        } else {
          stop(self$id, ": missing file path.")
        }

        if (length(hash)) {
          self$hash <- hash
        }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    update_definition =
      function(
        ...,
        path    = NULL,
        r_code  = NULL,
        r_expr  = NULL,
        store   = TRUE,
        verbose = TRUE
      ) {
        super$update_definition(..., verbose = verbose, store = FALSE)

        if (!identical(self$path, path)) {
          if (verbose) notify_update(self$id, "file path")
          self$path <- path
          private$.trigger_defchange <- TRUE
        }

        r_expr <- as_r_expr(firstnotnull(r_expr, r_code))
        if (!identical(as.character(self$r_expr), as.character(r_expr))) {
          if (verbose) notify_update(self$id, "R expression")
          private$.trigger_defchange <- TRUE
        }
        self$r_expr <- r_expr # overwrite in case the srouce code has changed

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    store_state = function(public_fields = NULL, private_fields = NULL) {
      super$store_state(
        public_fields  = unique(c(public_fields, c("r_expr", "path", "hash"))),
        private_fields = private_fields
      )
    },

    check_hash = function() {
      if (!self$exists()) return(NA) # TODO: or NULL?

      hash <- digest::digest(object = self$path, file = TRUE, algo = "md5")
      changed <- !isTRUE(self$hash$hash == hash)

      if (changed) {
        self$hash <- list(
          hash = hash,
          time = Sys.time()
        )

        if (self$persistence$enabled) self$store_state()
      }

      return(changed)
    },

    eval = function(verbose = TRUE, verbose_prefix = "") {
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": Evaluating R expression:\n", sep = "")
        cat_with_prefix(
          crayon::cyan(
            deparse_nicely(self$r_expr)
          ),
          prefix = paste0(verbose_prefix, "  ")
        )
      }

      # for referencing other objects in rflow
      .RFLOW <- parent.env(self)

      eval(self$r_expr) # TODO: explicitly specify some other envir for evaluation?

      # checking hash before signalling change to parent
      changed <- self$check_hash()

      private$.last_evaluated <- Sys.time()

      # all triggers should be resetted now
      self$reset_triggers()

      # make changes persistent
      if (self$persistence$enabled) self$store_state()

      return(changed)
    },

    exists = function() {
      file.exists(self$path)
    },

    check_triggers = function(verbose = TRUE, verbose_prefix = "") {

      if (!isFALSE(super$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) return(TRUE)
      if (!isTRUE(self$exists())) {if (verbose) notify_trigger(self$id, "missing target/value", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

      return(FALSE)
    },

    remove = function(verbose = TRUE, verbose_prefix = "") {
      if (self$exists()) {
        if (verbose) notify_removal(self$id, verbose_prefix = verbose_prefix)
        return(invisible(file.remove(self$path)))
      } else {
        if (verbose) notify_nonexistence(self$id, verbose_prefix = verbose_prefix)
        return(invisible(FALSE))
      }
    },

    title = function() {
      title <- super$title()

      title <- paste0(title, "<font size=\"-2\">", "path: ", self$path, "</font><br>")

      if (length(self$r_expr)) {
        title <- paste0(
          title,
          "<p>",
          "R:<br><font size=\"-2\" face = \"monospace\">",
          stringr::str_replace_all(
            stringr::str_replace_all(
              stringr::str_c(deparse_nicely(self$r_expr), collapse = "\n"),
              stringr::fixed("\n"), "<br>"),
            stringr::fixed(" "), "&nbsp;"),
          "</font></p>"
        )
      }

      return(title)
    },

    print = function(...) {
      super$print()
      cat("  path: ", self$path, "\n", sep = "")
      cat("  expression: \n")
      cat_with_prefix(
        crayon::cyan(
          deparse_nicely(self$r_expr)
        ),
        prefix = "    "
      )
    }

  ),

  active = list(

    last_evaluated = function(value) {
      if (missing(value)) {
        return(private$.last_evaluated)
      } else {
        private$.last_evaluated <- value
      }
    },

    last_changed = function(value) {
      if (missing(value)) {
        # file might have been modified but the content remained the same
        self$check_hash()
        time_changed  <- self$hash$time
        time_modified <- file.mtime(self$path)
        private$.last_changed <- min(time_changed, time_modified, na.rm = TRUE) # TODO: na.rm = ?
        return(private$.last_changed)
      } else {
        stop("Can't set `$last_changed")
      }
    }
  )
)


# csv_node ----------------------------------------------------------------
# basically a file node with get() function implemented
#' @export
csv_node <- R6::R6Class(

  classname = "csv_node",
  inherit   = file_node,

  public = list(
    read_args = NULL, # are passed to read functions

    initialize =
      function(
        ...,
        read_args = NULL,
        store     = TRUE
      ) {
        super$initialize(..., store = FALSE)
        log_record(self, self$id, "csv_file class initialization")

        self$read_args <- read_args

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    store_state = function(public_fields = NULL, private_fields = NULL) {
      super$store_state(
        public_fields  = unique(c(public_fields, "read_args")),
        private_fields = private_fields
      )
    },

    get = function(...) {
      if (self$exists()) {
        log_record(self, "Reading from csv file.")
        do.call(
          data.table::fread,
          args = union.list(
            self$read_args,
            list(file = self$path, ...)
          )
        )
      } else stop(self$id, ": target file '", self$path, " does not exists.")
    }
  )
)


# py_node -----------------------------------------------------------------

#' @export
py_node <- R6::R6Class(
  classname = "py_node",
  inherit = node,

  private = list(
    .vis_params_default = list(
      shape = "square"
    )
  ),

  public = list(

    cache = list(enabled = FALSE),
    py_code = NULL,

    hash    = NULL,  # hash of the represented R object from digest(), hashing enables checking for changes of the R objects

    initialize =
      function(
        ...,
        py_code  = NULL,
        type    = NULL,
        store   = TRUE,
        cache   = list(enabled = FALSE),
        hash    = NULL,

        verbose = TRUE
      ) {
        super$initialize(..., store = FALSE)
        log_record(self, self$id, "py_node class initialization")

        self$py_code <- as.character(py_code)

        if (!length(self$py_code) && length(self$depends))
          warning(self$id, " is not a leaf node but has no R expression to evaluate")

        # caching properties
        self$cache_setup(cache)

        # hash
        if (length(hash) && isTRUE(self$cache$enabled)) {
          self$hash <- hash
        }

        # try restoring the object from cache
        if (self$cache$enabled)
          if (self$cache_exists()) {
            tryCatch(
              {
                self$cache_restore()
                # self$check_hash()
              },
              error = function(e) {
                warning("Cache for ", self$id, " could not be recovered.\n")
                self$hash <- NULL # any hash loaded from stored state is meaningless now
              }
            )
          } else {
            if (verbose) cat(crayon::red(self$id), ": no cache found\n", sep = "")
          }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    cache_setup = function(cache) {

      log_record(self, self$id, "Setting up cache")

      self$cache <-
        if (is.list(cache) && length(cache$path)) {
          if (dir.exists(cache$path)) {
            list(
              enabled = TRUE,
              path    = cache$path,
              file    = filename_from_id(self$id, ext = "pickle")
            )
          } else stop("Cannot setup a cache file: ", cache$path, " does not exist.")
        } else if (is.character(cache)) {
          if (dir.exists(cache)) {
            list(
              enabled = TRUE,
              path    = cache,
              file    = filename_from_id(self$id, ext = "pickle")
            )
          } else stop(cache, " does not exist.")
        } else {
          list(
            enabled = FALSE
          )
        }

      invisible(TRUE)
    },

    cache_exists = function() {
      file.exists(file.path(self$cache$path, filename_from_id(self$id, value_hash = self$hash$hash, ext = "pickle")))
    },

    cache_write = function() {
      log_record(self, self$id, "Writing cache")
      reticulate::py_save_object(object = self$getref(), filename = file.path(self$cache$path, filename_from_id(self$id, value_hash = self$hash$hash, ext = "pickle")))
    },

    cache_restore = function(delayed = getOption("RFLOW_DELAYED_CACHE_LOAD", default = TRUE)) {
      # if (isTRUE(delayed)) warning("Delayed loading of Python objects is not supported currently.")
      log_record(self, self$id, "Restoring value from cache (delayed)")
      py <- reticulate::py
      py[[self$name]] <- reticulate::py_load_object(file.path(file.path(self$cache$path, filename_from_id(self$id, value_hash = self$hash$hash, ext = "pickle"))))
    },

    store_state = function(public_fields = NULL, private_fields = NULL) {
      super$store_state(
        public_fields  = unique(c(public_fields, c("py_code", "hash"))),
        private_fields = private_fields
      )
    },

    update_definition =
      function(
        ...,
        py_code  = NULL,
        store   = TRUE,
        verbose = TRUE
      ) {
        super$update_definition(..., verbose = verbose, store = FALSE)

        if (!identical(self$py_code, as.character(py_code))) {
          if (verbose) notify_update(self$id, "Python code")
          private$.trigger_defchange <- TRUE
        }
        self$py_code <- as.character(py_code) # overwrite in case the source code has changed

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    eval = function(verbose = TRUE, verbose_prefix = "") {
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": Evaluating R expression:\n", sep = "")
        cat_with_prefix(
          self$py_code,
          prefix = verbose_prefix
        )
      }

      log_record(self, self$id, "Evaluation started")
      reticulate::py_run_string(self$py_code, local = FALSE, convert = FALSE)
      private$.last_evaluated <- Sys.time()

      log_record(self, self$id, "Evaluation finished")

      # checking hash before signalling change to parent
      changed <- self$check_hash()

      if (changed) log_record(self, self$id, "Value has changed")
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": done", if (changed) crayon::yellow(" (value has changed)"), ".\n", sep = "")
      }

      if (self$cache$enabled && (changed || !isTRUE(self$cache_exists()))) self$cache_write()

      # all triggers should be resetted now
      self$reset_triggers()

      # make changes persistent
      if (self$persistence$enabled) self$store_state()

      return(changed)
    },

    exists = function() {
      isTRUE(self$name %in% names(reticulate::py))
    },

    check_hash = function() {
      if (!self$exists()) return(NA) # TODO: or NULL?

      log_record(self, self$id, "Computing hash")
      hash <- reticulate::py_eval(sprintf("hash(repr(%s))", self$name))
      changed <- !isTRUE(self$hash$hash == hash)

      if (changed)
        self$hash <- list(
          hash = hash,
          time = Sys.time()
        )

      log_record(self, self$id, "hash changed:", changed)
      if (self$persistence$enabled) self$store_state()

      return(changed)
    },

    check_triggers = function(verbose = TRUE, verbose_prefix = "") {

      if (!isFALSE(super$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) return(TRUE)
      if (!isTRUE(self$exists())) {if (verbose) notify_trigger(self$id, "missing target/value", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

      return(FALSE)
    },

    getref = function() {
      reticulate::py_eval(self$name, convert = FALSE)
    },

    get = function() {
      if (self$exists()) reticulate::py[[self$name]] else stop(self$id, " does not exists!")
    },

    remove = function(verbose = TRUE, verbose_prefix = "") {
      if (self$exists()) {
        if (verbose) notify_removal(self$id, verbose_prefix = verbose_prefix)
        reticulate::py_run_string(sprintf("del(%s)", self$name))
        return(invisible(TRUE))
      } else {
        if (verbose) notify_nonexistence(self$id, verbose_prefix = verbose_prefix)
        return(invisible(FALSE))
      }

    }
  ),

  active = list(

    last_evaluated = function(value) {
      if (missing(value)) {
        return(private$.last_evaluated)
      } else {
        private$.last_evaluated <- value
      }
    },

    last_changed = function(value) {
      if (missing(value)) {
        # file might have been modified but the content stayed the same
        self$check_hash()
        private$.last_changed <- self$hash$time
        return(private$.last_changed)
      } else {
        stop("Can't set `$last_changed")
      }
    }

  )

)



# julia node --------------------------------------------------------------


#' @export
julia_node <- R6::R6Class(
  classname = "julia_node",
  inherit = node,

  private = list(
    .vis_params_default = list(
      shape = "square"
    )
  ),

  public = list(
    cache = list(enabled = FALSE),
    julia_code = NULL,

    hash    = NULL,  # hash of the represented R object from digest(), hashing enables checking for changes of the R objects

    initialize =
      function(
        ...,
        julia_code  = NULL,
        type    = NULL,
        store   = TRUE,
        cache   = list(enabled = FALSE),
        hash    = NULL,

        verbose = TRUE
      ) {
        super$initialize(..., store = FALSE)
        log_record(self, self$id, "julia_node class initialization")

        self$julia_code <- as.character(julia_code)

        if (!length(self$julia_code) && length(self$depends))
          warning(self$id, " is not a leaf node but has no R expression to evaluate")

        # caching properties
        self$cache_setup(cache)

        # hash
        if (length(hash) && isTRUE(self$cache$enabled)) {
          self$hash <- hash
        }

        JuliaCall::julia_setup()
        JuliaCall::julia_library("JLD")

        # try restoring the object from cache
        if (self$cache$enabled)
          if (self$cache_exists()) {
            tryCatch(
              {
                self$cache_restore()
                # self$check_hash()
              },
              error = function(e) {
                warning("Cache for ", self$id, " could not be recovered.\n")
                self$hash <- NULL # any hash loaded from stored state is meaningless now
              }
            )
          } else {
            if (verbose) cat(crayon::red(self$id), ": no cache found\n", sep = "")
          }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    cache_setup = function(cache) {

      log_record(self, self$id, "Setting up cache")

      self$cache <-
        if (is.list(cache) && length(cache$path)) {
          if (dir.exists(cache$path)) {
            list(
              enabled = TRUE,
              path    = cache$path,
              file    = filename_from_id(self$id, ext = "jld")
            )
          } else stop("Cannot setup a cache file: ", cache$path, " does not exist.")
        } else if (is.character(cache)) {
          if (dir.exists(cache)) {
            list(
              enabled = TRUE,
              path    = cache,
              file    = filename_from_id(self$id, ext = "jld")
            )
          } else stop(cache, " does not exist.")
        } else {
          list(
            enabled = FALSE
          )
        }

      invisible(TRUE)
    },

    cache_exists = function() {
      file.exists(file.path(self$cache$path, filename_from_id(self$id, value_hash = self$hash$hash, ext = "jld")))
    },

    cache_write = function() {
      log_record(self, self$id, "Writing cache")
      jc <- sprintf('save("%s", "%s", %s)', file.path(self$cache$path, filename_from_id(self$id, value_hash = self$hash$hash, ext = "jld")), self$name, self$name)
      JuliaCall::julia_command(jc, show_value = FALSE)
    },

    cache_restore = function(delayed = getOption("RFLOW_DELAYED_CACHE_LOAD", default = TRUE)) {
      # if (isTRUE(delayed)) warning("Delayed loading of julia objects is not supported currently.")
      log_record(self, self$id, "Restoring value from cache (delayed)")
      jc <- sprintf('%s = JLD.load("%s", "%s")', self$name, file.path(self$cache$path, filename_from_id(self$id, value_hash = self$hash$hash, ext = "jld")), self$name)
      JuliaCall::julia_command(jc, show_value = FALSE)
    },

    store_state = function(public_fields = NULL, private_fields = NULL) {
      super$store_state(
        public_fields  = unique(c(public_fields, c("julia_code", "hash"))),
        private_fields = private_fields
      )
    },

    update_definition =
      function(
        ...,
        julia_code  = NULL,
        store   = TRUE,
        verbose = TRUE
      ) {
        super$update_definition(..., verbose = verbose, store = FALSE)

        if (!identical(self$julia_code, as.character(julia_code))) {
          if (verbose) notify_update(self$id, "Julia code")
          private$.trigger_defchange <- TRUE
        }
        self$julia_code <- as.character(julia_code) # overwrite in case the source code has changed

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    eval = function(verbose = TRUE, verbose_prefix = "") {
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": Evaluating R expression:\n", sep = "")
        cat_with_prefix(
          self$julia_code,
          prefix = verbose_prefix
        )
      }

      log_record(self, self$id, "Evaluation started")
      JuliaCall::julia_command(self$julia_code, show_value = FALSE)
      private$.last_evaluated <- Sys.time()

      log_record(self, self$id, "Evaluation finished")

      # checking hash before signalling change to parent
      changed <- self$check_hash()

      if (changed) log_record(self, self$id, "Value has changed")
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": done", if (changed) crayon::yellow(" (value has changed)"), ".\n", sep = "")
      }

      if (self$cache$enabled && (changed || !isTRUE(self$cache_exists()))) self$cache_write()

      # all triggers should be resetted now
      self$reset_triggers()

      # make changes persistent
      if (self$persistence$enabled) self$store_state()

      return(changed)
    },

    exists = function() {
      JuliaCall::julia_exists(self$name)
    },

    get = function() {
      JuliaCall::julia_eval(self$name, need_return = "R")
    },

    getref = function() {
      JuliaCall::julia_eval(self$name, need_return = "Julia")
    },

    check_hash = function() {
      if (!self$exists()) return(NA) # TODO: or NULL?

      log_record(self, self$id, "Computing hash")
      hash <- JuliaCall::julia_eval(sprintf("string(hash(repr(%s)))", self$name), need_return = "R")
      changed <- !isTRUE(self$hash$hash == hash)

      if (changed)
        self$hash <- list(
          hash = hash,
          time = Sys.time()
        )

      log_record(self, self$id, "hash changed:", changed)
      if (self$persistence$enabled) self$store_state()

      return(changed)
    },

    check_triggers = function(verbose = TRUE, verbose_prefix = "") {

      if (!isFALSE(super$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) return(TRUE)
      if (!isTRUE(self$exists())) {if (verbose) notify_trigger(self$id, "missing target/value", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

      return(FALSE)
    },

    remove = function(verbose = TRUE, verbose_prefix = "") {
      if (self$exists()) {
        if (verbose) notify_removal(self$id, verbose_prefix = verbose_prefix)
        warning("There is no way to remove a variable in Julia!")
        return(invisible(FALSE))
      } else {
        if (verbose) notify_nonexistence(self$id, verbose_prefix = verbose_prefix)
        return(invisible(FALSE))
      }

    }
  ),

  active = list(

    last_evaluated = function(value) {
      if (missing(value)) {
        return(private$.last_evaluated)
      } else {
        private$.last_evaluated <- value
      }
    },

    last_changed = function(value) {
      if (missing(value)) {
        # file might have been modified but the content stayed the same
        self$check_hash()
        private$.last_changed <- self$hash$time
        return(private$.last_changed)
      } else {
        stop("Can't set `$last_changed")
      }
    }
  )
)

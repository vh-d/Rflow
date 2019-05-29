# node --------------------------------------------------------------------

#' @title node class
#' @description basic building block of an rflow
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
#' node("node1", desc = "This an example node which does nothing")
#' @section Methods:
#' \describe{
#'   \item{\code{new(node)}}{This method is used to create object of this class.}
#' }
node <- R6::R6Class(

  classname = "node",

  private = list(
    .last_evaluated = NULL, # datetime when target value was last evaluated/made
    .last_changed   = NULL  # datetime when target value changed the last time (it might be evaluated without any change)
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

    persistence        = NULL,
    trigger_defchange  = FALSE,
    trigger_manual     = FALSE,
    trigger_condition  = NULL,
    # last_evaluated = NULL,

    # derive object's ID from given identificators
    set_id = function(id, name, env) {
      if (is.null(id) & (is.null(name) | is.null(env))) stop("Missing id or (env + name)!") # either 'id' or 'name' + 'env' arguments have to be provided
      self$id <- if (is.null(id)) paste0(env, ".", name) else id
      # set_persistence(self$persistence)
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
      # invisible(self)
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
        persistence = list(enabled = FALSE),
        .last_evaluated = NULL,
        .last_changed   = NULL,
        store   = TRUE,
        ...
      ) {
        self$set_id(id = id, name = name, env = env)

        self$name    <- name
        self$env     <- env
        self$desc    <- desc

        self$set_persistence(persistence)

        self$tags    <- as.character(tags)

        depends_char <- if (is.character(depends)) depends else names(depends)
        self$depends <- depends_char

        if (length(trigger_defchange))
          self$trigger_defchange <- as.logical(trigger_defchange)

        if (!is.null(trigger_condition))
          self$trigger_condition <- as_r_expr(r_code = trigger_condition)

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
      private_fields = NULL) {

      public_fields  <- unique(c("id", "name", "env", "desc", "tags", "depends", "trigger_condition", "trigger_defchange", public_fields))
      private_fields <- unique(c(".last_evaluated", ".last_changed", private_fields))

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
        trigger_defchange = NULL,
        trigger_condition = NULL,

        ...,
        store  = TRUE,
        verbose = TRUE
      ) {
        if (!is.null(trigger_defchange))
          self$trigger_defchange <- trigger_defchange

        # changes in dependencies
        depends_char <- if (is.character(depends)) depends else names(depends)
        if (!setequal(self$depends, depends_char)) {

          if (verbose) notify_update(self$id, "dependencies")

          self$depends <- depends_char
          self$trigger_defchange <- TRUE
        }

        # changes in description
        if (!is.null(desc)) self$desc <- desc

        # changes in tags
        if (!identical(as.character(self$tags), as.character(tags))) {
          if (verbose) notify_update(self$id, "tags")
          self$tags <- as.character(tags)
        }

        # changes in user defined trigger
        trigger_condition <- suppressWarnings(as_r_expr(r_code = trigger_condition))
        if (!identical(as.character(self$trigger_condition), as.character(trigger_condition))) {
          if (verbose) notify_update(self$id, "trigger condition")
          self$trigger_condition <- trigger_condition
          self$trigger_defchange <- TRUE
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
      if (verbose) cat("Connecting ", crayon::red(self$id), " to: ", crayon::red(self$depends, collapse = " "), "\n", sep = "")
      sapply(self$depends, self$connect_to)
    },

    get = function() {
      warning("This is just empty method...")
    },

    remove = function() {
      warning("This is just empty method...")
    },

    check_trigger_condition = function() {
      isTRUE(eval(self$trigger_condition, envir = self))
    },

    # run checks that triggers evaluation
    # note that the order of checking the triggers is important (short circuit)
    check_triggers = function(verbose = TRUE, verbose_prefix = "") {
      if (isNotFALSE(self$trigger_defchange))         {if (verbose) notify_trigger(self$id, "change in eval. expression", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}
      if (isNotFALSE(self$trigger_manual))            {if (verbose) notify_trigger(self$id, "manual trigger", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}
      if (isNotFALSE(self$check_trigger_condition())) {if (verbose) notify_trigger(self$id, "custom trigger condition", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

      if (!length(self$last_evaluated) || is.na(self$last_evaluated)) {if (verbose) notify_trigger(self$id, "unknown datetime of last eval", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

      return(FALSE)
    },

    # for resetting triggers after successfull evaluation
    reset_triggers = function() {
      self$trigger_defchange <- FALSE
      self$trigger_manual <- FALSE
    },

    # main evaluation function
    eval = function() {

    },

    # generic make function that recursivelly solves all dependencies
    # in most cases, this is not supposed to be overloaded in custom subclasses
    make = function(force = FALSE, verbose = TRUE, verbose_prefix = "") {

      if (verbose) {
        cat(verbose_prefix, "\u250C Solving node ", crayon::red(crayon::bold(self$id)), "\n", sep = "")
      }

      # solve/make upstream nodes first
      if (length(self$upstream) != length(self$depends)) self$connect() # check that it's connected to upstream

      results <- FALSE
      upstream_changed_ids <- character()
      for (y in self[["upstream"]]) {
        trigger_upstream_changed <- y$make(force = force, verbose = verbose, verbose_prefix = paste0(verbose_prefix, "\u2502  "))
        trigger_upstream_newer   <- y$last_changed > self$last_evaluated

        # increment list of changed nodes in upstream
        # there is another trigger for cases when self$last_evaluated in NULL/NA, therefore isTRUE() should be safe and is better for logging
        if (isNotFALSE(trigger_upstream_newer) || isTRUE(trigger_upstream_changed))
          upstream_changed_ids <- c(upstream_changed_ids, y$id)

        results <- results || trigger_upstream_newer || trigger_upstream_changed
      }

      triggered <-
        if (isTRUE(force)) {
          if (verbose) notify_trigger(self$id, "manual trigger", verbose_prefix = paste0(verbose_prefix, "\u2514 "))
          TRUE
        } else if (!length(self$last_evaluated) || is.na(self$last_evaluated)) {
          if (verbose) notify_trigger(self$id, "unknown datetime of last eval", verbose_prefix = paste0(verbose_prefix, "\u2514 "))
          TRUE
        } else if (isNotFALSE(results)) {
          if (verbose) notify_trigger(self$id, paste0("changes in upstream nodes (", paste0(upstream_changed_ids, collapse = ", "), ")"), verbose_prefix = paste0(verbose_prefix, "\u2514 "))
          TRUE
        } else if (isNotFALSE(self$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) {
          TRUE
        } else {
          if (verbose) cat(verbose_prefix, "\u2514 ", crayon::silver(self$id, " not triggered.", sep = ""), "\n", sep = "")
          return(invisible(FALSE))
        }

      # then make the object itself
      # eval should return if the object was evaluated/changed etc...
      trigger_downstream <- isNotFALSE(self$eval(verbose = verbose, verbose_prefix = paste0(verbose_prefix, "\u2502  ")))

      # all triggers should be resetted now
      self$reset_triggers()

      # return whether dependants should be triggered or not
      return(invisible(trigger_downstream))
    }
  ) ,

  active = list(

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
    }
  )
)


# r_node ------------------------------------------------------------------

#' @export
r_node <- R6::R6Class(
  classname = "r_node",
  inherit = node,

  public = list(

    r_env    = NULL,  # reference to R envinronment
    r_code   = NULL,  # R code text
    r_expr   = NULL,  # R expression (from r_code)

    hash     = NULL,  # hash of the represented R object from digest(), hashing enables checking for changes of the R objects
    cache    = list(enabled = FALSE),

    # triggers  = NULL, # every node has a list of triggers that are checked before evaluation

    cache_setup = function(cache) {
      self$cache <-
        if (is.list(cache) && length(cache$path)) {
          if (dir.exists(cache$path)) {
            list(
              enabled = TRUE,
              path    = cache$path,
              file    = filename_from_id(self$id)
            )
          } else stop(cache$path, " does not exist.")
        } else if (is.character(cache)) {
          if (dir.exists(cache)) {
            list(
              enabled = TRUE,
              path    = cache,
              file    = filename_from_id(self$id)
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
      file.exists(file.path(self$cache$path, self$cache$file))
    },

    cache_write = function() {
      saveRDS(object = self$get(), file = file.path(self$cache$path, self$cache$file))
    },

    cache_restore = function() {
      value <- readRDS(file.path(self$cache$path, self$cache$file))
      assign(self$name, value, pos = self$r_env)
    },

    print = function(...) {
      super$print()
      cat("  expression: ", crayon::cyan(head(as.character(self$r_expr))), "\n", sep = "")
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

        self$r_code <- r_code
        self$r_expr <- suppressWarnings(as_r_expr(r_code = r_code, r_expr = r_expr))

        if (is.null(self$r_expr) && length(self$depends))
          warning(self$id, " is not a leaf node but has no R expression to evaluate")

        # caching properties
        self$cache_setup(cache)

        # hash
        if (length(hash) && self$cache$enabled) {
          self$hash <- hash
        }

        # connect to R environment (.GlobalEnv if not specified)
        if (is.null(self$env)) {
          self$r_env <- .GlobalEnv
        } else {
          r_env <- get(self$env, .GlobalEnv) # other than .GlobalEnv?
          if (!is.environment(r_env)) stop(paste(self$env, "is not an R environment object"))
          self$r_env <- r_env
        }

        # try restoring the object from cache
        if (self$cache$enabled)
          if (self$cache_exists()) {
            tryCatch(
              {
                self$cache_restore()
                self$check_hash()
              },
              error = function(e) {
                warning("Cache for self$id could not be recovered.\n")
                self$hash <- NULL # any hash loaded from stored state is meaningless now
              }
            )
          } else {
            if (verbose) cat(crayon::red(self$id), ": no cache found\n", sep = "")
          }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    store_state = function() {
      super$store_state(
        public_fields  = c("r_code", "r_expr", "hash")
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

        r_expr <- suppressWarnings(as_r_expr(r_code = r_code, r_expr = r_expr))
        if (!identical(as.character(self$r_expr), as.character(r_expr))) {
          if (verbose) notify_update(self$id, "R expression")
          self$r_expr <- r_expr
          self$trigger_defchange <- TRUE
        }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    eval = function(verbose = TRUE, verbose_prefix = "") {
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": Evaluating R expression:\n", sep = "")
        cat_r_expr(self$r_expr, verbose_prefix)
      }

      # for referencing other objects in rflow
      .RFLOW <- parent.env(self)

      assign(self$name, eval(self$r_expr), pos = self$r_env)

      private$.last_evaluated <- Sys.time()

      # checking hash before signalling change to parent
      changed <- self$check_hash()

      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": done", if (changed) crayon::yellow(" (value has changed)"), ".\n", sep = "")
      }

      if (self$cache$enabled && (changed || isNotTRUE(self$cache_exists()))) self$cache_write()
      if (self$persistence$enabled) self$store_state()

      return(changed)
    },

    exists = function() {
      exists(self$name, where = self$r_env)
    },

    check_hash = function() {
      if (!self$exists()) return(NA) # TODO: or NULL?

      hash <- digest::digest(object = self$get(), file = FALSE, algo = "md5")
      changed <- isNotTRUE(self$hash$hash == hash)

      if (changed)
        self$hash <- list(
          hash = hash,
          time = Sys.time()
        )

      return(changed)
    },

    check_triggers = function(verbose = TRUE, verbose_prefix = "") {
      if (isNotFALSE(super$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) return(TRUE)
      if (isNotTRUE(self$exists())) {if (verbose) notify_trigger(self$id, "missing target/value", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

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

#' @export
db_node <- R6::R6Class(
  classname = "db_node",
  inherit   = node,

  public    = list(

    env        = NULL,

    mode       = NULL,
    driver     = NULL,
    con_code   = NULL,
    connection = NULL,
    sql        = NULL,

    r_code     = NULL,
    r_expr     = NULL,

    auto_remove = NULL,

    initialize =
      function(
        ...,
        auto_remove = TRUE,
        sql_code = NULL,
        sql      = NULL,
        r_code   = NULL,
        r_expr   = NULL,  # R expression (from r_code)
        con_code = NULL,
        connection = NULL,
        .last_evaluated = NULL,
        type     = NULL,
        store    = TRUE
      ) {
        super$initialize(..., store = FALSE)

        # TODO:
        # * how to handle storage of connection when DBI connection object is given? One solution is to only allow con_code?
        # * tryCatch connection to avoid failure during update
        self$con_code   <- con_code
        self$connection <- if (is.null(connection) & length(self$con_code)) eval(parse(text = self$con_code)) else connection

        self$auto_remove <- auto_remove

        # TODO: we need to handle situations when node is modified from R job to SQL job
        if (length(r_expr)) {
          self$mode <- "R"
          self$r_expr <- r_expr
        } else if (length(r_code)) {
          self$mode <- "R"
          self$r_code <- r_code
          self$r_expr <- parse(text = r_code)
        } else if (length(sql)) {
          self$sql <- sql_structure(sql)
          self$mode <- "SQL"
        } else if (length(sql_code)) {
          self$sql <- sql_structure(sql_code)
          self$mode <- "SQL"
        } else
          warning(id, ": no R expression/code or SQL code!")

        private$.last_evaluated <- if (length(.last_evaluated)) .last_evaluated else as.POSIXct(NA)

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    store_state = function() {
      super$store_state(
        public_fields  = c("con_code", "r_expr", "r_code", "sql", "mode", "auto_remove")
      )
    },

    update_definition =
      function(
        ...,
        auto_remove = NULL,
        con_code = NULL,
        sql_code = NULL,
        sql      = NULL,
        r_code   = NULL,
        r_expr   = NULL,  # R expression (from r_code)
        store    = TRUE,
        verbose  = FALSE
      ) {
        super$update_definition(..., verbose = verbose, store = FALSE)

        if (!identical(self$con_code, con_code)) {
          self$con_code <- con_code
          self$trigger_defchange <- TRUE
          self$connection <- if (length(self$con_code)) eval(parse(text = self$con_code)) else connection
        }

        if (length(auto_remove) && !identical(self$auto_remove, auto_remove)) {
          self$auto_remove <- auto_remove
          self$trigger_defchange <- TRUE
        }

        # TODO: we need to handle situations when node is modified from R job to SQL job
        if (length(r_expr)) {
          mode <- "R"
          r_expr <- r_expr
        } else if (length(r_code)) {
          mode <- "R"
          r_expr <- parse(text = r_code)
        } else if (length(sql)) {
          sql <- sql_structure(sql)
          mode <- "SQL"
        } else if (length(sql_code)) {
          sql <- sql_structure(sql_code)
          mode <- "SQL"
        } else
          warning(id, ": no R expression/code or SQL code!")

       if (!identical(self$mode, mode)) {
          if (verbose) notify_update(self$id, "R/SQL mode")
          self$mode <- mode
          self$trigger_defchange <- TRUE
        }

        if (!identical(as.character(self$r_expr), as.character(r_expr))) {
          if (verbose) notify_update(self$id, "R expression")
          self$r_expr <- r_expr
          self$trigger_defchange <- TRUE
        }

        if (!identical(self$sql,sql)) {
          if (verbose) notify_update(self$id, "SQL expressions")
          self$sql <- sql
          self$trigger_defchange <- TRUE
        }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    execute_sql = function(verbose = TRUE, verbose_prefix = "") {
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": Evaluating SQL statements:\n", sep = "")
      }
      sapply(self$sql,
             function(sql_statement) {
               if (verbose) {
                 cat_r_expr(sql_statement$code, verbose_prefix = paste0(verbose_prefix, "  "))
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
          cat_r_expr(self$r_expr, verbose_prefix = verbose_prefix_inc)
        }
        eval(self$r_expr) # TODO: explicitly specify some other envir for evaluation?
      }

      private$.last_evaluated <- Sys.time()

      if (self$persistence$enabled) self$store_state()

      return(TRUE)
    },

    exists = function() {
      DBI::dbExistsTable(conn = self$connection, name = self$name)
    },

    check_triggers = function(verbose = TRUE, verbose_prefix = "") {
      if (isNotFALSE(super$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) return(TRUE)
      if (isNotTRUE(self$exists())) {if (verbose) notify_trigger(self$id, "missing target/value", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

      return(FALSE)
    },

    print = function(...) {
      super$print()
      cat("  DBI: ", class(self$connection)[1], "\n", sep = "")
      if (length(self$con_code))
        cat("  Con. code: ", class(self$con_code), "\n", sep = "")
      if (length(self$sql_code))
        cat("  SQL code:\n   ", crayon::cyan(head(paste0(self$sql_code, collapse = "; \n\n"))), "\n", sep = "") else
          cat("  R expression:\n   ", crayon::cyan(head(as.character(self$r_expr))), "\n", sep = "")
    },

    print_sql = function(prefix = "") {
      cat(prefix, crayon::cyan(paste_sql(self$sql)), sep = "")
    },

    get = function() {
      if (self$exists()) {
        return(DBI::dbReadTable(conn = self$connection, name = self$name))
      } else {
        warning("Object ", self$id, " (", self$name, ")", " does not exist.")
        return(NULL)
      }
    },

    remove = function(verbose = TRUE, verbose_prefix = "") {
      if (self$exists()) {
        if (verbose) notify_removal(self$id, verbose_prefix = verbose_prefix)
        return(invisible(DBI::dbRemoveTable(conn = self$connection, name = self$name)))
      } else {
        if (verbose) notify_nonexistence(self$id, verbose_prefix = verbose_prefix)
        return(invisible(FALSE))
      }
    }

  )
)


# accdb_node --------------------------------------------------------------

#' @export
accdb_node <- R6::R6Class(
  classname = "accdb_node",
  inherit   = db_node,

  public    = list(

    exists = function() {
      isTRUE(self$name %in% odbc32::sqlTables(self$connection)$TABLE_NAME)
    },

    get = function() {
      if (self$exists()) {
        return(odbc32::sqlFetch(con = self$connection, name = self$name))
      } else {
        warning("Object ", self$id, " (", self$name, ")", " does not exist.")
        return(NULL)
      }
    },

    remove = function(verbose = TRUE, verbose_prefix = "") {
      if (self$exists()) {
        if (verbose) notify_removal(self$id, verbose_prefix = verbose_prefix)
        return(invisible(odbc32::sqlDrop(con = self$connection, name = self$name)))
      } else {
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
                 cat_r_expr(sql_statement$code, verbose_prefix = paste0(verbose_prefix, "  "))
               }
               tryCatch(odbc32::sqlQuery(con = self$connection, query = sql_statement$code),
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
        hash    = NULL,
        store   = TRUE,
        verbose = TRUE
      ) {
        super$update_definition(..., verbose = verbose, store = FALSE)

        if (!identical(self$path, path)) {
          if (verbose) notify_update(self$id, "file path")
          self$path <- path
          self$trigger_defchange <- TRUE
        }

        if (!identical(self$sheet, sheet)) {
          if (verbose) notify_update(self$id, "sheet name/index")
          self$sheet <- sheet
          self$trigger_defchange <- TRUE
        }

        if (length(hash)) {
          self$hash <- hash
        }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    store_state = function() {
      super$store_state(
        public_fields  = c("path", "sheet", "hash")
      )
    },

    eval = function(...) {
      # warning(self$id, " is read only node")
    },

    exists = function() {
      if (file.exists(self$path)) {
        if (is.numeric(self$sheet)) {
          return(isTRUE(as.integer(self$sheet) %in% seq_along(openxlsx::getSheetNames(self$path))))
        } else
          return(isTRUE(self$sheet %in% openxlsx::getSheetNames(self$path)))
      }
    },

    check_hash = function() {
      if (!self$exists()) return(NA) # TODO: or NULL?

      hash <- digest::digest(object = self$get(), algo = "md5")
      changed <- isNotTRUE(self$hash$hash == hash)

      if (changed)
        self$hash <- list(
          hash = hash,
          time = Sys.time()
        )

      return(changed)
    },

    check_triggers = function(verbose = TRUE, verbose_prefix = "") {
      if (isNotFALSE(super$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) return(TRUE)
      if (isNotTRUE(self$exists())) {if (verbose) notify_trigger(self$id, "missing target/value", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

      return(FALSE)
    },

    get = function() {
      if (self$exists()) {
        do.call(openxlsx::read.xlsx, args = c(list(xlsxFile = self$path, sheet = self$sheet), self$read_args))
      } else stop(self$id, ": sheet '", self$sheet, "' does not exists in ", self$path)
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

  public    = list(

    path    = NULL,
    r_code  = NULL,
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

        self$r_code <- r_code
        self$r_expr <- suppressWarnings(as_r_expr(r_code = r_code, r_expr = r_expr))

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
        hash    = NULL,
        store   = TRUE,
        verbose = TRUE
      ) {
        super$update_definition(..., verbose = verbose, store = FALSE)

        if (!identical(self$path, path)) {
          if (verbose) notify_update(self$id, "file path")
          self$path <- path
          self$trigger_defchange <- TRUE
        }

        r_expr <- suppressWarnings(as_r_expr(r_code = r_code, r_expr = r_expr))
        if (!identical(as.character(self$r_expr), as.character(r_expr))) {
          if (verbose) notify_update(self$id, "R expression")
          self$r_expr <- r_expr
          self$trigger_defchange <- TRUE
        }

        if (length(hash)) {
          self$hash <- hash
        }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    store_state = function() {
      super$store_state(
        public_fields  = c("r_expr", "path", "hash")
      )
    },

    check_hash = function() {
      if (!self$exists()) return(NA) # TODO: or NULL?

      hash <- digest::digest(object = self$path, file = TRUE, algo = "md5")
      changed <- isNotTRUE(self$hash$hash == hash)

      if (changed)
        self$hash <- list(
          hash = hash,
          time = Sys.time()
        )

      return(changed)
    },

    eval = function(verbose = TRUE, verbose_prefix = "") {
      # exists_check <- self$exists()

      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": Evaluating R expression:\n", sep = "")
        cat_r_expr(self$r_expr, paste0(verbose_prefix, "  "))
      }

      # for referencing other objects in rflow
      .RFLOW <- parent.env(self)

      eval(self$r_expr) # TODO: explicitly specify some other envir for evaluation?

      # checking hash before signalling change to parent
      changed <- self$check_hash()

      private$.last_evaluated <- Sys.time()

      if (self$persistence$enabled) self$store_state()

      return(changed)
    },

    exists = function() {
      file.exists(self$path)
    },

    check_triggers = function(verbose = TRUE, verbose_prefix = "") {
      if (isNotFALSE(super$check_triggers(verbose = verbose, verbose_prefix = verbose_prefix))) return(TRUE)
      if (isNotTRUE(self$exists())) {if (verbose) notify_trigger(self$id, "missing target/value", verbose_prefix = paste0(verbose_prefix, "\u2514 ")); return(TRUE)}

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
        time_changed  <- self$hash$time
        time_modified <- file.mtime(self$path)
        return(min(time_changed, time_modified, na.rm = TRUE)) # TODO: na.rm = ?
      } else {
        stop("Can't set `$last_changed")
      }
    }
  )
)


# csv_node ----------------------------------------------------------------

csv_node <- R6::R6Class(

  classname = "csv_node",
  inherit   = file_node,

  public = list(
    read_args = NULL,

    initialize =
      function(
        ...,
        store     = TRUE,
        read_args = NULL
      ) {
        super$initialize(..., store = FALSE)

        self$read_args <- read_args

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    get = function() {
      do.call(
        if (requireNamespace("data.table")) data.table::fread else read.csv,
        args = c(
          list(file = self$path),
          self$read_args
        )
      )
    }
  )
)


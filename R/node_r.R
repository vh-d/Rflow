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

    process = function(verbose = TRUE, verbose_prefix = "") {

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
      
      .DATA <- function(x) {
        .RFLOW[[x]]$value
      }
      
      .NODES <- function(x) {
        .RFLOW[[x]]
      }
      
      assign(self$name, eval(self$r_expr), pos = self$r_env)
    },

    changed = function(verbose = TRUE, verbose_prefix = "") {

      # checking hash before signalling change to parent
      haschanged <- self$check_hash()

      if (haschanged) log_record(self, self$id, "Value has changed")
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": done", if (haschanged) crayon::yellow(" (value has changed)"), ".\n", sep = "")
      }

      if (self$cache$enabled && (haschanged || !isTRUE(self$cache_exists()))) self$cache_write()

      return(haschanged)
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

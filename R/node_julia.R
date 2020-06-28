
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

    process = function(verbose = TRUE, verbose_prefix = "") {
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": Evaluating Julia code:\n", sep = "")
        cat_with_prefix(
          self$julia_code,
          prefix = verbose_prefix
        )
      }

      JuliaCall::julia_command(self$julia_code, show_value = FALSE)
    },

    changed = function(verbose = TRUE, verbose_prefix = "") {
      # checking hash before signalling change to parent
      changed <- self$check_hash()

      if (changed) log_record(self, self$id, "Value has changed")
      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": done", if (changed) crayon::yellow(" (value has changed)"), ".\n", sep = "")
      }

      if (self$cache$enabled && (changed || !isTRUE(self$cache_exists()))) self$cache_write()

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

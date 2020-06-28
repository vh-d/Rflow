
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

    process = function(verbose = TRUE, verbose_prefix = "") {
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

      .DATA <- function(x) {
        .RFLOW[[x]]$value
      }
      
      .NODES <- function(x) {
        .RFLOW[[x]]
      }
      
      eval(self$r_expr) # TODO: explicitly specify some other envir for evaluation?
    },

    changed = function(verbose = TRUE, verbose_prefix = "") {
      self$check_hash()
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


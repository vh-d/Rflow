# excel sheet node ---------------------------------------------------------
#' @export
excel_sheet <- R6::R6Class(

  classname = "excel_sheet",
  inherit   = node,

  public    = list(

    path   = NULL,
    sheet  = NULL,
    hash   = NULL,
    read_args = NULL,

    initialize =
      function(
        ...,
        path      = NULL,
        sheet     = 1L, # not in file_node
        read_args = NULL,
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
        read_args = NULL,
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

        if (!identical(self$read_args, read_args)) {
          if (verbose) notify_update(self$id, "read options")
          self$read_args <- read_args
          # private$.trigger_defchange <- TRUE  # TODO: this should trigger dependent nodes
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
        return(min(time_changed, time_modified, na.rm = TRUE))
      } else {
        stop("Can't set `$last_changed")
      }
    }

  )
)

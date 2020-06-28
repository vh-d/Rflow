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

    process = function(verbose = TRUE, verbose_prefix = "") {
      verbose_prefix_inc <- paste0(verbose_prefix, "  ")

      exists_check <- self$exists()

      # for referencing other objects in rflow
      .RFLOW <- parent.env(self)

      .DATA <- function(x) {
        .RFLOW[[x]]$value
      }
      
      .NODES <- function(x) {
        .RFLOW[[x]]
      }
      
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
    },

    changed = function(verbose = TRUE, verbose_prefix = "") {
      # currently no way to check... maybe give the user an option to run some custom query?
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


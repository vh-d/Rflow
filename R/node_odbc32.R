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

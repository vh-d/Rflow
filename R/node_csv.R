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
    
    update_definition =
      function(
        ...,
        read_args = NULL,
        store   = TRUE,
        verbose = TRUE
      ) {
        super$update_definition(..., verbose = verbose, store = FALSE)

        if (!identical(self$read_args, read_args)) {
          if (verbose) notify_update(self$id, "parameters for reading")
          self$read_args <- read_args
        }
        
        if (self$persistence$enabled && store) self$store_state()
        
        return(invisible(TRUE)) 
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


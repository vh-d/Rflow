

# rmd_node ---------------------------------------------------------

#' @export
rmd_node <- R6::R6Class(

  classname = "rmd_node",
  inherit   = file_node,

  public = list(

    path_rmd = NULL,
    knit_args = NULL,
    output_format = NULL,

    initialize =
      function(
        ...,
        path_rmd  = NULL,
        knit_args = NULL,
        output_format= NULL,
        store     = TRUE
      ) {
        if (!requireNamespace("rmarkdown")) stop("rmarkdown package is required to evaluate rmd_node.")

        super$initialize(..., store = FALSE)
        log_record(self, self$id, "rmd_file class initialization")

        if (!length(path_rmd)) stop(self$id, " failed to initialize. Path to Rmarkdown file is required.")
        if (!file.exists(path_rmd)) warning(self$id, " file ", path_rmd, "does not exist!")

        # do some basic checks on path_rmd
        if (!is.character(path_rmd)) stop(self$id, ": path_rmd should be a character")

        dn <- dirname(path_rmd)
        if (!dir.exists(dn)) {
          warning(self$id, ": directory ", dn, " does not exist.")
        }
        self$path_rmd <- path_rmd

        if (!is.null(knit_args)) {
          if (!is.list(knit_args)) warning(self$id, " knit_args should be a named list of arguments to knitr().")
          self$knit_args <- as.list(knit_args)
        }

        self$output_format <- output_format

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    store_state = function(public_fields = NULL, private_fields = NULL) {
      super$store_state(
        public_fields  = unique(c(public_fields, "path_rmd", "knit_args", "output_format")),
        private_fields = private_fields
      )
    },

    update_definition =
      function(
        ...,
        path_rmd = NULL,
        knit_args = NULL,
        output_format = NULL,
        store   = TRUE,
        verbose = TRUE
      ) {
        super$update_definition(..., verbose = verbose, store = FALSE)

        if (!identical(self$path_rmd, path_rmd)) {
          if (verbose) notify_update(self$id, "file path of the Rmarkdown document")
          # do some basic checks on path_rmd
          if (!is.character(path_rmd)) stop(self$id, ": path_rmd should be a character")

          dn <- dirname(path_rmd)
          if (!dir.exists(dn)) {
            warning(self$id, ": directory ", dn, " does not exist.")
          }
          self$path_rmd <- path_rmd
          private$.trigger_defchange <- TRUE
        }

        if (!identical(self$knit_args, as.list(knit_args))) {
          if (!is.list(knit_args)) warning(self$id, " knit_args should be a named list of arguments to knitr().")
          self$knit_args <- as.list(knit_args)
          private$.trigger_defchange <- TRUE
        }

        if (!identical(self$output_format, output_format)) {
          self$output_format <- output_format
          private$.trigger_defchange <- TRUE
        }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    process = function(verbose = TRUE, verbose_prefix = "") {

      if (!requireNamespace("knitr")) stop("knitr package required for compiling Rmarkdown documents!")

      if (verbose) {
        cat(verbose_prefix, crayon::red(self$id), ": Knitting an Rmarkdown file: ", self$path_rmd, "\n", sep = "")
      }

      # for referencing other objects in rflow
      .RFLOW <- parent.env(self)

      .DATA <- function(x) {
        .RFLOW[[x]]$value
      }

      .NODES <- function(x) {
        .RFLOW[[x]]
      }

      # knitr::knit(
      rmarkdown::render(
        input  = self$path_rmd,
        output_file = basename(self$path),
        output_dir  = dirname(self$path),
      )
    }

  )
)

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
    .last_evaluated  = NULL,
    .last_changed   = NULL
  ),

  public    = list(
    id         = NULL,
    env        = NULL,
    name       = NULL,
    desc       = NULL,

    tags       = NULL,

    depends    = NULL,
    upstream   = NULL,
    downstream = NULL,

    persistence        = NULL,
    trigger_defchange  = FALSE,
    trigger_manual     = FALSE,
    trigger_condition  = NULL,
    # last_evaluated = NULL,

    set_id = function(id, name, env) {
      if (is.null(id) & (is.null(name) | is.null(env))) stop("Missing id or (env + name)!") # either 'id' or 'name' + 'env' arguments have to be provided
      self$id <- if (is.null(id)) paste0(env, ".", name) else id
      # set_persistence(self$persistence)
    },

    set_persistence = function(persistence) {
      self$persistence <-
        if (is.list(persistence) && length(persistence$path)) {
          if (dir.exists(persistence$path)) {
            list(
              enabled = TRUE,
              path    = persistence$path,
              file    = paste0(self$id, ".rds")
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

        self$tags    <- tags

        depends_char <- if (is.character(depends)) depends else names(depends)
        self$depends <- depends_char

        # self$trigger_defchange <- TRUE # WHY???
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

    update_definition =
      function(
        id      = NULL,
        type    = NULL,
        desc    = NULL,
        tags    = NULL,
        depends = NULL,
        trigger_condition = NULL,

        ...,
        store  = TRUE,
        verbose = FALSE
      ) {
        depends_char <- if (is.character(depends)) depends else names(depends)
        if (!setequal(self$depends, depends_char)) {

          if (verbose) notify_update(self$id, "dependencies")

          self$depends <- depends_char
          self$trigger_defchange <- TRUE
        }

        if (!is.null(desc)) self$desc <- desc

        if (!identical(as.character(self$tags), as.character(tags))) {
          if (verbose) notify_update(self$id, "tags")
          self$tags <- tags
          # self$trigger_defchange <- TRUE
        }

        trigger_condition <- suppressWarnings(as_r_expr(r_code = trigger_condition))
        if (!identical(as.character(self$trigger_condition), as.character(trigger_condition))) {
          if (verbose) notify_update(self$id, "trigger condition")
          self$trigger_condition <- trigger_condition
          self$trigger_defchange <- TRUE
        }

        # other_args <- list(...)
        # if (length(other_args)) warning("Not updating ", paste(names(other_args), collapse = ", "), " properties.\n")

        # this should be always at the very end
        # storing is not neccesary when e.g. update_definition() is called from ancestor's method
        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    set_upstream = function(obj) {
      if (!in.R6(obj, self$upstream)) {
        self$upstream <- c(self$upstream, obj)
      }
    },

    set_downstream = function(obj) {
      if (!in.R6(self, obj$downstream)) {
        obj$downstream <- c(obj$downstream, self)
      }
    },

    connect_to = function(id) {
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

    check_triggers = function() {
      return(
        FALSE ||
          self$trigger_defchange ||
          self$trigger_manual ||
          self$check_trigger_condition() ||
          !length(self$last_evaluated) || is.na(self$last_evaluated))
    },

    reset_triggers = function() {
      self$trigger_defchange <- FALSE
      self$trigger_manual <- FALSE
    },

    eval = function() {

    },

    make = function(force = FALSE, verbose = TRUE, verbose_prefix = "") {

      if (verbose) {
        cat(verbose_prefix, "\u250C Solving node ", crayon::red(crayon::bold(self$id)), "\n", sep = "")
      }

      # solve/make upstream nodes first
      if (length(self$upstream) != length(self$depends)) self$connect() # check that it's connected to upstream

      results <- FALSE
      for (y in self[["upstream"]]) {
        results <-
          results |
          y$last_changed > self$last_evaluated |
          y$make(force = force, verbose = verbose, verbose_prefix = paste0(verbose_prefix, "\u2502  "))
      }

      triggered <- isTRUE(force) || isNotFALSE(results) || isNotFALSE(self$check_triggers())

      if (!triggered) {
        if (verbose) cat(verbose_prefix, "\u2514 ", crayon::silver(self$id, " not triggered.", sep = ""), "\n", sep = "")
        return(invisible(FALSE))
      }

      # then make the object itself
      # eval should return if the object was evaluated/changed etc...
      trigger <- isNotFALSE(self$eval(verbose = verbose, verbose_prefix = paste0(verbose_prefix, "\u2502  ")))

      # all triggers should be resetted now
      self$reset_triggers()

      # return whether dependants should be triggered or not
      return(invisible(trigger))

    }
  ) ,

  active = list(

    last_evaluated = function(value) {
      if (missing(value)) {
        return(private$.last_evaluated)
      } else {
        private$.last_evaluated <- value
        private$.last_changed   <- value
      }
    },

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

    set_cache = function(cache) {
      self$cache <-
        if (is.list(cache) && length(cache$path)) {
          if (dir.exists(cache$path)) {
            list(
              enabled = TRUE,
              path    = cache$path,
              file    = paste0(self$id, ".rds")
            )
          } else stop(cache$path, " does not exist.")
        } else {
          list(
            enabled = FALSE
          )
        }

      invisible(TRUE)
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
        cache   = list(enabled = FALSE)
      ) {
        super$initialize(..., store = FALSE)

        self$r_code <- r_code
        self$r_expr <- suppressWarnings(as_r_expr(r_code = r_code, r_expr = r_expr))

        if (is.null(self$r_expr) && length(self$depends))
          warning(self$id, " is not a leaf node but has no R expression to evaluate")

        # caching properties
        self$set_cache(cache)

        # from deprecated setup()
        # connect to specified R environment
        if (is.null(self$env)) {
          self$r_env <- .GlobalEnv
        } else {
          r_env <- get(self$env, .GlobalEnv) # other than .GlobalEnv?
          if (!is.environment(r_env)) stop(paste(self$env, "is not an R environment object"))
          self$r_env <- r_env
        }

        # try restoring the object from cache
        if (self$cache$enabled)
          if (file.exists(file.path(self$cache$path, self$cache$file)))
            try(
              {
                value <- readRDS(file.path(self$cache$path, self$cache$file))
                assign(self$name, value, pos = self$r_env)
                self$check_hash()
              },
              silent = TRUE
            )

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    store_state = function() {
      super$store_state(
        public_fields  = c("r_code", "r_expr")
      )
    },

    update_definition =
      function(
        ...,
        r_code  = NULL,
        r_expr  = NULL,
        store   = TRUE,
        verbose = FALSE
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

      if (changed && self$cache$enabled)
        saveRDS(object = self$get(), file = file.path(self$cache$path, self$cache$file))

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

    check_triggers = function() {
      return(super$check_triggers() || !self$exists())
    },

    get = function() {
      if (self$exists()) {
        return(get(self$name, pos = self$r_env))
      } else {
        warning("Object ", self$id, " (", self$name, ")", " does not exist.")
        return(NULL)
      }
    },

    remove = function() {
      if (self$exists()) {
        warning("Deleting R object represented by ", crayon::red(self$id), " !")
        return(invisible(rm(list = self$name, pos = self$r_env)))
      } else {
        warning("Object ", self$id, " (", self$name, ")", " does not exist.")
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
        return(self$hash$time)
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

    env   = NULL,
    driver     = NULL,
    con_code   = NULL,
    connection = NULL,
    sql_code   = NULL,
    r_expr     = NULL,

    initialize =
      function(
        ...,
        sql_code = NULL,
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

        if (!length(r_expr)) {
          if (!length(r_code)) {
            if (!length(sql_code)) {
              warning(id, ": no R expression/code or SQL code!")
              self$r_expr <- NULL
            } else {
              sql_code      <- escape_quotes(sql_code)
              self$r_expr   <- parse(text = private$sql_to_r_expr(sql_code))
              self$sql_code <- sql_code # storning sql_code really just for printing now!
            }
          } else {
            self$r_expr <- parse(text = r_code)
          }
        } else {
          self$r_expr <- r_expr
        }

        private$.last_evaluated <- if (length(.last_evaluated)) .last_evaluated else as.POSIXct(NA)

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },

    store_state = function() {
      super$store_state(
        public_fields  = c("con_code", "r_expr", "sql_code")
      )
    },

    update_definition =
      function(
        ...,
        con_code = NULL,
        sql_code = NULL,
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

        if (!length(r_expr)) {
          if (!length(r_code)) {
            if (!length(sql_code)) {
              warning(id, ": no R expression/code or SQL code!")
              r_expr <- NULL
            } else {
              sql_code      <- escape_quotes(sql_code)
              r_expr        <- parse(text = private$sql_to_r_expr(sql_code))
              self$sql_code <- sql_code # storning sql_code really just for printing now!
            }
          } else {
            r_expr <- parse(text = r_code)
          }
        } else {
          r_expr <- r_expr
        }

        if (!identical(as.character(self$r_expr), as.character(r_expr))) {
          if (verbose) notify_update(self$id, "R/SQL expression")
          self$r_expr <- r_expr
          self$trigger_defchange <- TRUE
        }

        if (self$persistence$enabled && store) self$store_state()

        return(invisible(TRUE))
      },


    eval = function(verbose = TRUE, verbose_prefix = "") {
      exists_check <- self$exists()

      if (verbose) {
        # if (!is.null(self$sql_code)) cat(verbose_prefix, "SQL: ", self$sql_code, sep = "")
        cat(verbose_prefix, crayon::red(self$id), ": Evaluating R expression:\n", sep = "")
        cat_r_expr(self$r_expr, paste0(verbose_prefix, "  "))
      }

      # for referencing other objects in rflow
      .RFLOW <- parent.env(self)

      eval(self$r_expr) # TODO: explicitly specify some other envir for evaluation?

      private$.last_evaluated <- Sys.time()

      if (self$persistence$enabled) self$store_state()

      return(TRUE)
    },

    exists = function() {
      DBI::dbExistsTable(conn = self$connection, name = self$name)
    },

    check_triggers = function() {
      return(super$check_triggers() || !self$exists())
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
      cat(prefix, crayon::cyan(paste_sql(self$sql_code)), sep = "")
    },

    get = function() {
      if (self$exists()) {
        return(DBI::dbReadTable(conn = self$connection, name = self$name))
      } else {
        warning("Object ", self$id, " (", self$name, ")", " does not exist.")
        return(NULL)
      }
    },

    remove = function() {
      if (self$exists()) {
        warning("Deleting DB object represented by ", crayon::red(self$id), " !")
        return(invisible(DBI::dbRemoveTable(conn = self$connection, name = self$name)))
      } else {
        warning("Object ", self$id, " (", self$name, ")", " does not exist.")
        return(invisible(FALSE))
      }
    }

  ),

  private = list(
    sql_to_r_expr = function(sql_statements) {

      paste(
        sapply(X = sql_statements,
               FUN = function(sql_statement)
                 stringr::str_interp(
                   "DBI::dbExecute(conn = self$connection, statement = '${sql_statement}')"
                 )
        ),
        collapse = "\n"
      )
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

    remove = function() {
      if (self$exists()) {
        warning("Deleting DB object represented by ", crayon::red(self$id), " !")
        return(invisible(odbc32::sqlDrop(con = self$connection, name = self$name)))
      } else {
        warning("Object ", self$id, " (", self$name, ")", " does not exist.")
        return(invisible(FALSE))
      }
    }

  ),

  private = list(
    sql_to_r_expr = function(sql_statements) {

      res <-
        sapply(X = sql_statements,
               FUN = function(sql_statement)
                 stringr::str_interp(
                   "odbc32::sqlQuery(con = self$connection, query = '${sql_statement}', errors = TRUE)"
                 )
        )

      # workaround for ACCESS which does not have DROP IF EXISTS
      if (length(sql_statements) > 1 && stringr::str_detect(sql_statements[1], stringr::fixed("DROP"))) {
        res[1] <- paste0("try(", res[1], ")")
      }

      paste(res, collapse = "\n")

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
        verbose = FALSE
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
        if (is.numeric(self$sheet))
          return(isTRUE(as.integer(self$sheet) %in% seq_along(openxlsx::getSheetNames(self$path)))) else
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

    check_triggers = function() {
      return(super$check_triggers() || !self$exists())
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
        verbose = FALSE
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

    check_triggers = function() {
      return(super$check_triggers() || !self$exists())
    },

    remove = function() {
      if (self$exists()) {
        warning("Deleting file represented by ", crayon::red(self$id), " !")
        return(invisible(file.remove(self$path)))
      } else {
        warning("Object ", self$id, " (", self$name, ")", " does not exist.")
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


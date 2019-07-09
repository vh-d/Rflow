

# tools for passing arguments ---------------------------------------------


# ff <- function(a = 1+2, b = a, ...) {
#   # sys.call()
#   # sys.function()
#   args_form <- formals()
#   args_form$... <- NULL
#   args_var <- as.list(match.call()[-1])
#   args_union <- Rflow:::union.list(args_form, args_var)
#   
#   # def_args_eval <- lapply(def_args, eval)
#   # var_args_eval <- lapply(var_args, eval)
#   # 
#   args_union_eval <- lapply(args_union, eval, envir = args_union)
#   
#   str(args_union_eval)
# }


unionArgsToList <- function(args_form = NULL, args_var = NULL) {
  args_form$... <- NULL
  args_union <- Rflow:::union.list(args_form, args_var)
  
  lapply(args_union, eval, envir = args_union)
}


# example how to use unionArgsToList
fff <- function(arg1 = 2, ...) {
  unionArgsToList(formals(), match.call()[-1])
}

# fff(c = 1+2)


node_defaults <- 
  list(
    id      = NULL, 
    name    = NULL, 
    env     = NULL, 
    desc    = NULL,  
    depends    = NULL, # character vector of dependencies (upstream nodes)
    
    upstream   = NULL, # vector of references to depencies
    downstream = NULL, # vector of references to dependants
    
    trigger_defchange = NULL,
    trigger_condition = NULL,
    persistence = list(enabled = FALSE),
    .last_evaluated = NULL,
    .last_changed   = NULL,
    
    vis_params = NULL,
    
    store   = TRUE
  )

node <- function(...) {
  def <- Rflow:::union.list(x = node_defaults, y = list(...))
  def$id <- Rflow:::get_id(def)
  def$type <- "node"
  
  self <- list2env(def)
  class(self) <- "node"
  
  set_persistence(self, self$persistence)
  
  self
}

print.node <-  function(self) {
  cat("<", class(self)[1], "> ", crayon::red(self$id), ": ", self$name,  "\n", sep = "")
  if (length(self$desc)) cat("  desc: ", crayon::italic(self$desc),         "\n", sep = "")
  if (length(self$tags)) cat("  tags: ", paste0(self$tags, collapse = " "), "\n", sep = "")
  cat("  depends: ", paste0(self$depends, collapse = ", "), "\n", sep = "")
  cat("  evaluated: ", as.character(self$last_evaluated), "\n",
      "  changed:   ", as.character(self$last_changed),   "\n", sep = "")
  cat("  triggers: ", "\n", sep = "")
  cat("    definition: ", self$trigger_defchange, "\n", sep = "")
  cat("    manual:     ", self$trigger_manual,    "\n", sep = "")
  if (length(self$trigger_condition)) cat("    condition:  ", self$check_trigger_condition(), "\n", sep = "")
}

set_persistence <- function(self, persistence) {
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
}

node_r_defaults <- list(
  
)

node_r <- function(...) {
  self <- node(...)
  class(self) <- c("node_r", class(self))
  
  return(self)
}

print.node_r <- function(self) {
  NextMethod("print", self)
  cat("  cache: \n")
  # cat("    enabled: ", self$cache$enabled,  "\n", sep = "")
  # cat("    exists:  ", self$cache_exists(), "\n", sep = "")
  cat("  expression: \n")
  Rflow:::cat_r_expr(head(self$r_expr), verbose_prefix = "    ")
}

n1 <- node_r(name = "df", env = "RDATA")
n2 <- node(name = "tab1", env = "DB", depends = c("RDATA.df"))
n2
n1

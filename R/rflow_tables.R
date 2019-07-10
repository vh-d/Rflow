
format_tags <- function(tags = NULL) {
  if (!length(tags)) return("")
  paste0("|", paste0(tags, collapse = "|"), "|")
}

#' list all nodes of an Rflow object
#'
#' @param rflow rflow object
#' @export
nodes <- function(x) {
  UseMethod("nodes", x)
}

#' @export
nodes.rflow <- function(rflow) {

  coln <- c("id", "name", "env", "desc", "sql_code", "r_expr", "node_type")

  node_objs <- get_nodes(rflow)
  
  # for empty rflow return empty data.table
  if (!length(node_objs)) {
    return(setnames(data.table(matrix(character(), 1, length(coln))), coln)[0])
  } # else:

  dtNODES <-
    rbindlist(
      lapply(
        node_objs,
        function(x) {
          data.table(
            id    = null2na(x$id),
            name  = null2na(x$name),
            env   = null2na(x$env),
            desc  = null2na(x$desc),
            tags  = format_tags(x$tags),
            sql_code  = deescape_quotes(paste_sql(x$sql)),
            r_expr    = paste0(as.character(x$r_expr), collapse = ";\n\n"),
            node_type = class(x)[1], 
            title = x$title(),
            label = x$label(),
            shape = x$vis_params$shape
          )
        }
      )
    )

  dtNODES

}


#' @export
edges <- function(x) {
  UseMethod("edges", x)
}

#' @export
edges.rflow <- function(rflow) {

  rbindlist(
    c(
      list(data.table(from = character(), to = character())),
      lapply(
        mget(ls(rflow), envir = rflow),
        function(x) if (length(x$depends)) data.table(from = x$depends, to = x$id) else NULL
      )
    )
  )
    
}


format_tags <- function(tags = NULL) {
  if (!length(tags)) return("")
  paste0("|", paste0(tags, collapse = "|"), "|")
}


basic_node_attr_names <- c("id", "name", "env", "desc", "sql_code", "r_expr", "node_type")
  
#' @export
basic_node_attributes <- function(x) {
    data.table(
      id    = null2na(x$id),
      name  = null2na(x$name),
      env   = null2na(x$env),
      desc  = null2na(x$desc),
      # tags  = format_tags(x$tags),
      tags  = list(x$tags),
      sql_code  = deescape_quotes(paste_sql(x$sql)),
      r_expr    = paste0(as.character(x$r_expr), collapse = ";\n\n"),
      node_type = class(x)[1], 
      title = x$title(),
      label = x$label(),
      as.data.table(x$vis_params)[1]
    )
}


#' @export
as.data.table.rflow <- function(x, what = c("nodes", "edges")) {
  what <- match.arg(what)

  switch(what,
    "nodes" = as_data_table_nodes(x),
    "edges" = as_data_table_edges(x)
  )
}





as_data_table_nodes <- function(rflow) {

  node_objs <- nodes(rflow)
  
  # for empty rflow return empty data.table
  if (!length(node_objs)) {
    return(setnames(data.table(matrix(character(), 1, length(basic_node_attr_names))), basic_node_attr_names)[0])
  } # else:

  dtNODES <-
    rbindlist(
      lapply(
        node_objs,
        basic_node_attributes
      ), 
      use.names = TRUE, # graphical parameters can vary
      fill      = TRUE
    )

  dtNODES
}


#' @export
as_data_table_edges <- function(x) {

  rbindlist(
    c(
      list(data.table(from = character(), to = character())),
      lapply(
        mget(ls(x), envir = x),
        function(x) if (length(x$depends)) data.table(from = x$depends, to = x$id) else NULL
      )
    )
  )
    
}

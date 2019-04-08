
disc_scale <- function(
  ..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
  direction = 1, na.value = "grey50", aesthetics = "fill") {
  scales::hue_pal(h, c, l, h.start, direction)
}

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

  # for empty rflow return empty data.table
  node_objs <- get_nodes(rflow)
  if (!length(node_objs)) {
    return(setnames(data.table(matrix(character(), 1, length(coln))), coln)[0])
  }

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
            sql_code  = deescape_quotes(paste_sql(x$sql_code)),
            r_expr    = paste_sql(as.character(x$r_expr)),
            node_type = class(x)[1])
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
    lapply(
      mget(ls(rflow), envir = rflow),
      function(x) if (length(x$depends)) data.table(from = x$depends, to = x$id) else NULL))

}


#' visualize Rflow DAGs using visNetwork
#'
#' @param rflow an rflow objects
#' @param tags vector of tags for filtering nodes
#' @param includeIsolated logical; Switch to filter isolated/lonely nodes.
#' @param direction see visNetwork docs on hierarchical graphs
#' @param ... args passed to visNetwork
#'
#' @export
plot.rflow <- function(rflow, tags = NULL, includeIsolated = TRUE, direction = "LR", ...) {

  if (!requireNamespace("visNetwork", quietly = TRUE)) stop("visNetwork package required to plot graphs")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table package required to plot graphs")

  dtEDGES <- edges.rflow(rflow)
  dtNODES <- nodes.rflow(rflow)
  if (!nrow(dtNODES)) {
    warning("Nothing to plot...")
    return(invisible(NULL))
  }
  # TODO: objects will have their own methods for printing nice hover titles

  if (length(tags)) {
    query_tags <- tags
    rm(tags)
    dtNODES <- dtNODES[stringr::str_detect(tags, stringr::fixed(paste0("|", query_tags, "|")))]
  }

  # drop isolated/lonely nodes if needed
  if (isFALSE(includeIsolated)) {
    dtNODES <- dtNODES[dtEDGES[, .(id = unique(c(from, to)))], on = "id"]
  }

  dtNODES[, label := paste0(env, " / ", name)]
  dtNODES[,
          title :=
            paste0("<b>", id, "</b>",
                   " &lt;", node_type, "&gt;", "<br>")]

  # add description if present
  dtNODES[(!is.na(desc)),
          title := paste0(title,
                          "<p><em>",
                          stringr::str_replace_all(
                            stringr::str_wrap(desc, width = 40), stringr::fixed("\n"), "<br>"),
                          "</em></p>")]

  # add SQL code if present
  dtNODES[(nchar(sql_code)>0),
          title := paste0(title,
                          "<p>",
                          "SQL:<br>
                          <font size=\"-2\" face = \"monospace\">",
                          stringr::str_replace_all(
                            stringr::str_replace_all(
                              sql_code,
                              stringr::fixed("\n"), "<br>"),
                            stringr::fixed(" "), "&nbsp;"),
                          "</font></p>")]

  # add SQL code if present
  dtNODES[(is.na(sql_code) | nchar(sql_code)==0),
          title := paste0(title,
                          "<p>",
                          "R:<br>
                          <font size=\"-2\" face = \"monospace\">",
                          stringr::str_replace_all(
                            stringr::str_replace_all(
                              r_expr,
                              stringr::fixed("\n"), "<br>"),
                            stringr::fixed(" "), "&nbsp;"),
                          "</font></p>")]

  dtNODES[,
          shape :=
            c("r_node"     = "triangle",
              "db_node"    = "square",
              "accdb_node" = "square",
              "file_node"  = "star"
            )[node_type]]

  unique_levels <- dtNODES[, unique(env)]
  unique_levels_colors <- disc_scale()(length(unique_levels))
  names(unique_levels_colors) <- unique_levels

  dtNODES[, color := unique_levels_colors[env]]

  visNetwork::visNetwork(
    nodes = dtNODES,
    edges = dtEDGES,
    ...
  ) %>%
    # visLayout()
    visNetwork::visHierarchicalLayout(
      sortMethod = "directed",
      direction = direction
    ) %>%
    visNetwork::visNodes() %>%
    visNetwork::visEdges(
      arrows = "to",
      smooth = FALSE
    ) %>%
    visNetwork::visOptions(

      nodesIdSelection =
        list(
          enabled = TRUE,
          useLabels = TRUE
        ),

      selectedBy =
        list(
          variable = "env"
        ),

      highlightNearest =
        list(enabled = TRUE,
             algorithm = "hierarchical",
             degree = list(from = 50, to = 50),
             hover   = TRUE)

    ) %>%
    # visNetwork::visLegend(
    #   enabled = TRUE,
    #   width = .2
    # ) %>%
    visNetwork::visInteraction(
      tooltipDelay      = 1,
      navigationButtons = TRUE,
      dragNodes         = TRUE) %>%
    visNetwork::visPhysics(
      enabled = FALSE
    )
}



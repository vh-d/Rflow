
disc_scale <- function(
  ..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
  direction = 1, na.value = "grey50", aesthetics = "fill") {
  scales::hue_pal(h, c, l, h.start, direction)
}


#' @export
nodes <- function(x, ...) {
  UseMethod("nodes", x)
}

#' list all nodes of an Rflow object
#'
#' @param rflow
#' @param ...
#' @export
nodes.rflow <- function(rflow, ...) {
  dtNODES <-
    rbindlist(
      lapply(
        mget(ls(rflow), envir = rflow),
        function(x) {
          data.table(
            id    = null2na(x$id),
            name  = null2na(x$name),
            env   = null2na(x$env),
            desc  = null2na(x$desc),
            sql_code  = null2na(x$sql_code),
            r_expr    = as.character(null2na(x$r_expr)),
            node_type = class(x)[1])
        }
      )
    )

  dtNODES

}


#' visualize DAG networks
#'
#' @param rflow
#' @param direction
#' @param ...
#'
#' @export
plot.rflow <- function(rflow, direction = "LR", ...) {

  if (!requireNamespace("visNetwork")) stop("visNetwork package required to plot graphs")


  dtEDGES <-
    rbindlist(
      lapply(
        mget(ls(rflow), envir = rflow),
        function(x) if (length(x$depends)) data.table(from = x$depends, to = x$id) else NULL))

  dtNODES <-
    rbindlist(
      lapply(
        mget(ls(rflow), envir = rflow),
        function(x) {
          data.table(
            id    = null2na(x$id),
            name  = null2na(x$name),
            env   = null2na(x$env),
            desc  = null2na(x$desc),
            sql_code  = null2na(paste_sql(x$sql_code)),
            node_type  = class(x)[1])
        }
      )
    )
  # TODO: objects will have their own methods for printing nice hover titles

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
      selectedBy =
        list(variable = "label"),

      highlightNearest =
        list(enabled = TRUE,
             algorithm = "hierarchical",
             degree = list(from = 50, to = 50),
             hover   = TRUE),

      nodesIdSelection = FALSE
    ) %>%
    # visNetwork::visLegend(
    #   enabled = TRUE,
    #   width = .2
    # ) %>%
    visNetwork::visInteraction(
      tooltipDelay      = 1,
      navigationButtons = TRUE,
      dragNodes         = TRUE)
}



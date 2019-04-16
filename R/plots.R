
disc_scale <- function(
  ..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
  direction = 1, na.value = "grey50", aesthetics = "fill") {
  scales::hue_pal(h, c, l, h.start, direction)
}



visData <- function(rflow, tags = NULL, includeIsolated = TRUE) {
  
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
  
  dtNODES[, label := paste0(env, "\n", name)]
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
  
  dtNODES[, color.background := unique_levels_colors[env]]
  dtNODES[, color.border     := color.background]
  dtNODES[, color.hover      := color.background]
  dtNODES[, color.highlight.border := "black"]
  dtNODES[, color.highlight.background := color.background]
  # dtNODES[, color.border := "black"]
  
  list(
    dtNODES = dtNODES,
    dtEDGES = dtEDGES
  )
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
  
  l <- visData(rflow = rflow, tags = tags, includeIsolated = includeIsolated)
  
  visNetwork::visNetwork(
    nodes = l$dtNODES,
    edges = l$dtEDGES,
    ...
  ) %>%
    # visLayout()
    visNetwork::visHierarchicalLayout(
      sortMethod = "directed",
      direction = direction
    ) %>%
    visNetwork::visNodes(
      borderWidthSelected = 4
    ) %>%
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
        list(
          enabled = TRUE,
          algorithm = "hierarchical",
          degree = list(from = 50, to = 50),
          hover   = TRUE
        )
      
    ) %>%
    visNetwork::visInteraction(
      tooltipDelay      = 1,
      navigationButtons = TRUE,
      dragNodes         = TRUE) %>%
    visNetwork::visPhysics(
      enabled = FALSE
    )
}


#' visualize workflow DAG
#'
#' @param rflow 
#' @param tags 
#' @param includeIsolated 
#' @param direction 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
visRflow <- function(rflow, tags = NULL, includeIsolated = TRUE, direction = "UD", ...) {
  
  if (!requireNamespace("visNetwork", quietly = TRUE)) stop("visNetwork package required to plot graphs")
  
  l <- visData(rflow = rflow, tags = tags, includeIsolated = includeIsolated)
  
  visNetwork::visNetwork(
    nodes = l$dtNODES,
    edges = l$dtEDGES,
    ...
  ) %>%
    visNetwork::visIgraphLayout(
      layout  = "layout_with_sugiyama", # layout with layers
      physics = FALSE,
      smooth  = TRUE,
      type    = "full"
    ) %>% 
    visNetwork::visNodes(
      borderWidthSelected = 4
    ) %>%
    visNetwork::visEdges(
      arrows = "to",
      smooth = TRUE
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
        list(
          enabled = TRUE,
          algorithm = "hierarchical",
          degree = list(from = 50, to = 50),
          hover   = TRUE
        ), 
      
      autoResize = TRUE,
      
    ) %>%
    visNetwork::visInteraction(
      tooltipDelay      = 1,
      navigationButtons = TRUE,
      dragNodes         = TRUE) %>%
    visNetwork::visPhysics(
      enabled = FALSE
    )
}


#' visualize workflow DAG
#' @param rflow 
#' @param tags 
#' @param includeIsolated 
#' @param direction 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
visRflow2 <- function(rflow, tags = NULL, includeIsolated = TRUE, direction = "UD", ...) {
  
  if (!requireNamespace("visNetwork", quietly = TRUE)) stop("visNetwork package required to plot graphs")
  
  l <- visData(rflow = rflow, tags = tags, includeIsolated = includeIsolated)
  
  visNetwork::visNetwork(
    nodes = l$dtNODES,
    edges = l$dtEDGES,
    ...
  ) %>%
    visNetwork::visHierarchicalLayout(
      sortMethod = "directed",
      direction  = direction, 
    ) %>%
    visNetwork::visNodes(
      borderWidthSelected = 4
    ) %>%
    visNetwork::visEdges(
      arrows = "to",
      smooth = TRUE
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
        list(
          enabled = TRUE,
          algorithm = "hierarchical",
          degree = list(from = 50, to = 50),
          hover   = TRUE
        ),
      
      autoResize = TRUE
      
    ) %>%
    visNetwork::visInteraction(
      tooltipDelay      = 1,
      navigationButtons = TRUE,
      dragNodes         = TRUE) %>%
    visNetwork::visPhysics(
      enabled = FALSE,
      stabilization = FALSE,
      hierarchicalRepulsion = list(
        nodeDistance = 200
      )
    )
}


disc_scale <- function(
  ..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
  direction = 1, na.value = "grey50", aesthetics = "fill") {
  scales::hue_pal(h, c, l, h.start, direction)
}



visData <- function(rflow, tags = NULL, includeIsolated = TRUE, tagsMatchLogic = "all") {

  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table package required to plot graphs")

  dtEDGES <- as_data_table_edges(rflow)
  dtNODES <- as_data_table_nodes(rflow)

  if (!nrow(dtNODES)) {
    warning("Nothing to plot...")
    return(invisible(NULL))
  }
  # TODO: objects will have their own methods for printing nice hover titles

  # format tags
  if (length(tags)) {
    query_tags <- tags
    rm(tags)
    # dtNODES <- dtNODES[stringr::str_detect(tags, stringr::fixed(paste0("|", query_tags, "|")))]
    dtNODES <- dtNODES[matchtags(pattern = query_tags, logic = tagsMatchLogic, tags = tags)]
  }

  # drop isolated/lonely nodes if needed
  if (isFALSE(includeIsolated)) {
    dtNODES <- dtNODES[dtEDGES[, .(id = unique(c(from, to)))], on = "id"]
  }

  # graphics -------------------------
  # colors are dynamic: based on number of environments
  unique_levels <- dtNODES[, unique(env)]
  unique_levels_colors <- disc_scale()(length(unique_levels))
  names(unique_levels_colors) <- unique_levels

  # some nodes can have graphical params already set
  if (!("color.background"           %in% colnames(dtNODES))) dtNODES[, color.background := NA_character_]
  if (!("color.border"               %in% colnames(dtNODES))) dtNODES[, color.border := NA_character_]
  if (!("color.hover"                %in% colnames(dtNODES))) dtNODES[, color.hover := NA_character_]
  if (!("color.highlight.border"     %in% colnames(dtNODES))) dtNODES[, color.highlight.border := NA_character_]
  if (!("color.highlight.background" %in% colnames(dtNODES))) dtNODES[, color.highlight.background := NA_character_]

  dtNODES[is.na(color.background),           color.background           := unique_levels_colors[env]]
  dtNODES[is.na(color.border),               color.border               := color.background]
  dtNODES[is.na(color.hover),                color.hover                := color.background]
  dtNODES[is.na(color.highlight.border),     color.highlight.border     := "black"]
  dtNODES[is.na(color.highlight.background), color.highlight.background := color.background]


  setkeyv(dtNODES, "id")
  setkeyv(dtEDGES, c("from", "to"))

  list(
    dtNODES = dtNODES,
    dtEDGES = dtEDGES
  )
}


#' visualize Rflow DAGs using visNetwork
#'
#' @param x an rflow objects
#' @param tags vector of tags for filtering nodes
#' @param includeIsolated logical; Switch to filter isolated/lonely nodes.
#' @param direction see visNetwork docs on hierarchical graphs
#' @param tagsMatchLogic one of: any/all/none
#' @param ... args passed to visNetwork
#'
#' @export
plot.rflow <- function(x, tags = NULL, includeIsolated = TRUE, direction = "LR", tagsMatchLogic = "all", ...) {

  if (!requireNamespace("visNetwork", quietly = TRUE)) stop("visNetwork package required to plot graphs")

  l <- visData(rflow = x, tags = tags, includeIsolated = includeIsolated, tagsMatchLogic = tagsMatchLogic)

  visNetwork::visNetwork(
    nodes = l$dtNODES,
    edges = l$dtEDGES,
    ...
  ) ->.

  # visLayout()
  visNetwork::visHierarchicalLayout(
    .,
    sortMethod = "directed",
    direction = direction
  ) ->.

  visNetwork::visNodes(
    .,
    borderWidthSelected = 4
  ) ->.

  visNetwork::visEdges(
    .,
    arrows = "to",
    smooth = FALSE
  ) ->.

  visNetwork::visOptions(
    .,
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

  ) ->.

  visNetwork::visInteraction(
    .,
    tooltipDelay      = 1,
    navigationButtons = TRUE,
    dragNodes         = TRUE
  ) ->.

  visNetwork::visPhysics(
    .,
    enabled = FALSE
  )
}


#' visualize workflow DAG
#'
#' @param rflow an rflow object
#' @param tags character; vector of tags or regular expression for filtering nodes by tags
#' @param includeIsolated logical; inlude disconnected nodes?
#' @param direction orientation of flow ("UD" by default)
#' @param ...
#'
#' @return
#' A visNetwork plot.
#' @export
visRflow <- function(rflow, tags = NULL, includeIsolated = TRUE, direction = "UD", orderBy = "id", tagsMatchLogic = "all", ...) {

  if (!requireNamespace("visNetwork", quietly = TRUE)) stop("visNetwork package required to plot graphs")

  l <- visData(rflow = rflow, tags = tags, includeIsolated = includeIsolated, tagsMatchLogic = tagsMatchLogic)

  nodes <- l$dtNODES
  edges <- l$dtEDGES

  if (length(orderBy)) setorderv(nodes, orderBy)

  visNetwork::visNetwork(
    nodes = nodes,
    edges = edges,
    ...
  ) ->.

  visNetwork::visIgraphLayout(
    .,
    layout  = "layout_with_sugiyama", # layout with layers
    physics = FALSE,
    smooth  = TRUE,
    type    = "full"
  ) ->.

  visNetwork::visNodes(
    .,
    borderWidthSelected = 4
  ) ->.

  visNetwork::visEdges(
    .,
    arrows = "to",
    smooth = FALSE
  ) ->.

  visNetwork::visOptions(
    .,
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

  ) ->.

  visNetwork::visInteraction(
    .,
    tooltipDelay      = 1,
    navigationButtons = TRUE,
    dragNodes         = TRUE
  ) ->.

  visNetwork::visPhysics(
    .,
    enabled = FALSE
  )
}


#' visualize workflow DAG
#' @param rflow an rflow object
#' @param tags character; vector of tags or regular expression for filtering nodes by tags
#' @param includeIsolated logical; inlude disconnected nodes?
#' @param direction orientation of flow ("UD" by default)
#' @param ...
#'
#' @return
#' A visNetwork plot.
#' @export
visRflow2 <- function(rflow, tags = NULL, includeIsolated = TRUE, direction = "UD", orderBy = "id", tagsMatchLogic = "all", ...) {

  if (!requireNamespace("visNetwork", quietly = TRUE)) stop("visNetwork package required to plot graphs")

  l <- visData(rflow = rflow, tags = tags, includeIsolated = includeIsolated, tagsMatchLogic = tagsMatchLogic)

  nodes  <- l$dtNODES
  edges  <- l$dtEDGES

  if (length(orderBy)) setorderv(nodes, orderBy)

  visNetwork::visNetwork(
    nodes = nodes,
    edges = edges,
    ...

  ) ->.

  visNetwork::visHierarchicalLayout(
    .,
    sortMethod = "directed",
    direction  = direction,
  ) ->.

  visNetwork::visNodes(
    .,
    borderWidthSelected = 4
  ) ->.

  visNetwork::visEdges(
    .,
    arrows = "to",
    smooth = TRUE
  ) ->.

  visNetwork::visOptions(
    .,
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

  ) ->.

  visNetwork::visInteraction(
    .,
    tooltipDelay      = 1,
    navigationButtons = TRUE,
    dragNodes         = TRUE
  ) ->.

  visNetwork::visPhysics(
    .,
    enabled = TRUE,
    stabilization = FALSE,
    hierarchicalRepulsion = list(
      nodeDistance = 200
    )
  )
}

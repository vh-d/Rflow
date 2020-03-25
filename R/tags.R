# TODO: regexp option
matchtags_ <- function(pattern, tags, logic = c("any", "all", "none")) {
  logic <- match.arg(logic)
  switch (logic,
    "all"  =  all(pattern %in% tags),
    "any"  =  any(pattern %in% tags),
    "none" = !any(pattern %in% tags)
  )
}

#' @param pattern search pattern (currently only vectors of tags for exact match are supported)
#'
#' @param logic "any"/"all"/"none"
#' @param tags do not use
#'
#' @export
matchtags <- function(pattern, logic = "all", tags = get("tags", pos = parent.frame())) {
  sapply(tags, matchtags_, pattern = pattern, logic = logic)
}

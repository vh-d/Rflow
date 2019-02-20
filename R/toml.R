#' @export
read_TOML_file <- function(file, escape = FALSE, ...) {
  tryCatch(
    RcppTOML::parseTOML(input = file, escape = escape, ...),
    error = function(e) {
      stop("File ", file, " could not be loaded:\n    ", e)
    }
  )
}


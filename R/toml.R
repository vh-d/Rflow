#' @export
read_TOML_file <- function(file, escape = FALSE, ...) {

  if (!requireNamespace("RcppTOML")) stop("Package RcppTOML not available!")

  tryCatch(
    RcppTOML::parseTOML(input = file, escape = escape, ...),
    error = function(e) {
      stop("File ", file, " could not be loaded:\n    ", e)
    }
  )
}


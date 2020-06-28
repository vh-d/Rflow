filename_from_id <- function(id, id_hash = NULL, value_hash = NULL, ext = "rds") {
  if (!length(id_hash))
    id_hash <- digest::digest(id, algo = "md5", serialize = FALSE)

  value_hash <-
    if (is.null(value_hash)) "" else {
      if (length(value_hash) > 1) stop("Hash not length of 1: ", paste0(value_hash, collapse = ", "))
      paste0("-", value_hash)
    }

  paste0(id, "_", id_hash, value_hash, ".", ext)
}

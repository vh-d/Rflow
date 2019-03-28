#' Check consistency of identificators and fill missings
#' @export
process_obj_defs <- function(obj_defs) {

  for (obj_ind in seq_along(obj_defs)) {
    obj_name <- names(obj_defs[obj_ind])
    
    has_id_outside  <- length(obj_name) && obj_name != ""
    has_env_inside  <- length(obj_defs[[obj_ind]]$env)
    has_name_inside <- length(obj_defs[[obj_ind]]$name)
    has_id_inside   <- length(obj_defs[[obj_ind]]$id)
    
    if (!(has_id_outside || (has_env_inside & has_name_inside) || has_id_inside)) stop("Either named list or id or env + name have to be specified")
      
    if (has_id_outside | has_id_inside) {
      obj_id_inside  <- names(obj_defs[obj_ind])
      obj_id_outside <- obj_defs[[obj_ind]][["id"]]
      
      obj_id <- unique(c(obj_id_outside, obj_id_inside))
      
      if (length(obj_id) > 1) {
        warning(stringr::str_interp("id '${obj_id_inside}' does not match with object/file name '${obj_id_outside}'. Choosing '${obj_id_inside}' as an id."))
        obj_id <- obj_id_inside
      }
      
      node_env  <- env_name_from_id(obj_name)[, env[1]]
      node_name <- env_name_from_id(obj_name)[, name[1]]
      
      # node_id   <- env_names[id == "obj_id", id]
      if (!has_env_inside)
        obj_defs[[obj_ind]]$env  <- node_env  else 
          if (obj_defs[[obj_ind]]$env  != node_env)  
            stop(stringr::str_interp("${obj_id}: env '${obj_defs[[obj_ind]]$env}' does not match"))
      
      if (!has_env_inside) 
        obj_defs[[obj_ind]]$name <- node_name else 
          if (obj_defs[[obj_ind]]$name != node_name) 
            stop(stringr::str_interp("${obj_id}: name '${obj_defs[[obj_ind]]$name}' does not match"))
    } else {
      full_names <- names(obj_defs)
      full_names[obj_ind] <- get_id(obj_defs[[obj_ind]])
      names(obj_defs) <- full_names
    }
  }
  
  obj_defs
}

# process_obj_defs <- function(obj_defs) {
#   # list_names <- names(obj_defs)
#   obj_names <- names(obj_defs)
#   has_names <- which(!is.na(obj_names))
#   env_names <- env_name_from_id(names(obj_defs))
#   
#   node_envs  <- sapply(obj_defs[has_names], function(x) if (length(x$env)) x$env else NA_character_)
#   node_names <- sapply(obj_defs[has_names], function(x) if (length(x$name)) x$name else NA_character_)
#   
#   env_names[.(id = names(node_envs), env = node_envs), on = "id"][, .(isTRUE(env == i.env)), by = .(id)]
# }


#' @export
env_name_from_id <- function(id) {
  strm <- stringr::str_match(string = id, pattern = "^([0-9a-zA-Z_]+?\\.)?([0-9a-zA-Z]+?[0-9a-zA-Z._]*?)$")
  strm[, 2] <- stringr::str_replace(strm[, 2], "\\.$", "")
  
  data.table(
    id   = strm[, 1],
    env  = strm[, 2],
    name = strm[, 3]
  )
}

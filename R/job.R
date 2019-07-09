job <- function(x, ...) {
  UseMethod("job", x)
}

job.character <- function(x) {
  print(x)
}

job.list <- function(x) {
  do.call(job, x$type, x)
}

job.expression <- function(x) {
  eval(x)
}

r_job <- function(
  r_expr = NULL, 
  r_code = NULL, 
  r_file = NULL
) {
  
  job <- list(r_expr = NULL, mode = NULL)
  
  if (length(r_expr)) {
    job$mode <- "expression"
    job$r_expr <- r_expr
    
    return(job)
  }
  
  if (length(r_code)) {
    job$mode <- "code"
    job$r_expr <- parse(text = r_code)
    
    return(job)
  }
  
    
  if (length(r_file)) {
    job$mode <- "file"
    job$r_expr <- parse(file = r_file)
    
    return(job)
  }
  
  # else:
  # warning("?")
  return(NULL)
  
}

# job(expression({1+b}))


library(Rflow)
library(reticulate)

py1 <-
  list(
    id   = "py1",
    name = "py1",
    type = "py_node",
    py_code = "'Aa'.lower()"
  )

RF <- new_rflow()


add_node(py1, RF)

RF$py1$r_env
RF$py1

RF$py1$make()
RF$py1$get()
RF$py1$remove()


# Rflow 0.3.3
**2020-06-13**

## Changes to the API

* Support for RMarkdown documents (see `rmd_node`).
* New option to skip errors when evaluationg jobs. Use either `onError="skip"` argument of `make()` or RFLOW_ON_ERRORS global option.
* new_rflow() is deprecated for rflow()
* New .DATA() and .NODES() for referencing values of other nodes or node objects inside R jobs. 
* Improved automatic checking of dependencies between nodes. `.RFLOW[[depends[n]]]` is now recognized and does not throw warnings.
* *igraph* package is no longer a dependency.


## Bug fixes

* Argument `read_args` in excel_sheet nodes are now functional.
* Changes in `read_args` are now persistent.
* Updating node definitions to NULL (or just leaving the default value) is now handled correctly.
* csv_node nodes' definitions can be updated 

# Rflow 0.3.2
**2020-06-13**


## Visible new features

* Names of cache files now contain hash values of the data stored inside them to speed up its loading (true lazy/delayed is now possible).
* New `FilterWith`, `lapplyWith` and `sapplyWith()` family of functions to make it easier to apply vectorized functions on nodes.
* Rflow now records time needed to evaluate a node obtainable via `node$time_to_eval()`.
* Indexing a `node_list` object now preserves class attribute (result is still a `node_list`).
* Improved documentation.
* Added a `NEWS.md` file to track changes to the package.

## Internal changes

* eval() is now a universal wrapper function of process() method (where all the node-type specific code has been moved into).



# Rflow 0.2.23
**2020-05-28**

## Visible new features

* New `py_node` and `julia_node` node types allow using Python (via reticulate) and Julia (via JuliaCall).

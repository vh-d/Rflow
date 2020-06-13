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

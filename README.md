
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rflow

Rflow is an R package providing R users a general-purpose workflow
system.

## The idea

**Rflow** is a DAG connecting nodes through dependency relations. There
are three building blocks of rflows:

  - **nodes** (aka targets) represent your data objects such as R
    values, db tables, spreadsheets, files, etc…
  - **environments** serves as containters for nodes. For example a
    database is a container for tables.
  - **jobs** represents dependency connection between nodes

## Installation

Rflow is hosted on GitHub. The easies way to install it is by using
`devtools` package:

``` r
devtools::install_github("vh-d/Rflow")
```

## Examples

…

## TODO:

**Features**

  - R/SQL code in standalone R scripts (outside of TOML file)
  - more nuanced `verbose` option
  - logging into rflow object
  - handling of obsolete nodes
      - removing objects from DAG
      - removing cache
      - removing node config files
      - deleting represented objects
  - deleting properties / setting some to NULL
      - currently, if a property is deleted update() method ignores it
  - query function to set or get fields of multiple objects
  - documentation
  - quick start guide
  - allow Python and Julia code
  - new node types:
      - test node
  - Rflow manager as a Shiny app

**Implementation**

  - experiment with proper ORM instead of serialization of selected
    properties
  - environment class
  - generic methods in node class for initializing and updating
    properties
  - more unit tests
  - make all public properties active (trigger persistence storage)

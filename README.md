
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rflow

Rflow is an R package providing R users with a general-purpose workflow
framework. It is suitable for various purposes: from simple automation
to building powerfull ETL tools.

Rflow makes your data pipelines better organized and managable (worflow
can be visualized, objects may have documentation and tags).

It saves your time as your objects are rebuild only when its needed
(also most objects are persistent over sessions).

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

Define the target nodes:

``` r
objs <- 
  list(
    "DB.mytable" = list(
      type = "db_node",
      desc = "A db table with data that serves as source for further computation"
    ),
    "mytable_summary" = list(
      type = "r_node", # unnecessary for R objects
      desc = "Summary statistics of DB.mytable",
      r_expr = expression_r({
        RETL::etl_read(.RFLOW[["DB.mytable"]]) %>% summary()
      })
    ),
    "main_product" = list(
      desc = "Main output",
      r_expr = expression_r({
        .RFLOW[["DB.mytable"]]$get() %>% some_fancy_computation()
      })
    ), 
    "DB.output" = list(
      desc = "Outcome is loaded back to the DB",
      r_expr = expression_r({
        .RFLOW[["main_product"]]$get() %>%
          RETL::etl_write(to = self$connection, name = self$name)
      })
    )
  ) 
```

Add the to your existing workflow:

``` r
objs %>% 
  Rflow::process_obj_defs() %>% 
  Rflow::add_nodes(rflow = .RFLOW)
```

and build targets

``` r
make(.RFLOW)
```

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

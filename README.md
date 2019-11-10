
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href="https://travis-ci.org/vh-d/Rflow"><img src="https://travis-ci.org/vh-d/Rflow.svg?branch=master" alt="Travis"></a>

# Rflow

Rflow is an R package providing R users with a general-purpose workflow
framework. Rflow allows you to describe your data declaratively as
objects with dependencies and does the heavy lifting for you. It is
suitable for various purposes: from managing several simple automation
scripts to building powerfull ETL pipelines.

Rflow makes your data pipelines better organized and managable (worflow
can be visualized, objects may have documentation and tags).

It saves your time as your objects are rebuild only when its needed
(also objects are persistent over sessions).

**Development of `Rflow` package is still in its beta version phase
(some breaking changes may happen).**

## Usecases

1.  We have a complex suite of scripts that prepare data from various
    sources for analysis or publication. We need to update our output
    whenever some of the inputs changes or when some of the scripts are
    changed.

2.  We want to get data from a database, transform them in R and then
    upload them back to the database (or other place).

3.  We have complex long-running computations that need to be run only
    when some of the inputs/parameters change.

## Getting started

### Prerequisites

  - `R` (\>= 3.5.3 tested)
  - `devtools` package

<!-- end list -->

``` r
install.packages("devtools")
```

### Installation

Rflow is hosted on GitHub. The easies way to install it is by using
`devtools` package:

``` r
devtools::install_github("vh-d/Rflow")
```

## How it works

**Rflow** is a DAG connecting nodes through dependency relations. There
are three building blocks of rflows:

  - **nodes** (aka targets) represent your data objects such as R
    values, db tables, spreadsheets, files, etc…
  - **environments** serves as containters for nodes. For example a
    database is a container for database tables, R environemnt is a
    container for R objects, etc…
  - **jobs** represents dependency connection between nodes. It carries
    the recipe how to build a target object.

Currently, we have these types of nodes implemented:

  - `node`: a generic node class (really just a parent class other
    classes inherit from)
  - `r_node`: node representing R objects
  - `db_node`: node representing database tables and views
  - `accdb_node`: node representing tables in 32-bit Access database
  - `file_node`: for representing files on disk
  - `csv_node`: descendant of `file_node` for representing csv files
  - `excel_sheet`: for excel sheets (read-only)

## Examples

``` r
.RFLOW <- Rflow::new_rflow()
```

We can define the target nodes using TOML files or directly in R as a
list:

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
        .RFLOW[["DB.mytable"]] %>% 
          RETL::etl_read() %>% 
          summary()
      })
    ),
    
    "main_product" = list(
      desc = "Main output",
      r_expr = expression_r({
        .RFLOW[["DB.mytable"]] %>% some_fancy_computation()
      })
    ),
    
    "DB.output" = list(
      desc = "Outcome is loaded back to the DB",
      type = "db_node",
      r_expr = expression_r({
        .RFLOW[["main_product"]] %>%
          RETL::etl_write(to = self)
      })
    )
  ) 
```

Now we can add these definitions into an existing workflow:

``` r
objs %>% 
  Rflow::process_obj_defs() %>% 
  Rflow::add_nodes(rflow = .RFLOW)
```

and visualize

``` r
Rflow::visRflow(.RFLOW)
```

or build targets

``` r
make(.RFLOW)
```

For more examples see:

  - [Intro 1](./examples/intro1/Rflow_intro_1.md)

## Other similar tools and projects

Rflow overlaps with several other tools in this domain.

  - [GNU make](https://www.gnu.org/software/make/) is a general purpose
    framework from UNIX ecosystem. Compared to `Rflow` `GNU make` is
    strictly file-based. It requires that every job has to produce a
    file.
  - [gnumaker](https://github.com/petebaker/gnumaker) is an R package
    that builds upon `GNU make` and help you generate your make files
    using R.
  - [drake](https://cran.r-project.org/web/packages/drake/) is an R
    package very similar to Rflow. Compared to Rflow, it tracks your
    code dependency automatically, it is able to run your jobs in
    parallel. With `drake`, you are limited to use R langauge. Rflow
    allows you to manage database tables via R or SQL recipes. Support
    for Python and Julia languages is planned too.
  - [orderly](https://github.com/vimc/orderly) framework seems to have
    very similar goals (to tackle the problem of reproducibility with
    various R scripts, inputs and outputs)
  - [Luigi](https://github.com/spotify/luigi) is a workflow management
    framework for Python. Similarly to `make` `luigi` relies on file
    outputs.
  - [Apache Airflow](https://airflow.apache.org/) is a very general and
    sophisticated platform written in
Python.

<!-- ## TODO: -->

<!-- * Add more nuanced `verbose` options -->

<!-- * Add proper logging for rflow object -->

<!-- * Allow deleting properties / setting some to NULL -->

<!--   * currently, if a property is deleted update() method ignores it -->

<!-- * Make all public properties active (mutation would trigger persistence storage) -->

<!-- * Make methods initializing and updating properties more generic. -->

<!-- * Improve test coverage -->

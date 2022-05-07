
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href="https://travis-ci.org/vh-d/Rflow"><img src="https://travis-ci.org/vh-d/Rflow.svg?branch=master" alt="Travis"></a>
<a href="https://codecov.io/github/vh-d/Rflow?branch=master"><img src="https://codecov.io/github/vh-d/Rflow/coverage.svg?branch=master" alt="Codecov"></a>
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# Rflow

## About

*TLDR*:

A library for creating and executing DAGs, everything is stored
(persistent) so you can restart workflows where it failed, support for
R, SQL, Python and Julia.

*Long version*:

Rflow is an R package providing R users with a general-purpose workflow
framework. Rflow allows you to describe your data declaratively as
objects with dependencies and does the heavy lifting for you. It is
suitable for various purposes: from managing several simple automation
scripts to building powerfull ETL pipelines.

Rflow makes your data pipelines better organized and manageable
(workflow can be visualized, objects may have documentation and tags).

It saves your time as your objects are rebuild only when its needed
(also objects are persistent over sessions).

**Development of `Rflow` package is still in its beta version phase
(some breaking changes may happen).**

## Usecases

1.  You have a complex suite of scripts that prepare data from various
    sources for analysis or publication. You need to update our output
    repeatedly – e.g. whenever some of the inputs changes or when some
    of the scripts are changed. You want to organize the tasks and
    inspect the workflow visually.

2.  You need to use R as an ETL tool: to get data from a database,
    transform them using R/SQL/Python/Julia and then to upload them back
    to the database (or other place).

3.  You have complex long-running computations that need to be run only
    when some of the inputs/parameters change. You need to skip the
    parts that we computed the last time with the same inputs.

## Features

What’s working:

-   Languages supported (to be used in tasks): R, SQL, Python, Julia,
    RMarkdown
-   Objects supported: R objects, Python objects, Julia objects, DB/SQL
    tables, files, spreadsheets, R Markdown
-   Bottom-up approach (you choose the final outputs you need, and Rflow
    resolves dependencies)
-   Persistence
-   Logging
-   Plotting and workflow visualization

What’s on the roadmap:

-   Other type of objects: tests, RMarkdowns, xmls
-   Error handling
-   Recovery
-   GUI

## Getting started

### Prerequisites

-   `R` (\>= 3.5.3 tested)
-   `devtools` package

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

An **rflow** represent a directed acyclic graph connecting nodes through
dependency relations. There are three building blocks of rflows:

-   **nodes** (aka targets) represent your data objects such as R
    values, db tables, spreadsheets, files, etc…
-   **environments** serves as containers for nodes. For example a
    database is a container for database tables, R environment is a
    container for R objects, etc…
-   **jobs** represents dependency connection between nodes. It carries
    the recipe how to build a target object.

Currently, we have these types of nodes implemented:

-   `node`: a generic node class (really just a parent class other
    classes inherit from)
-   `r_node`: node representing R objects
-   `db_node`: node representing database tables and views
-   `file_node`: for representing files on disk
-   `csv_node`: descendant of `file_node` for representing csv files
-   `excel_sheet`: for excel sheets (read-only)
-   `julia_node`: node representing a Julia object
-   `python_node`: node representing a Python object
-   `rmd_node`: node representing Rmarkdown targets

## Examples

``` r
MYFLOW <- Rflow::new_rflow()
```

We can define the target nodes using TOML files or directly in R as a
list:

``` r
objs <- 
  list(
    
    "DB.mytable" = list(
      type = "db_node",
      desc = "A db table with data that serves as source for further computation in R"
      sql = "
        CREATE TABLE mytable AS 
        SELECT * FROM customers WHERE year > 2010
      "
    ),
    
    "RENV.mytable_summary" = list(
      type = "r_node", # you can skip this when defining R nodes
      desc = "Summary statistics of DB.mytable",
      depends = "DB.mytable", # dependencies have to be declared (this can be tested/automated)
      r_expr = expression_r({
        .RFLOW[["DB.mytable"]] %>% # Use .RFLOW to refer to the upstream nodes.
          RETL::etl_read() %>% 
          summary()
      })
    ),
    
    "RENV.main" = list(
      desc = "Main output",
      depends = "DB.mytable",
      r_expr = expression_r({
        .RFLOW[["DB.mytable"]] %>% some_fancy_computation()
      })
    ),
    
    "DB.output" = list(
      desc = "Outcome is loaded back to the DB",
      type = "db_node",
      depends = "main_product",
      r_expr = expression_r({
        .RFLOW[["R_OUT"]] %>%
          RETL::etl_write(to = self)
      })
    )
  ) 
```

Now we can add these definitions into an existing workflow:

``` r
objs %>% 
  Rflow::process_obj_defs() %>% 
  Rflow::add_nodes(rflow = MYFLOW)
```

and visualize

``` r
Rflow::visRflow(MYFLOW)
```

or build targets

``` r
make(MYFLOW)
```

For more examples see:

-   [Introduction to Rflow](./examples/intro1/Rflow_intro_1.md)

## Details

### Queries on nodes

Nodes are (R6) objects with properties and methods. You can make queries
to find/filter nodes based on its properties such as tags, time of last
build, etc…

``` r
nodes(RF) %>% # list all nodes in the rflow
  FilterWith("slow" %in% tags) %>% # expressions ("slow" %in% tags) is evaluated within each node, results in list of nodes with positive results
  names()
```

### Non-deterministic jobs

In case a building a node uses a non-deterministic functions (e.g. when
it depends on random numbers, system time, etc…) we can use
`trigger_condition = TRUE` property to override all the other triggers
and to always build the node.

## Why Rflow? (comparison to other similar tools)

Rflow overlaps with several other tools in this domain.

[GNU make](https://www.gnu.org/software/make/) is a general purpose
framework from UNIX ecosystem.
[People](https://towardsdatascience.com/structure-and-automated-workflow-for-a-machine-learning-project-2fa30d661c1e)
use `GNU Make` in their Python/R datascience projects. Compared to
`Rflow` `GNU make` is strictly file-based. It requires that every job
has to produce a file. If your targets are files (you can safe R values
in .RData files too) GNU Mmake may be a good choice.

[gnumaker](https://github.com/petebaker/gnumaker) is an R package that
builds upon `GNU make` and help you generate your make files using R.

[drake](https://cran.r-project.org/web/packages/drake/) is an R package
quite similar to `Rflow`. Compared to `Rflow`, `drake` has more
features: it tracks all your code dependencies automatically (including
functions), it is able to run your jobs in parallel, etc… Currently,
`Rflow` does not track changes in functions in your code. On the
downside `drake` is limited to R langauge. It allows you to define input
and output files, but all the logic has to be implemented in R. `Rflow`
allows you to manage database tables via R or SQL recipes. Support for
Bash, Python and Julia languages or knitting RMarkdwon files is planned
too.

[orderly](https://github.com/vimc/orderly) framework seems to have very
similar goals (to tackle the problem of complexity and reproducibility
with various R scripts, inputs and outputs).

[ProjectTemplate](http://projecttemplate.net/index.html) provides a
standardized sceleton for your project and convenient API for
preprocessing data from various formats and sources.

[Luigi](https://github.com/spotify/luigi) is a popular workflow
management framework for Python.

[Apache Airflow](https://airflow.apache.org/) is a very general and
sophisticated platform written in Python.

<!-- * data is dependent on tasks, tasks are dependent on data, ... -->
<!-- * data is dependent on tasks, tasks are dependent on data, ... -->
<!-- ## TODO: -->
<!-- * Add more nuanced `verbose` options -->
<!-- * Add proper logging for rflow object -->
<!-- * Allow deleting properties / setting some to NULL -->
<!--   * currently, if a property is deleted update() method ignores it -->
<!-- * Make all public properties active (mutation would trigger persistence storage) -->
<!-- * Make methods initializing and updating properties more generic. -->
<!-- * Improve test coverage -->

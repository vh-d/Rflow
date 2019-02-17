
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rflow

Rflow is an R package providing R users a general-purpose workflow
system.

## Installation

…

## Examples

…

## TODO:

**Features**

  - enable defining objects from R scripts (without TOML files)
  - R/SQL code in standalone R scripts (outside of TOML file)
  - more nuanced `verbose` option
  - better logging
      - show result of each trigger
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
  - hashing of leaf nodes (currently only after eval())
  - new node types:
      - Python nodes
      - Julia nodes
      - test node
      - csv file nodes
      - excel sheets (check file change -\> check hash of extracted
        data)
  - Rflow manager as a Shiny app

**Implementation**

  - SQL execution by a generic R function instead of metaprogramming
      - solves potential problems with escaping quotes in SQL code
  - environments as R6 classes
  - add check that Rflow is not empty before plotting etc…
  - store hashes of files in RDS
  - generic methods in node class for initializing and updating
    properties
  - more unit tests
  - make all public properties active (trigger persistence storage)

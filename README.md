
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
  - Python nodes
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
  - quick start guide
  - more SQL dialects (Oracle, Access)
  - Rflow manager as a Shiny app
  - special test node type
  - make storage optional
  - new node types:
      - csv file nodes
      - excel sheets (check file change -\> check hash of extracted
        data)

**Implementation**

  - SQL execution by a generic R function instead of metaprogramming
      - solves potential problems with escaping quotes in SQL code
  - rflow object as R6 class
  - environments as R6 classes
  - add check that Rflow is not empty before plotting etc…
  - store hashes in RDS
  - generic methods in node class for initializing and updating
    properties


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
  - file nodes: option to check hash before triggering
  - add csv file nodes
  - make(): add option to run on end-nodes only

**Implementation**

  - SQL execution by a generic R function instead of metaprogramming
      - solves potential problems with escaping quotes in SQL code
  - rflow object as R6 class
  - environments as R6 classes
  - add check that Rflow is DAG

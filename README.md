
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rflow

Rflow is an R package providing R users a general-purpose workflow
system.

## Installation

…

## Examples

…

## TODO:

  - SQL execution by a generic R function instead of metaprogramming
      - solves potential problems with escaping quotes in SQL code
  - more SQL dialects (Oracle, Access)
  - better logging
      - show result of each trigger
  - reuse node’s generic update() in other nodes
  - handling of obsolete nodes
  - function for removing objects from DAG
  - node method for deleting represented objects
  - deleting properties / setting some tu NULL
      - currently, if a property is deleted update() method ignores it
  - query function to set or get fields of multiple objects
  - quick start guide
  - enable defining objects from R scripts (without TOML files)
  - support for Python nodes

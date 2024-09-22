# Linked Tree

Another abstract data model to process linked data, intended to be used for small datasets processed in real time as tree-like structures. 

Influenced by JSON-LD expanded and flattened document forms. Linked tree terms and values are fully expanded but only identifiable node fragments are merged and flattened. Nodes with no identifiers are kept embedded.

The project is an experiment, please share feedback.

> [!NOTE]
> A stargazer gets a free beer. Collect it when you are in Prague! ;)

## Goals

  * easily work with linked data programmatically  
  * fast intrinsic canonicalization
  * finest processing granularity, i.e. a statement level
  * translation from/into different representations (JSON-LD, RDF forms, CBOR-LD, etc.)
  * language-agnostic

## Motivation
We like to think in graphs, and we like to map things to graphs, but then we prefer to work with trees.

Graphs are great to catch ideas, plans, and knowledge. There are many successful examples. A tree is a basic data structure. It's easy to work it programmatically, and there are many successful examples.

When it comes to software engineering, graphs are hard to work with, graphs are too generic and add unnecessary complexity to a domain limited use-cases.

Think in graphs, work with trees.

## LTDM

![Data Model](/doc/linked-tree-data-model-v1.0.2.png)


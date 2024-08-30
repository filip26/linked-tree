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

## LTDM

![Data Model](/doc/ltdm-0.0.1.png)


## Status

[![Java 17 CI](https://github.com/filip26/linked-tree/actions/workflows/java17-push.yml/badge.svg)](https://github.com/filip26/linked-tree/actions/workflows/java17-push.yml)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

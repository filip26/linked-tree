# Linked Tree

Another abstract data model to process linked data, intended to be used for small datasets processed in real time as tree-like structures. Influenced by JSON-LD expanded and flattened document forms.

Linked tree terms and values are fully expanded but only identifiable node fragments are merged and flattened. Nodes with no identifiers are kept embedded.

> [!IMPORTANT]
> The project is an experiment, please share feedback.


## Status

[![Java 17 CI](https://github.com/filip26/linked-tree/actions/workflows/java17-push.yml/badge.svg)](https://github.com/filip26/linked-tree/actions/workflows/java17-push.yml)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


## Goals

  * easily work with linked data programmatically  
  * fast intrinsic canonicalization
  * finest processing granularity, i.e. a statement level
  * translation from/into different representations (JSON-LD, RDF forms, CBOR-LD, etc.)
  * language-agnostic

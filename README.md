# Linked Trees

Another abstract data model to process linked data, intended to be used for small datasets serialized and exchanged as tree-like structures. Influenced by JSON-LD expanded and flattened document forms.

Linked tree terms and values are fully expanded but only identifiable node fragments are merged and flattened. Nodes with no identifiers are kept embedded.

The project is an experiment, please share feedback.

## Goals

* easily work with linked data programmatically  
* seamless translation from/into different representations (JSON-LD, RDF forms, CBOR-LD, etc.)
* intrinsically canonical
* finest data processing granularity, i.e. a statement level
* language-agnostic

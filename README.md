![Data Model](/doc/linked-tree-data-model-v1.0.5.png)

# Linked Tree

An alternative abstract data model for processing linked data, designed for small datasets to be handled in real-time as tree-like structures.

Inspired by the expanded and flattened document forms of JSON-LD, Linked Tree terms and values are fully expanded. However, only identifiable node fragments are merged and flattened, while nodes without identifiers remain embedded.

JSON-LD offers a comprehensive set of features to transform any JSON into any graph structure. Linked Trees, by contrast, takes a different approach, aiming to provide a simple and straightforward way to convert JSON into Linked Data while leaving complex corner cases aside.

In many cases, <20% of JSON-LD’s functionality is sufficient. The absence of features like @reverse, @nested, or framing is outweighed by the simplicity and clarity that Linked Trees offer.

This project is an experiment - your feedback is highly appreciated!

## Goals

 * **Effortless Programmatic Interaction** - Enable seamless handling of linked data programmatically.
 * **Fast Intrinsic Canonicalization** - Ensure rapid and efficient canonicalization.
 * **Fine-Grained Processing** - Support the finest level of granularity, down to individual statements.
 * **Interoperable Representation** - Facilitate translation to and from various representations (e.g., JSON-LD, RDF forms, CBOR-LD).
 * **Language-Agnostic** - Maintain compatibility across diverse programming languages.

## Motivation

We often think in terms of graphs and map concepts to graphs, but when it comes to practical application, we prefer working with trees.

Graphs excel at capturing ideas, plans, and knowledge, with numerous successful examples showcasing their utility. However, trees, as a fundamental data structure, are simpler to work with programmatically and are also backed by many success stories.

In software engineering, graphs can be challenging to handle. Their generic nature often introduces unnecessary complexity, especially in domains with limited use cases.

Think in graphs, work with trees.

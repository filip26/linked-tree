package com.apicatalog.linkedtree.traversal;

@FunctionalInterface
public interface NodeConsumer<T> {

    void accept(
            T node,
            int indexOrder,
            String indexTerm,
            int depth);
}

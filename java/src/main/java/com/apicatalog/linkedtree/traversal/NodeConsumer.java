package com.apicatalog.linkedtree.traversal;

import com.apicatalog.linkedtree.builder.TreeBuilderError;

@FunctionalInterface
public interface NodeConsumer<T> {

    void accept(
            T node,
            int indexOrder,
            String indexTerm,
            int depth) throws TreeBuilderError;
}

package com.apicatalog.linkedtree.traversal;

import com.apicatalog.linkedtree.LinkedTreeError;

@FunctionalInterface
public interface NodeConsumer<T> {

    void accept(
            T node,
            int indexOrder,
            String indexTerm,
            int depth) throws LinkedTreeError;
}

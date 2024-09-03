package com.apicatalog.linkedtree.traversal;

import com.apicatalog.linkedtree.LinkedNode;

@FunctionalInterface
public interface NodeConsumer {

    void accept(
            LinkedNode node,
            int indexOrder,
            String indexTerm,
            int depth);
}

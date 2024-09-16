package com.apicatalog.linkedtree.traversal;

import com.apicatalog.linkedtree.builder.TreeBuilderError;

@FunctionalInterface
public interface NodeSelector<T> {

    public enum TraversalPolicy {
        /** consumed and followed */
        Accept,
        /* consumed, not followed */
        Stop,
        /** not consumed, not followed */
        Drop,
        /** not consumed, but followed */
        Ignore,
    }
    
    TraversalPolicy test(
            T node,
            int indexOrder,
            String indexTerm,
            int depth) throws TreeBuilderError;
}

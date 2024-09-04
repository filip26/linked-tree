package com.apicatalog.linkedtree.traversal;

import com.apicatalog.linkedtree.LinkedNode;

@FunctionalInterface
public interface NodeSelector {

    public enum ProcessingPolicy {
        /* consumed and followed */
        Accept,
        /* not consumed, not followed */
        Drop,
        /* not consumed, but followed */
        Ignore,
    }
    
    ProcessingPolicy test(
            LinkedNode node,
            int indexOrder,
            String indexTerm,
            int depth);
}

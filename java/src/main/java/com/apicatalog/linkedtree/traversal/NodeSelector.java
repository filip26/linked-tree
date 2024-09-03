package com.apicatalog.linkedtree.traversal;

import com.apicatalog.linkedtree.LinkedNode;

@FunctionalInterface
public interface NodeSelector {

    public enum ProcessingPolicy {
        /* consumed and followed */
        Accepted,
        /* not consumed, not followed */
        Dropped,
        /* not consumed, but followed */
        Ignored,
    }
    
    ProcessingPolicy test(
            LinkedNode node,
            int indexOrder,
            String indexTerm,
            int depth);
}

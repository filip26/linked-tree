package com.apicatalog.linkedtree.traversal;

@FunctionalInterface
public interface NodeSelector<T> {

    public enum ProcessingPolicy {
        /** consumed and followed */
        Accept,
        //TODO 
        /* consumed, not followed */
        Stop,
        /** not consumed, not followed */
        Drop,
        /** not consumed, but followed */
        Ignore,
    }
    
    ProcessingPolicy test(
            T node,
            int indexOrder,
            String indexTerm,
            int depth);
}

package com.apicatalog.linkedtree.adapter;

import com.apicatalog.linkedtree.LinkedNode;

@FunctionalInterface
public interface NodeAdapter<T extends LinkedNode, R> {

    /**
     * Creates a new object instance initialized by values found in the given
     * source object.
     * 
     * @param source
     * @return a new instance
     * @throws NodeAdapterError
     */
    R materialize(T source) throws NodeAdapterError;
}

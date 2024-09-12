package com.apicatalog.linkedtree.adapter;

public interface Adapter<T, R> {

    /**
     * Creates a new object instance initialized by values found in the given
     * source object.
     * 
     * @param source
     * @return a new instance
     * @throws AdapterError
     */
    R materialize(T source) throws AdapterError;
}

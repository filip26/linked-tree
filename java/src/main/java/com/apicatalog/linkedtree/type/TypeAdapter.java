package com.apicatalog.linkedtree.type;

import com.apicatalog.linkedtree.LinkedFragment;

@FunctionalInterface
public interface TypeAdapter {

    /**
     * Creates a new object instance initialized by values found in the given
     * {@link LinkedFragment}.
     * 
     * @param fragment
     * @return a new instance
     * @throws TypeAdapterError
     */
    Object materialize(LinkedFragment fragment) throws TypeAdapterError;

    default Class<?> typeInterface() {
        return this.getClass();
    }
}

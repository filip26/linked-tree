package com.apicatalog.linkedtree.type;

import com.apicatalog.linkedtree.LinkedFragment;

public interface TypeAdapter {

    /**
     * Creates a new object instance initialized by values found in the given
     * {@link LinkedFragment}.
     * 
     * @param fragment
     * @return a new instance
     * @throws TypeAdapterError
     */
    <T> T materialize(LinkedFragment fragment) throws TypeAdapterError;

    Class<?> typeInterface();
}

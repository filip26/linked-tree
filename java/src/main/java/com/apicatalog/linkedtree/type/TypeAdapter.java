package com.apicatalog.linkedtree.type;

import com.apicatalog.linkedtree.LinkedNode;

public interface TypeAdapter {

    /**
     * Creates a new object instance initialized by values found in the given
     * {@link LinkedNode}.
     * 
     * @param node
     * @return a new instance
     * @throws TypeAdapterError
     */
    <T> T adapt(LinkedNode node) throws TypeAdapterError;

    Class<?> typeInterface(); 
//    {
//        return Object.class;
//    }
}

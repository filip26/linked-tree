package com.apicatalog.linkedtree.type;

import com.apicatalog.linkedtree.LinkedNode;

public class GenericTypeAdapter implements TypeAdapter {

    @SuppressWarnings("unchecked")
    @Override
    public <T> T adapt(LinkedNode node) throws TypeAdapterError {
        return (T)node;
    }

    @Override
    public Class<?> typeInterface() {
        return LinkedNode.class;
    }

}

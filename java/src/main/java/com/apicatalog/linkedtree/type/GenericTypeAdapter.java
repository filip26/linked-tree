package com.apicatalog.linkedtree.type;

import com.apicatalog.linkedtree.LinkedFragment;

public class GenericTypeAdapter implements TypeAdapter {

    @SuppressWarnings("unchecked")
    @Override
    public <T> T materialize(LinkedFragment fragment) throws TypeAdapterError {
        return (T)fragment;
    }

    @Override
    public Class<?> typeInterface() {
        return LinkedFragment.class;
    }

}

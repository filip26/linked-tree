package com.apicatalog.linkedtree.type;

import com.apicatalog.linkedtree.LinkedFragment;

public class GenericTypeAdapter implements TypeAdapter {

    @Override
    public Object materialize(LinkedFragment fragment) throws TypeAdapterError {
        return fragment;
    }

    @Override
    public Class<?> typeInterface() {
        return LinkedFragment.class;
    }

}

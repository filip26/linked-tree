package com.apicatalog.linkedtree.type;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.Adapter;
import com.apicatalog.linkedtree.adapter.AdapterError;

public record GenericTypeAdapter(
        Class<?> typeInterface,
        Adapter<LinkedFragment, Object> materialize
        ) implements TypeAdapter {

    @Override
    public Object materialize(LinkedFragment fragment) throws AdapterError {
        return materialize().materialize(fragment);
    }
}

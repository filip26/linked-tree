package com.apicatalog.linkedtree.type;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapter;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public record GenericTypeAdapter(
        Class<?> typeInterface,
        NodeAdapter<LinkedFragment, Object> materialize
        ) implements TypeAdapter {

    @Override
    public Object materialize(LinkedFragment fragment) throws NodeAdapterError {
        return materialize().materialize(fragment);
    }
}

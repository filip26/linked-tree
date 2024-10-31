package com.apicatalog.linkedtree.literal.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public record GenericLiteralAdapter(
        String datatype,
        Class<? extends LinkedLiteral> typeInterface,
        LiteralAdapter adapter) implements DataTypeAdapter {

    @Override
    public LinkedLiteral materialize(String source) throws NodeAdapterError {
        return adapter().materialize(source);
    }
}

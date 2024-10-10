package com.apicatalog.linkedtree.literal.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public record GenericLiteralAdapter(
        String datatype,
        Class<? extends LinkedLiteral> typeInterface,
        LiteralAdapter adapter) implements TypedLiteralAdapter {

    @Override
    public LinkedLiteral materialize(String source, LinkedTree root) throws NodeAdapterError {
        return adapter().materialize(source, root);
    }
}

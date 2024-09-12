package com.apicatalog.linkedtree.literal.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.AdapterError;
import com.apicatalog.linkedtree.adapter.NodeAdapter;

public record GenericLiteralAdapter(
        String datatype,
        NodeAdapter<String, LinkedLiteral> adapter
        ) implements LiteralAdapter {

    @Override
    public LinkedLiteral materialize(String source, LinkedTree root) throws AdapterError {
        return adapter().materialize(source, root);
    }

}

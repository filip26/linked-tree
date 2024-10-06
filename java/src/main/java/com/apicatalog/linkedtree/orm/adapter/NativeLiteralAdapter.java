package com.apicatalog.linkedtree.orm.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.literal.adapter.TypedLiteralAdapter;

public interface NativeLiteralAdapter {

    default void setup(String[] params) throws NodeAdapterError {
        /* empty */ }

    TypedLiteralAdapter literalAdapter();

    Object materialize(Class<?> type, LinkedLiteral literal) throws NodeAdapterError;
}

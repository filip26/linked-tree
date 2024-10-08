package com.apicatalog.linkedtree.orm.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.literal.adapter.TypedLiteralAdapter;

@FunctionalInterface
public interface LiteralMapper {

    default void setup(String[] params) throws NodeAdapterError {
        /* empty */ }

    default TypedLiteralAdapter adapter() {
        return null;
    }

    Object map(Class<?> type, LinkedLiteral literal) throws NodeAdapterError;

}

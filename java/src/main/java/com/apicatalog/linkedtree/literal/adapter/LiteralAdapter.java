package com.apicatalog.linkedtree.literal.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

@FunctionalInterface
public interface LiteralAdapter {

    LinkedLiteral materialize(String source, LinkedTree root) throws NodeAdapterError;
}

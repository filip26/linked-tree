package com.apicatalog.linkedtree.orm.mapper;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

@FunctionalInterface
public interface LiteralMapper<T extends LinkedLiteral, R> {

    R map(T literal) throws NodeAdapterError;

}

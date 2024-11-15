package com.apicatalog.linkedtree.orm.mapper;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

@FunctionalInterface
public interface ObjectReader<T extends LinkedLiteral, R> {

    R object(T literal) throws NodeAdapterError;

}

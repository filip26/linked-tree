package com.apicatalog.linkedtree.orm.mapper;

import com.apicatalog.linkedtree.LinkedLiteral;

@FunctionalInterface
public interface ObjectWriter<T> {

    LinkedLiteral literal(T object);

}

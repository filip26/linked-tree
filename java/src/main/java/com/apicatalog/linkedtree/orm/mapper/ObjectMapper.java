package com.apicatalog.linkedtree.orm.mapper;

import com.apicatalog.linkedtree.LinkedLiteral;

public interface ObjectMapper<T extends LinkedLiteral, R> extends
        ObjectReader<T, R>,
        ObjectWriter<R> {
}

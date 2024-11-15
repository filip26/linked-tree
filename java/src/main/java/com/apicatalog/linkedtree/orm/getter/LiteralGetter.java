package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.orm.mapper.ObjectReader;

public class LiteralGetter implements Getter {

    String term;
    Class<?> returnType;
    ObjectReader<LinkedLiteral, ?> adapter;

    public LiteralGetter(
            String term,
            Class<?> returnType,
            ObjectReader<LinkedLiteral, ?> adapter) {
        this.term = term;
        this.returnType = returnType;
        this.adapter = adapter;
    }

    public Object get(LinkedFragment source) throws NodeAdapterError {

        LinkedLiteral literal = source.literal(term, LinkedLiteral.class);
        if (literal == null) {
            return null;
        }
        return adapter.object(literal);
    }

}

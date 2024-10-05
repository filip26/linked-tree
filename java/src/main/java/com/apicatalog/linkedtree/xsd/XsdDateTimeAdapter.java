package com.apicatalog.linkedtree.xsd;

import java.time.Instant;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.literal.DateTimeValue;
import com.apicatalog.linkedtree.orm.adapter.NativeLiteralAdapter;

public class XsdDateTimeAdapter implements NativeLiteralAdapter {

    @Override
    public LinkedLiteral materialize(String source, LinkedTree root) throws NodeAdapterError {
        return XsdDateTime.of(source, root);
    }

    @Override
    public Object materialize(Class<?> type, LinkedLiteral literal) throws NodeAdapterError {
        if (type.isAssignableFrom(Instant.class)) {
            if (literal instanceof DateTimeValue datetime) {
                return datetime.datetime();
            }
        }

        return null;
    }

    @Override
    public String datatype() {
        return XsdDateTime.TYPE;
    }
}

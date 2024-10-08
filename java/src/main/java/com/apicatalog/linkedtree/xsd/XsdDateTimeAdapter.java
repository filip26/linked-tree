package com.apicatalog.linkedtree.xsd;

import java.sql.Date;
import java.time.Instant;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.literal.DateTimeValue;
import com.apicatalog.linkedtree.literal.adapter.TypedLiteralAdapter;
import com.apicatalog.linkedtree.orm.adapter.LiteralMapper;

public class XsdDateTimeAdapter implements LiteralMapper {

    @Override
    public Object map(Class<?> type, LinkedLiteral literal) throws NodeAdapterError {
        if (literal instanceof DateTimeValue datetime) {
            if (type.isAssignableFrom(Instant.class)) {
                return datetime.datetime();
            }
            if (type.isAssignableFrom(Date.class)) {
                return datetime.datetime() != null
                        ? Date.from(datetime.datetime())
                        : null; 
            }
        }
        throw new ClassCastException("Cannot be cast to " + type.getCanonicalName());
    }

    @Override
    public TypedLiteralAdapter adapter() {
        return XsdDateTime.typeAdapter();
    }
}

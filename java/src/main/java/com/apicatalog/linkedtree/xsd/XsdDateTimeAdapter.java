package com.apicatalog.linkedtree.xsd;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.literal.adapter.DataTypeAdapter;

public class XsdDateTimeAdapter implements DataTypeAdapter {

    static XsdDateTimeAdapter INSTANCE = new XsdDateTimeAdapter();

    @Override
    public LinkedLiteral materialize(String source, LinkedNode parent) throws NodeAdapterError {
        return XsdDateTime.of(source);
    }

    @Override
    public String datatype() {
        return XsdVocab.DATE_TIME;
    }

    @Override
    public Class<? extends LinkedLiteral> typeInterface() {
        return XsdDateTime.class;
    }

//    @Override
//    public Object map(Class<Object> type, LinkedLiteral literal) throws NodeAdapterError {
//        if (literal instanceof DateTimeValue datetime) {
//            if (type.isAssignableFrom(Instant.class)) {
//                return datetime.datetime();
//            }
//            if (type.isAssignableFrom(Date.class)) {
//                return datetime.datetime() != null
//                        ? Date.from(datetime.datetime())
//                        : null; 
//            }
//        }
//        throw new ClassCastException("Cannot be cast to " + type.getCanonicalName());
//    }
}

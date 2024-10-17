package com.apicatalog.linkedtree.xsd;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.literal.adapter.DataTypeAdapter;

public class XsdDateTimeAdapter implements DataTypeAdapter {

    static XsdDateTimeAdapter INSTANCE = new XsdDateTimeAdapter();

    @Override
    public LinkedLiteral materialize(String source) throws NodeAdapterError {
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
}

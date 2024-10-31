package com.apicatalog.linkedtree.xsd;

import java.time.Instant;
import java.time.temporal.ChronoUnit;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.literal.adapter.DataTypeAdapter;
import com.apicatalog.linkedtree.literal.adapter.DataTypeNormalizer;

public class XsdDateTimeAdapter implements DataTypeAdapter, DataTypeNormalizer<Instant> {

    static XsdDateTimeAdapter INSTANCE = new XsdDateTimeAdapter();

    @Override
    public LinkedLiteral materialize(String source) throws NodeAdapterError {
        return XsdDateTime.of(source);
    }

    @Override
    public String normalize(Instant value) {
        return value.truncatedTo(ChronoUnit.MILLIS).toString();
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

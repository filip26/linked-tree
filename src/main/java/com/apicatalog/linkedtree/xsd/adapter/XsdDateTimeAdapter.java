package com.apicatalog.linkedtree.xsd.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.LinkedLiteralAdapter;
import com.apicatalog.linkedtree.xsd.XsdConstants;
import com.apicatalog.linkedtree.xsd.XsdDateTime;

public class XsdDateTimeAdapter implements LinkedLiteralAdapter {

    @Override
    public String datatype() {
        return XsdConstants.DATE_TIME;
    }

    @Override
    public LinkedLiteral read(String value, Object meta) {
        return XsdDateTime.of(value, meta);
    }

}

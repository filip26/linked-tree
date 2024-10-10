package com.apicatalog.linkedtree.literal.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;

public interface DataTypeAdapter extends LiteralAdapter {

    String datatype();

    Class<? extends LinkedLiteral> typeInterface();
}

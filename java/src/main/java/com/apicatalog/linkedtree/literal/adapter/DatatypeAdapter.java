package com.apicatalog.linkedtree.literal.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;

public interface DatatypeAdapter extends LiteralAdapter {

    Object materialize(LinkedLiteral literal);

}

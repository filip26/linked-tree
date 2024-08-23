package com.apicatalog.linkedtree.io;

import com.apicatalog.linkedtree.LinkedLiteral;

public interface LinkedLiteralAdapter {

    String datatype();

    LinkedLiteral read(String value, Object meta);

}

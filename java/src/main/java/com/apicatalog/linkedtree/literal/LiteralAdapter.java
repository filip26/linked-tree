package com.apicatalog.linkedtree.literal;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;

public interface LiteralAdapter {

    LinkedLiteral materialize(String value, LinkedTree root);

    String datatype();
}

package com.apicatalog.linkedtree.reader;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;

@FunctionalInterface
public interface LiteralReader {

    LinkedLiteral read(String value, LinkedTree root);

}

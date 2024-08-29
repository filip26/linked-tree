package com.apicatalog.linkedtree.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;

@FunctionalInterface
public interface LinkedLiteralReader {

    //TODO type
    LinkedLiteral read(String value);

}

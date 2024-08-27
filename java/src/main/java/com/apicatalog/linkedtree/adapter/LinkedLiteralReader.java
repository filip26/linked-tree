package com.apicatalog.linkedtree.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;

@FunctionalInterface
public interface LinkedLiteralReader {

    LinkedLiteral read(String value);

}

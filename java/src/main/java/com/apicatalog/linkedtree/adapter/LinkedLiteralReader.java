package com.apicatalog.linkedtree.adapter;

import java.util.function.Supplier;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;

@FunctionalInterface
public interface LinkedLiteralReader {

    //TODO type
    LinkedLiteral read(String value, Supplier<LinkedTree> rootSupplier);

}

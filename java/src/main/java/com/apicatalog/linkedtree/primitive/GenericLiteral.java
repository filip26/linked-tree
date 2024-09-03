package com.apicatalog.linkedtree.primitive;

import java.util.function.Supplier;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;

public record GenericLiteral(
        String lexicalValue,
        String datatype,
        Supplier<LinkedTree> rootSupplier) implements LinkedLiteral {

    @Override
    public LinkedTree root() {
        return rootSupplier != null
                ? rootSupplier.get()
                : null;
    }
}

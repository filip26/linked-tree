package com.apicatalog.linkedtree.primitive;

import com.apicatalog.linkedtree.LinkedLiteral;

public record GenericLiteral(
        String lexicalValue,
        String datatype) implements LinkedLiteral {

}

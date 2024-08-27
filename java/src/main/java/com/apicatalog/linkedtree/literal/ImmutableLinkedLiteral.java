package com.apicatalog.linkedtree.literal;

import com.apicatalog.linkedtree.LinkedLiteral;

public record ImmutableLinkedLiteral(
        String lexicalValue,
        String datatype) implements LinkedLiteral {

}

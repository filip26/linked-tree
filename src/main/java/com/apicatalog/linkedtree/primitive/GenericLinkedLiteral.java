package com.apicatalog.linkedtree.primitive;

import com.apicatalog.linkedtree.LinkedLiteral;

public record GenericLinkedLiteral(
        String value,
        String datatype,
        Object pi) implements LinkedLiteral {

}

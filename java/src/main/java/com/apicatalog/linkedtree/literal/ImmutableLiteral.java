package com.apicatalog.linkedtree.literal;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;

public record ImmutableLiteral(
        String lexicalValue,
        String datatype,
        LinkedTree root) implements LinkedLiteral {

}

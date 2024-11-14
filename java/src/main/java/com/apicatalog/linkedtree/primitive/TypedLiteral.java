package com.apicatalog.linkedtree.primitive;

import com.apicatalog.linkedtree.LinkedLiteral;

public record TypedLiteral(
        String datatype,
        String lexicalValue
        ) implements LinkedLiteral {
}

package com.apicatalog.linkedtree.literal;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public record ImmutableLinkedLiteral(
        String lexicalValue,
        String datatype,
        ProcessingInstruction pi) implements LinkedLiteral {

}

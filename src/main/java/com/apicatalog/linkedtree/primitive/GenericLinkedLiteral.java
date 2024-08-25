package com.apicatalog.linkedtree.primitive;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public record GenericLinkedLiteral(
        String value,
        String datatype,
        ProcessingInstruction pi) implements LinkedLiteral {

}

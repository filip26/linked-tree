package com.apicatalog.linkedtree.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

@FunctionalInterface
public interface LinkedLiteralReader {

    LinkedLiteral read(String value, ProcessingInstruction pi);

}

package com.apicatalog.linkedtree.io;

import com.apicatalog.linkedtree.LinkedLiteral;

public interface LinkedLiteralAdapter {
    
    String datatype();
    
    //TODO @index?
    LinkedLiteral read(String value, String language, String direction);
    
}

package com.apicatalog.linkedtree.primitive;

import com.apicatalog.linkedtree.LinkedLiteral;

public class GenericLinkedLiteral implements LinkedLiteral {
    
    protected String value;
    protected String datatype;
    protected String language;
    
    protected GenericLinkedLiteral() {
        
    }
    
    public static GenericLinkedLiteral of(String value, String datatype, String language, String direction) {
        final GenericLinkedLiteral node = new GenericLinkedLiteral();
        node.value = value;
        node.datatype = datatype;
        node.language = language;
        return node;
    }
    
    @Override
    public String value() {
        return value;
    }

    @Override
    public String datatype() {
        return datatype;
    }

    @Override
    public String language() {
        return language;
    }
    
}

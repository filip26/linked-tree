package com.apicatalog.linkedtree.primitive;

import com.apicatalog.linkedtree.value.LangString;
import com.apicatalog.linkedtree.xsd.XsdConstants;

public class GenericLangString implements LangString {
    
    protected String value;
    protected String datatype;
    protected String language;
    
    protected Object meta;
    
    protected GenericLangString() {
        
    }
    
    public static GenericLangString of(String value, String language, String direction, Object meta) {
        final GenericLangString node = new GenericLangString();
        node.value = value;
        node.datatype = XsdConstants.STRING;
        node.language = language;
        node.meta = meta;
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

    @Override
    public Object metadata() {
        return meta;
    }    
}

package com.apicatalog.linkedtree.jsonld.primitive;

import com.apicatalog.linkedtree.primitive.GenericLinkedLiteral;

public class JsonLdLiteral extends GenericLinkedLiteral {
    
    protected String  index;
    
    protected JsonLdLiteral() {
        // protected
    }
    
    public static JsonLdLiteral of(String value, String datatype, String language, String direction, String index) {
        final JsonLdLiteral node = new JsonLdLiteral();
        node.value = value;
        node.datatype = datatype;
        node.language = language;
        node.index = index;
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
    
    public String index() {
        return index;
    }
}

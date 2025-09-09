package com.apicatalog.anchored;

public interface Literal extends Node {

    @Override
    default NodeType nodeType() {
        return NodeType.Literal;
    }
    
    String datatype();
    
    String lexicalValue();

}

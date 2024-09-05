package com.apicatalog.linkedtree.type;

import com.apicatalog.linkedtree.LinkedNode;

public interface Type {

    String uri();
    
    <T> T cast(Class<T> clazz, LinkedNode node) throws ClassCastException;
    
    <T> Class<T> clazz();
    
    @Override 
    boolean equals(Object obj);
    
    @Override
    int hashCode();
}

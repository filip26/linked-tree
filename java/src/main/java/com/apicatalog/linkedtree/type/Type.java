package com.apicatalog.linkedtree.type;

import java.util.Collection;

public interface Type {

    String uri();
    
    <T> T cast(Class<T> clazz) throws ClassCastException;
    
    Collection<Class<?>> interfaces();
    
    @Override 
    boolean equals(Object obj);
    
    @Override
    int hashCode();
}

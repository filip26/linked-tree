package com.apicatalog.linkedtree.type;

import java.util.Collection;

public interface TypeSelector extends Iterable<Type> {

    <T> T cast(Class<T> clazz) throws ClassCastException;
    
    boolean includes(String type);
    
    Type get(String type);
    
    Collection<Class<?>> interfaces();
}

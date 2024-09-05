package com.apicatalog.linkedtree.type;

public interface TypeSelector extends Iterable<String> {

    <T> T cast(Class<T> clazz) throws ClassCastException;
    
    boolean includes(String type);
    
    
}

package com.apicatalog.linkedtree;

import java.util.Collection;
import java.util.Collections;

public non-sealed interface LinkedContainer extends LinkedNode {

    public static LinkedContainer EMPTY = new LinkedContainer() {};
    
    public enum Type {
        OrderedList,
        UnorderedSet
    };
    
    default Type containerType() {
        return Type.UnorderedSet;
    }
    
    default Collection<LinkedNode> nodes() {
        return Collections.emptyList();
    }
        
    @Override
    default boolean isContainer() {
        return true;
    }

    @Override
    default LinkedContainer asContainer() {
        return this;
    }
}

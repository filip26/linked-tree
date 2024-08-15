package com.apicatalog.linkedtree;

public sealed interface LinkedData permits LinkedTree, LinkedContainer, LinkedNode, LinkedValue {

    default boolean isTree() {
        return false;
    }
    
    default boolean isNode() {
        return false;
    }

    default boolean isValue() {
        return false;
    }
    
    default LinkedNode asNode() {
        throw new ClassCastException();
    }

    default LinkedNode asTree() {
        throw new ClassCastException();
    }

    default LinkedValue asValue() {
        throw new ClassCastException();
    }
}

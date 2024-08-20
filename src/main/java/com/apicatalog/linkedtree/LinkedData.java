package com.apicatalog.linkedtree;

public sealed interface LinkedData permits LinkedTree, LinkedContainer, LinkedFragment, LinkedValue {

    default boolean isTree() {
        return false;
    }
    
    default boolean isNode() {
        return false;
    }

    default boolean isValue() {
        return false;
    }
    
    default LinkedFragment asNode() {
        throw new ClassCastException();
    }

    default LinkedFragment asTree() {
        throw new ClassCastException();
    }

    default LinkedValue asValue() {
        throw new ClassCastException();
    }
}

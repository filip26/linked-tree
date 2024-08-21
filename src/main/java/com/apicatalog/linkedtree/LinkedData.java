package com.apicatalog.linkedtree;

public sealed interface LinkedData permits LinkedTree, LinkedContainer, LinkedFragment, LinkedLiteral {

    default boolean isTree() {
        return false;
    }
    
    default boolean isFragment() {
        return false;
    }

    default boolean isLiteral() {
        return false;
    }
    
    default LinkedFragment asFragment() {
        throw new ClassCastException();
    }

    default LinkedTree asTree() {
        throw new ClassCastException();
    }

    default LinkedLiteral asLiteral() {
        throw new ClassCastException();
    }
}

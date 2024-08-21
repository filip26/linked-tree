package com.apicatalog.linkedtree;

public sealed interface LinkedNode permits LinkedContainer, LinkedFragment, LinkedLiteral {

    default boolean isTree() {
        return false;
    }

    //TODO unify container and tree
    default boolean isContainer() {
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
    
    default LinkedContainer asContainer() {
        throw new ClassCastException();
    }
}

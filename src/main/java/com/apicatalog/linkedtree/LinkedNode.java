package com.apicatalog.linkedtree;

public sealed interface LinkedNode permits LinkedContainer, LinkedTree, LinkedFragment, LinkedLiteral {

    default boolean isTree() {
        return false;
    }

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
    
    //TODO not sure about this - @index should not be supported, JSON-LD specific feature 
//    default String index() {
//        return null;
//    }
}

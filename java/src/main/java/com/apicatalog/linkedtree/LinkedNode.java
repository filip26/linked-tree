package com.apicatalog.linkedtree;

public interface LinkedNode extends Linkable {

    default LinkedTree root() {
        return null;
    }
    
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

    @Override
    boolean equals(Object obj);

    @Override
    int hashCode();
}

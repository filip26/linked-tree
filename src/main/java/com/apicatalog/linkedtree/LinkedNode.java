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

    /**
     * A custom processing instructions (temporary solution).
     * 
     * @return custom data
     */
    default Object pi() {
        return null;
    }
}

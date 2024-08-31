package com.apicatalog.linkedtree;

public interface LinkedNode extends Linkable {

    /**
     * A {@link LinkedTree} instance to which the {@link LinkedNode} belongs to.
     * 
     * Please note a tree instance can have a root if is a child node of another
     * tree instance.
     * 
     * @return an instance or <code>null</code> if the node is a root
     */
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

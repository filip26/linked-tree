package com.apicatalog.linkedtree;

import java.util.Collection;
import java.util.Collections;

public non-sealed interface LinkedTree extends LinkedFragment, LinkedNode {

    static LinkedTree EMPTY = new LinkedTree() {
    };

    @Override
    default boolean isTree() {
        return true;
    }

    @Override
    default LinkedTree asTree() {
        return this;
    }

    /**
     * root fragments
     * 
     * @return a collection of root nodes
     */
    default Collection<LinkedNode> nodes() {
        return Collections.emptyList();
    }

    /**
     * identifiable fragments index
     * 
     * @return a collection of links found in the tree
     */
    default Collection<Link> links() {
        return Collections.emptyList();
    }

    // TODO predicates. i.e. terms???
}

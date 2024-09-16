package com.apicatalog.linkedtree;

import java.util.Collection;

import com.apicatalog.linkedtree.link.Link;

public interface LinkedTree extends LinkedFragment, LinkedContainer, LinkedNode {

    @Override
    default boolean isTree() {
        return true;
    }

    @Override
    default LinkedTree asTree() {
        return this;
    }

    /**
     * Identifiable fragments index
     * 
     * @return a list of all links found in the tree, never <code>null</code>
     */
    Collection<Link> links();

    /**
     * A set of all subtrees found
     * 
     * @return a set of all subtrees, never <code>null</code>
     */
    Collection<LinkedTree> subtrees();

    @Override
    default ContainerType containerType() {
        return ContainerType.Tree;
    }
}

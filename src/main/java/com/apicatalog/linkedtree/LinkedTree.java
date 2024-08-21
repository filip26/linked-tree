package com.apicatalog.linkedtree;

import java.util.Collection;
import java.util.Collections;

public interface LinkedTree extends LinkedFragment, LinkedContainer {

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
    @Override
    default Collection<LinkedNode> nodes() {
        return Collections.emptyList();
    }

    /**
     * identifiable fragments
     * 
     * @return a collection of links found in the tree
     */
    default Collection<Link> links() {
        return Collections.emptyList();
    }
    
    @Override
    default String containerType() {
        return "@graph";
    }

    // TODO predicates. i.e. terms???
}

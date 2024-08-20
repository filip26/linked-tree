package com.apicatalog.linkedtree;

import java.util.Collection;
import java.util.Collections;

public non-sealed interface LinkedTree extends LinkedData {

    static LinkedTree EMPTY = new LinkedTree() {};
    
    /**
     * root fragments
     * 
     * @return a collection of root fragments
     */
    default Collection<LinkedFragment> fragments() {
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

    // TODO predicates. i.e. terms???
}

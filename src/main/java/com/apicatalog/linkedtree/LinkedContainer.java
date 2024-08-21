package com.apicatalog.linkedtree;

import java.util.Collection;

public non-sealed interface LinkedContainer extends LinkedNode {

    Collection<LinkedNode> nodes();
    
    String containerType();
    
    @Override
    default boolean isContainer() {
        return true;
    }

    @Override
    default LinkedContainer asContainer() {
        return this;
    }
}

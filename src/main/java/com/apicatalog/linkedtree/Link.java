package com.apicatalog.linkedtree;

import java.util.Collection;

public interface Link {

    String uri();

    /**
     * All fragments using the same id found in a tree - never adapted
     * 
     * @return a collection of fragments in a tree
     */
    Collection<LinkedFragment> fragments();

    /**
     * A fragment composed of all fragments - adapted if an adapter is present
     * 
     * @return a fragment
     */
    LinkedFragment target();

    // TODO isblank, etc
}

package com.apicatalog.linkedtree;

import java.util.Collection;

public interface Link {

    String uri();

    /**
     * All fragments using the same id found in a tree - never adapted
     * 
     * @return a collection of fragments in a tree
     */
    Collection<LinkedFragment> refs();

    /**
     * A fragment composed of all {@link Link#refs()}, adapted if an appropriate
     * adapter is found.
     * 
     * @return a fragment
     */
    LinkedFragment target();

    /**
     * Checks if the link targets a blank node
     * 
     * @return <code>true</code> if the link targets a blank node
     */
    default boolean isBlank() {
        final String uri = uri();
        return uri != null && uri.startsWith("_:");
    }
}

package com.apicatalog.linkedtree.link;

import java.util.Collection;

import com.apicatalog.linkedtree.LinkedFragment;

public interface Link {

    String uri();

    /**
     * All fragments using the same id found in a tree.
     * 
     * @return a collection of fragments in a tree
     */
    Collection<LinkedFragment> refs();

    /**
     * A fragment composed of all {@link Link#refs()}. Can return the same
     * unmodified instance in a case of single fragment reference.
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

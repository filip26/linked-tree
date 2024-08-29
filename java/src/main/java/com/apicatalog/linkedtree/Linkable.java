package com.apicatalog.linkedtree;

/**
 * Allows to hide {@link LinkedNode} and inherited interfaces. Only
 * {@link Linkable#ld()} method is exposed.
 */
public interface Linkable {

    default LinkedNode ld() {
        return (LinkedNode) this;
    }

}

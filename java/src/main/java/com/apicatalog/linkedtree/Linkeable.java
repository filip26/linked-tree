package com.apicatalog.linkedtree;

/**
 * Allows to hide {@link LinkedNode} and inherited interfaces. Only
 * {@link Linkeable#ld()} method is exposed.
 */
public interface Linkeable {

    default LinkedNode ld() {
        return (LinkedNode) this;
    }

}

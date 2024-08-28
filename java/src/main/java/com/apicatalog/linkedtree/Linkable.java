package com.apicatalog.linkedtree;

/**
 * Allows to hide {@link LinkedNode} and inherited interfaces. Only
 * {@link Linkable#linkedNode()} method is exposed.
 */
public interface Linkable {

    default LinkedNode linkedNode() {
        return (LinkedNode) this;
    }

}

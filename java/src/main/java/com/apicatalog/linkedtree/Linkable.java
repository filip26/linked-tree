package com.apicatalog.linkedtree;

/**
 * Allows to hide {@link LinkedNode} and inherited interfaces. Only
 * {@link Linkable#ld()} method is exposed.
 */
public interface Linkable {

    LinkedNode ld();

}

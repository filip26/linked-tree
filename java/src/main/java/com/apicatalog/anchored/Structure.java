package com.apicatalog.anchored;

import java.util.Collection;
import java.util.Set;

/**
 * Represents the description of a resource in an RDF graph.
 * <p>
 * A Description corresponds to the set of RDF statements where this resource is
 * the subject. Conceptually, it can be seen as a mapping from predicates to one
 * or more objects (nodes). It extends {@link Node} so that a Description can
 * itself be treated as a node in a tree or graph structure.
 * </p>
 */
public interface Structure extends Node {

    /**
     * Returns the set of predicates used in this resource description.
     * <p>
     * Each predicate is represented as a String (typically a URI). There are no
     * duplicates: each predicate appears at most once.
     * </p>
     *
     * @return an unmodifiable set of predicate URIs present in this description
     */
    Set<String> predicates();

    /**
     * Returns all objects associated with the given predicate.
     * <p>
     * In RDF, a predicate may map to multiple objects. Each object is a
     * {@link Node}.
     * </p>
     *
     * @param predicate the predicate URI whose objects are requested
     * @return a collection of objects associated with the predicate, empty if none
     * @throws IllegalArgumentException if the predicate is null or invalid
     */
    Collection<Node> objects(String predicate);
}

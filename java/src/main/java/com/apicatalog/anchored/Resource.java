package com.apicatalog.anchored;

import java.util.Collection;
import java.util.Set;

public interface Resource extends Node {

    String id();

    Collection<String> type();
    
    /**
     * Returns the set of predicates used in this resource
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

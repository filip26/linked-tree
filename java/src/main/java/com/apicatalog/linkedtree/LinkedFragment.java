package com.apicatalog.linkedtree;

import java.util.Collection;

import com.apicatalog.linkedtree.link.Link;

public non-sealed interface LinkedFragment extends LinkedNode {

    /**
     * An optional unique fragment identifier. The same identifier can be shared
     * among many fragments allowing composition.
     */
    Link id();

    Collection<String> type();

    Collection<String> terms();

    LinkedContainer property(String term);

    @Override
    default boolean isFragment() {
        return true;
    }

    @Override
    default LinkedFragment asFragment() {
        return this;
    }

    @SuppressWarnings("unchecked")
    default <T> T cast(Class<T> clazz) {
        return (T) this;
    }
}

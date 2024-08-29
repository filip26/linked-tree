package com.apicatalog.linkedtree;

import java.util.Collection;

import com.apicatalog.linkedtree.link.Link;

public interface LinkedFragment extends LinkedNode {

    /**
     * An optional unique fragment link if an identifier is present. The same
     * {@link Link} can be shared among many fragments allowing composition.
     */
    Link link();

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

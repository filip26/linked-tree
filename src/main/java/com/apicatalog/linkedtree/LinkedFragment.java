package com.apicatalog.linkedtree;

import java.util.Collection;
import java.util.Collections;

import com.apicatalog.linkedtree.link.Link;

public non-sealed interface LinkedFragment extends LinkedNode {

    /**
     * An optional unique fragment identifier. The same identifier can be shared
     * among many fragments allowing composition.
     */
    default Link id() {
        return null;
    }

    default Collection<String> type() {
        return Collections.emptySet();
    }

    default Collection<String> terms() {
        return Collections.emptySet();
    }

    default LinkedContainer property(String term) {
        return LinkedContainer.EMPTY;
    }

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
        return (T)this;
    }
}

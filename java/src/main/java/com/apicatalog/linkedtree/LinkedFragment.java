package com.apicatalog.linkedtree;

import java.util.Collection;

public interface LinkedFragment extends LinkedNode {

    /**
     * An optional unique fragment link if an identifier is present. The same
     * {@link Link} can be shared among many fragments enabling composition.
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
        if (id() != null && id().target() != null) {
            return (T)id().target();
        }
        return (T) this;
    }

    default Linkable cast() {
        if (id() != null && id().target() != null) {
            return id().target();
        }
        return this;
    }
}

package com.apicatalog.linkedtree;

import java.util.Collection;

public non-sealed interface LinkedFragment extends LinkedData {

    /**
     * An optional unique fragment identifier. The same identifier can be shared
     * among many fragments allowing composition.
     */
    Link id();

    Collection<String> type();
    
    Collection<String> terms();

    Collection<LinkedData> values(String term);
    
    @Override
    default boolean isFragment() {
        return true;
    }

    @Override
    default LinkedFragment asFragment() {
        return this;
    }

}

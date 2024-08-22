package com.apicatalog.linkedtree;

import java.util.Collection;
import java.util.Collections;

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

    default LinkedContainer values(String term) {
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

}

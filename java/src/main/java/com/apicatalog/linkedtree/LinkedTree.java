package com.apicatalog.linkedtree;

import java.util.Collection;
import java.util.Collections;

import com.apicatalog.linkedtree.link.Link;

public non-sealed interface LinkedTree extends LinkedFragment, LinkedContainer, LinkedNode {

    static LinkedTree EMPTY = new LinkedTree() {

        @Override
        public Collection<Link> links() {
            return Collections.emptyList();
        }

        @Override
        public Collection<String> type() {
            return Collections.emptySet();
        }

        @Override
        public Collection<String> terms() {
            return Collections.emptySet();
        }

        @Override
        public LinkedContainer property(String term) {
            return null;
        }

        @Override
        public Link id() {
            return null;
        }
    };

    @Override
    default boolean isTree() {
        return true;
    }

    @Override
    default LinkedTree asTree() {
        return this;
    }

    /**
     * Identifiable fragments index
     * 
     * @return a collection of links found in the tree
     */
    Collection<Link> links();

    // TODO Collection<LinkedTree> subtree()
}

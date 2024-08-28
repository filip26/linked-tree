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

        @Override
        public Collection<LinkedTree> subtrees() {
            // TODO Auto-generated method stub
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
     * @return a list of all links found in the tree, never <code>null</code>
     */
    Collection<Link> links();

    /**
     * A set of all subtrees found
     * 
     * @return a set of all subtrees, never <code>null</code>
     */
    Collection<LinkedTree> subtrees();
}

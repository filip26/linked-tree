package com.apicatalog.linkedtree;

import java.util.Collection;
import java.util.Collections;

import com.apicatalog.linkedtree.link.Link;

public non-sealed interface LinkedTree extends LinkedFragment, LinkedNode {

    static LinkedTree EMPTY = new LinkedTree() {

        @Override
        public Collection<LinkedNode> nodes() {
            return Collections.emptyList();
        }

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
            return LinkedContainer.EMPTY;
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
     * root fragments
     * 
     * @return a collection of root nodes
     */
    Collection<LinkedNode> nodes();

    /**
     * expect a single root note
     * 
     * @return a root node
     */
    default LinkedNode singleNode() {
        Collection<LinkedNode> nodes = nodes();
        if (nodes.size() != 1) {
            throw new IllegalStateException();
        }
        return nodes.iterator().next();
    }

    /**
     * Identifiable fragments index
     * 
     * @return a collection of links found in the tree
     */
    Collection<Link> links();

    // TODO predicates. i.e. terms???
}

package com.apicatalog.linkedtree;

import java.util.Collection;
import java.util.Collections;

import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.type.Type;

public interface LinkedTree extends LinkedFragment, LinkedContainer, LinkedNode {

    static LinkedTree EMPTY = new LinkedTree() {

        @Override
        public Collection<Link> links() {
            return Collections.emptyList();
        }

        @Override
        public Type type() {
            return Type.empty();
        }

        @Override
        public Collection<String> terms() {
            return Collections.emptySet();
        }

        @Override
        public LinkedContainer container(String term) {
            return null;
        }

        @Override
        public Link id() {
            return null;
        }

        @Override
        public Collection<LinkedTree> subtrees() {
            return Collections.emptyList();
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
    
    @Override
    default ContainerType containerType() {
        return ContainerType.Tree;
    }
}

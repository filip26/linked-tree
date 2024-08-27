package com.apicatalog.linkedtree;

import java.util.Collection;
import java.util.Collections;

import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public non-sealed interface LinkedContainer extends LinkedNode {

    public static LinkedContainer EMPTY = new LinkedContainer() {
    };

    public enum Type {
        OrderedList,
        UnorderedSet
    };

    default Type containerType() {
        return Type.UnorderedSet;
    }

    default Collection<LinkedNode> nodes() {
        return Collections.emptyList();
    }

    /**
     * A custom processing instructions related to the given {@link LinkedNode}.
     * 
     * @param processingOrder an order in which the node has been processed,
     *                        starting with 0
     * 
     * @return a list of custom processing instructions, never <code>null</code>
     */
    default Collection<ProcessingInstruction> pi(int processingOrder) {
        if (processingOrder < 0) {
            throw new IllegalArgumentException();
        }
        return Collections.emptyList();
    }

    @Override
    default boolean isContainer() {
        return true;
    }

    @Override
    default LinkedContainer asContainer() {
        return this;
    }

    default int size() {
        return nodes().size();
    }

    default LinkedNode single() {
        final Collection<LinkedNode> nodes = nodes();
        if (nodes.size() != 1) {
            throw new IllegalStateException();
        }
        return nodes.iterator().next();
    }

    @SuppressWarnings("unchecked")
    default <T> T single(Class<T> clazz) {
        return (T) single();
    }

    default LinkedLiteral singleLiteral() {
        return single().asLiteral();
    }

    default LinkedFragment singleFragment() {
        return single().asFragment();
    }
}

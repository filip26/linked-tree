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
     * @param node a linked to what PIs are requested
     * 
     * @return a list of custom processing instructions, never <code>null</code>
     */
    default Collection<ProcessingInstruction> pi(LinkedNode node) {
        return Collections.emptyList();
    }

    default void attach(LinkedNode node, ProcessingInstruction pi) {
        throw new UnsupportedOperationException();
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

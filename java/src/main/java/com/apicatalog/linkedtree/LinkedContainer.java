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

    @Override
    default boolean isContainer() {
        return true;
    }

    @Override
    default LinkedContainer asContainer() {
        return this;
    }

    default LinkedNode single() {
        Collection<LinkedNode> nodes = nodes();
        if (nodes.size() != 1) {
            throw new IllegalStateException();
        }
        return nodes.iterator().next();
    }

    default int size() {
        return nodes().size();
    }
    
    default LinkedLiteral singleLiteral() {
        return single().asLiteral();
    }

    @SuppressWarnings("unchecked")
    default <T> T singleLiteral(Class<T> clazz) {
        return (T)single();
    }
    
    @Override
    default ProcessingInstruction pi() {
        return null;
    }
}

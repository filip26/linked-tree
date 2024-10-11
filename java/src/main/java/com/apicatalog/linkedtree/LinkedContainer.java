package com.apicatalog.linkedtree;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public interface LinkedContainer extends LinkedNode, Iterable<LinkedNode> {

    public enum ContainerType {
        OrderedList,
        UnorderedSet,
        Tree
    };

    default ContainerType containerType() {
        return ContainerType.UnorderedSet;
    }

    default Collection<LinkedNode> nodes() {
        return Collections.emptyList();
    }

    /**
     * A custom processing instructions related to the given {@link LinkedNode}. A
     * temporary workaround, needs a revision.
     * 
     * @param processingOrder an order in which the node has been processed,
     *                        starting with 1, 0 is reserved for PIs related to the
     *                        container itself
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

    default LinkedNode node() {
        final Collection<LinkedNode> nodes = nodes();
        if (nodes.isEmpty()) {
            return null;
        }
        if (nodes.size() != 1) {
            throw new IllegalStateException();
        }
        return nodes.iterator().next();
    }

    @SuppressWarnings("unchecked")
    default <T> T materialize(Class<T> clazz) throws NodeAdapterError {

        final LinkedNode single = node();

        if (single == null) {
            return null;
        }

        if (single.isFragment()) {
            if (single.asFragment().id() != null
                    && single.asFragment().id().target() != null) {
                return single.ld().asFragment().id().target().type().materialize(clazz);
            }
            return single.asFragment().type().materialize(clazz);
        }

        return (T) single;
    }

    default LinkedLiteral literal() {
        return node().asLiteral();
    }

    default LinkedFragment fragment() {
        return node().asFragment();
    }

    default Iterator<LinkedNode> iterator() {
        return nodes().iterator();
    }

}

package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public record GenericContainer(
        ContainerType containerType,
        Collection<LinkedNode> nodes,
        LinkedTree root,
        Map<Integer, Collection<ProcessingInstruction>> ops) implements LinkedContainer {

    public GenericContainer {
        Objects.requireNonNull(containerType);
        Objects.requireNonNull(nodes);
    }

    @Override
    public Collection<ProcessingInstruction> pi(int processingOrder) {
        return ops != null
                ? ops.getOrDefault(processingOrder, Collections.emptyList())
                : Collections.emptyList();
    }

    @Override
    public int hashCode() {
        return Objects.hash(containerType, nodes, ops);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        GenericContainer other = (GenericContainer) obj;
        return containerType == other.containerType && Objects.equals(nodes, other.nodes) && Objects.equals(ops, other.ops);
    }

    @Override
    public String toString() {
        return "GenericContainer [containerType=" + containerType + ", nodes=" + nodes.size() + ", ops=" + ops.size() + "]";
    }
    
}

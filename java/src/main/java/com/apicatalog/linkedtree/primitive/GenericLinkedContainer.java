package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public record GenericLinkedContainer(
        Type containerType,
        Collection<LinkedNode> nodes,
        Map<LinkedNode, Collection<ProcessingInstruction>> ops) implements LinkedContainer {

    public GenericLinkedContainer {
        Objects.requireNonNull(containerType);
        Objects.requireNonNull(nodes);
    }

    @Override
    public Collection<ProcessingInstruction> pi(LinkedNode node) {
        return ops != null 
                ? ops.getOrDefault(node, Collections.emptyList())
                : Collections.emptyList();
    }
}

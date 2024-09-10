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
}

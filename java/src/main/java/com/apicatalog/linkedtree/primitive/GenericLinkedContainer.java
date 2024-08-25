package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Objects;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public record GenericLinkedContainer(
        Type containerType,
        Collection<LinkedNode> nodes,
        ProcessingInstruction pi
        ) implements LinkedContainer {

    public GenericLinkedContainer {
        Objects.requireNonNull(containerType);
        Objects.requireNonNull(nodes);
    }
}

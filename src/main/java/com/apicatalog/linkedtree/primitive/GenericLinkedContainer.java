package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Objects;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;

public record GenericLinkedContainer(
        Type containerType,
        Collection<LinkedNode> nodes) implements LinkedContainer {

    public GenericLinkedContainer {
        Objects.requireNonNull(containerType);
        Objects.requireNonNull(nodes);
    }
}

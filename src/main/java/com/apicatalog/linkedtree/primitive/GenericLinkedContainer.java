package com.apicatalog.linkedtree.primitive;

import java.util.Collection;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;

public class GenericLinkedContainer implements LinkedContainer {

    protected Collection<LinkedNode> nodes;
    protected String containerType;

    protected GenericLinkedContainer() {
        // protected
    }

    public static GenericLinkedContainer of(String type, Collection<LinkedNode> nodes) {
        final GenericLinkedContainer container = new GenericLinkedContainer();
        container.containerType = type;
        container.nodes = nodes;
        return container;
    }
    
    @Override
    public Collection<LinkedNode> nodes() {
        return nodes;
    }

    @Override
    public String containerType() {
        return containerType;
    }

}

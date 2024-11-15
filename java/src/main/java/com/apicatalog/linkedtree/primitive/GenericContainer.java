package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public class GenericContainer implements LinkedContainer {

    protected final ContainerType containerType;
    protected final Collection<LinkedNode> nodes;
    protected final LinkedTree root;
    protected final Map<Integer, Collection<ProcessingInstruction>> ops;

    public GenericContainer(
            ContainerType containerType,
            Collection<LinkedNode> nodes,
            LinkedTree root,
            Map<Integer, Collection<ProcessingInstruction>> ops) {

        Objects.requireNonNull(containerType);
        Objects.requireNonNull(nodes);

        this.containerType = containerType;
        this.nodes = nodes;
        this.root = root;
        this.ops = ops;
    }

    @Override
    public Collection<ProcessingInstruction> pi(int processingOrder) {
        return ops != null
                ? ops.getOrDefault(processingOrder, Collections.emptyList())
                : Collections.emptyList();
    }
    
    @Override
    public String toString() {
        return "GenericContainer [containerType=" + containerType + ", nodes=" + nodes.size() + ", ops=" + ops.size() + "]";
    }

    public static GenericContainer empty(final LinkedTree root) {
        return new GenericContainer(
                ContainerType.UnorderedSet,
                Collections.emptyList(),
                root,
                Collections.emptyMap());
    }
    

    @Override
    public ContainerType containerType() {
        return containerType;
    }

    @Override
    public Collection<LinkedNode> nodes() {
        return nodes;
    }

    public LinkedTree root() {
        return root;
    }
    
    public Map<Integer, Collection<ProcessingInstruction>> ops() {
        return ops;
    }
}

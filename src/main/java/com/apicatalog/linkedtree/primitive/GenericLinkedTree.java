package com.apicatalog.linkedtree.primitive;

import java.util.Collection;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;

public class GenericLinkedTree implements LinkedTree {

    protected Collection<LinkedNode> nodes;
    protected Type containerType;

    protected GenericLinkedTree(Collection<LinkedNode> nodes) {
        this.nodes = nodes;
    }

    public static GenericLinkedTree of(Type type, Collection<LinkedNode> nodes) {
        final GenericLinkedTree tree = new GenericLinkedTree(nodes);
        tree.containerType = type;
        return tree;
    }
    
    @Override
    public Collection<LinkedNode> nodes() {
        return nodes;
    }

    @Override
    public Collection<Link> links() {
        // TODO Auto-generated method stub
        return null;
    }
}

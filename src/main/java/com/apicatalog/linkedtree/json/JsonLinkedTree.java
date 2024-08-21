package com.apicatalog.linkedtree.json;

import java.util.Collection;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;

public class JsonLinkedTree implements LinkedTree {

    protected Collection<LinkedNode> nodes;

    public JsonLinkedTree(Collection<LinkedNode> nodes) {
        this.nodes = nodes;
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

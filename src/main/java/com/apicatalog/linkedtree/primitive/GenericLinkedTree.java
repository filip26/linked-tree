package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;

public class GenericLinkedTree implements LinkedTree {

    protected Link id;
    protected Collection<String> types;
    protected String index;
    
    protected Map<String, LinkedContainer> data;
    
    protected Collection<LinkedNode> nodes;

    protected GenericLinkedTree(Collection<LinkedNode> nodes) {
        this.nodes = nodes;
    }

    public static GenericLinkedTree of(Collection<LinkedNode> nodes) {
        return of(null, Collections.emptySet(), Collections.emptyMap(), nodes);
    }
    
    public static GenericLinkedTree of(Link id, Collection<String> type, Map<String, LinkedContainer> data, Collection<LinkedNode> nodes) {
        final GenericLinkedTree tree = new GenericLinkedTree(nodes);
        tree.id = id;
        tree.types = type;
        tree.data = data;
        tree.nodes = nodes; 
        return tree;    
    }

    @Override
    public Link id() {
        return id;
    }

    @Override
    public Collection<String> type() {
        return types;
    }

    @Override
    public Collection<String> terms() {
        return data.keySet();
    }

    @Override
    public LinkedContainer values(String term) {
        return data.get(term);
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

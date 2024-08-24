package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.link.Link;

public class GenericLinkedTree implements LinkedTree {

    protected Link id;
    protected Collection<String> types;
    protected String index;
    
    protected Map<String, LinkedContainer> data;
    
    protected Collection<LinkedNode> nodes;
    
    protected Map<String, Link> links;
    
    protected Object meta;

    protected GenericLinkedTree(Collection<LinkedNode> nodes) {
        this.nodes = nodes;
    }

    public static GenericLinkedTree of(Collection<LinkedNode> nodes, Map<String, Link> links) {
        return of(null, Collections.emptySet(), Collections.emptyMap(), nodes, links, null);
    }
    
    public static GenericLinkedTree of(Link id, Collection<String> type, Map<String, LinkedContainer> data, Collection<LinkedNode> nodes, Map<String, Link> links, Object meta) {
        final GenericLinkedTree tree = new GenericLinkedTree(nodes);
        tree.id = id;
        tree.types = type;
        tree.data = data;
        tree.nodes = nodes;
        tree.links = links;
        tree.meta = meta;
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
    public LinkedContainer property(String term) {
        return data.get(term);
    }
    

    @Override
    public Collection<LinkedNode> nodes() {
        return nodes;
    }

    @Override
    public Collection<Link> links() {
        return links.values();
    }
    
    @Override
    public Object pi() {
        return meta;
    }
}

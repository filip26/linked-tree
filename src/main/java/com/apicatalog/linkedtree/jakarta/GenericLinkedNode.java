package com.apicatalog.linkedtree.jakarta;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedData;
import com.apicatalog.linkedtree.LinkedFragment;

public class GenericLinkedNode implements LinkedFragment {
    
    protected Link id;
    protected Collection<String> type; 
    
    protected Map<String, Collection<LinkedData>> data;
    
    protected GenericLinkedNode() {
        
    }
    
    public static GenericLinkedNode of(Link id, Collection<String> type, Map<String, Collection<LinkedData>> data) {
        final GenericLinkedNode node = new GenericLinkedNode();
        node.id = id;
        node.type = type;
        node.data = data;
        return node;
    }
    
    @Override
    public Link id() {
        return id;
    }

    @Override
    public Collection<String> type() {
        return type;
    }

    @Override
    public Collection<String> terms() {
        return data.keySet();
    }

    @Override
    public Collection<LinkedData> data(String term) {
        return data.get(term);
    }

}

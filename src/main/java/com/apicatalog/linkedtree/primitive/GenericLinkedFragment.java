package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedFragment;

public class GenericLinkedFragment implements LinkedFragment {
    
    protected Link id;
    protected Collection<String> types; 
    
    protected Map<String, Collection<LinkedNode>> data;
    
    protected GenericLinkedFragment() {
        
    }
    
    public static GenericLinkedFragment of(Link id, Collection<String> type, Map<String, Collection<LinkedNode>> data) {
        final GenericLinkedFragment node = new GenericLinkedFragment();
        node.id = id;
        node.types = type;
        node.data = data;
        return node;
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
    public Collection<LinkedNode> values(String term) {
        return data.get(term);
    }

}

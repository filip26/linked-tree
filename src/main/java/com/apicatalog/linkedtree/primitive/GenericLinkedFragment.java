package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;

public class GenericLinkedFragment implements LinkedFragment {
    
    protected Link id;
    protected Collection<String> types;
    
    protected Map<String, LinkedContainer> data;
    
    protected GenericLinkedFragment() {
        
    }
    
    public static GenericLinkedFragment of(Link id, Collection<String> type, Map<String, LinkedContainer> data) {
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
    public LinkedContainer values(String term) {
        return data.get(term);
    }
}

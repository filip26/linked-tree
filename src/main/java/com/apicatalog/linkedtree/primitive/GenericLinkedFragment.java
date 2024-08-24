package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.link.Link;

public class GenericLinkedFragment implements LinkedFragment {
    
    protected Link id;
    protected Collection<String> types;
    
    protected Map<String, LinkedContainer> data;
    
    protected Object meta;
    
    protected GenericLinkedFragment() {
        // protected
    }
    
    public static GenericLinkedFragment of(Link id, Collection<String> type, Map<String, LinkedContainer> data, Object meta) {
        final GenericLinkedFragment node = new GenericLinkedFragment();
        node.id = id;
        node.types = type;
        node.data = data;
        node.meta = meta;
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
    
    public Map<String, LinkedContainer> entries() {
        return data;
    }

    @Override
    public LinkedContainer property(String term) {
        return data.get(term);
    }
    
    @Override
    public Object pi() {
        return meta;
    }
    
    
}

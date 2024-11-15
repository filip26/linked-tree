package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.type.TypeAdapter;

public class FragmentGetter implements Getter {

    String term;
    TypeAdapter adapter;
    
    public FragmentGetter(String term, TypeAdapter adapter) {
        this.term = term;
        this.adapter = adapter;
    }
    
    public Object get(LinkedFragment source) throws NodeAdapterError {
    
        LinkedNode node = source.node(term);
        
        if (node == null) {
            return null;
        }
        
        if (node.asFragment().type().isAdaptableTo(adapter.typeInterface())) {
            return node.asFragment().type().materialize(adapter.typeInterface());    
        }
        
        LinkedFragment target = node.asFragment();
        
        if (target.id() != null) {
            target = target.id().target();
        }
        
        return adapter.materialize(target);
    }

}

package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.orm.adapter.NativeFragmentAdapter;

public class FragmentGetter implements Getter {

    String term;
    NativeFragmentAdapter adapter;
    
    public FragmentGetter(String term, NativeFragmentAdapter adapter) {
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
        return adapter.materialize(node.asFragment().id().target());
    }

}

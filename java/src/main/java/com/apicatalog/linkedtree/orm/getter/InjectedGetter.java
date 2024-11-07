package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapter;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public class InjectedGetter implements Getter {

    NodeAdapter<LinkedFragment, ?> adapter;
    
    public InjectedGetter(NodeAdapter<LinkedFragment, ?> adapter) {
        this.adapter = adapter;
    }
    
    public Object get(LinkedFragment source) throws NodeAdapterError {        
        return adapter.materialize(source);
    }

}

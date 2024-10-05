package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public class TypeGetter implements Getter {

    protected static final TypeGetter INSTANCE = new TypeGetter();
    
    protected TypeGetter() {
        // protected
    }
    
    public Object materialize(LinkedFragment source) throws NodeAdapterError {
        return source.type();
    }
    
    public static TypeGetter instance() {
        return INSTANCE;
    }

}

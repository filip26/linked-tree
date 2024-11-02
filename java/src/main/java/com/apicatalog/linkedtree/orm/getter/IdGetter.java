package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public class IdGetter implements Getter {

    protected static final IdGetter INSTANCE = new IdGetter();
    
    protected IdGetter() {
        // protected
    }
    
    @Override
    public Object get(LinkedFragment source) throws NodeAdapterError {
        return source.uri();
    }

    public static IdGetter instance() {
        return INSTANCE;
    }
}

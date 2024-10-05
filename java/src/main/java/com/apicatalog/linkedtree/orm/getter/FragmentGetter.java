package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.orm.adapter.NativeFragmentAdapter;

public class FragmentGetter implements Getter {

    String term;
    NativeFragmentAdapter adapter;
    
    public FragmentGetter(String term, NativeFragmentAdapter adapter) {
        this.term = term;
        this.adapter = adapter;
    }
    
    public Object materialize(LinkedFragment source) throws NodeAdapterError {
        if (source.type().isAdaptableTo(adapter.typeInterface())) {
            return source.type().materialize(adapter.typeInterface());    
        }
        return adapter.materialize(source);
    }

}

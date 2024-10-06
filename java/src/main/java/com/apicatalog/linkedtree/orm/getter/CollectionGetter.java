package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapter;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.orm.adapter.NativeFragmentAdapter;

public class CollectionGetter implements Getter {

    String term;
    Class<?> componentType;
    NativeFragmentAdapter adapter;
    
    public CollectionGetter(String term,
            Class<?> componentType
            , NativeFragmentAdapter adapter
            ) {
        this.term = term;
        this.componentType = componentType;
        this.adapter = adapter;
    }
    
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public Object get(LinkedFragment source) throws NodeAdapterError {
        return source.collection(term, componentType, (NodeAdapter)adapter);        
    }

}

package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.orm.adapter.NativeLiteralAdapter;

public class LiteralGetter implements Getter {

    String term;
    Class<?> returnType;
    NativeLiteralAdapter adapter;
    
    public LiteralGetter(
            String term,
            Class<?> returnType,
            NativeLiteralAdapter adapter) {
        this.term = term;
        this.returnType = returnType;
        this.adapter = adapter;
        
    }
    
    public Object get(LinkedFragment source) throws NodeAdapterError {
        
        
        LinkedLiteral x = source.literal(term, LinkedLiteral.class);
        
    
        
        return adapter.materialize(returnType, x);
        //;
    }

}

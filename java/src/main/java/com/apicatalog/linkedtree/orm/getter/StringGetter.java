package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public class StringGetter implements Getter {

    String term;
    
    public StringGetter(String term) {
        this.term = term;
    }
    
    public Object get(LinkedFragment source) throws NodeAdapterError {
        LinkedNode node = source.node(term);

        if (node == null) {
            return null;
        }
        
        if (node instanceof LinkedLiteral literal) {
            return literal.lexicalValue();
        }
        throw new IllegalStateException();
    }

}

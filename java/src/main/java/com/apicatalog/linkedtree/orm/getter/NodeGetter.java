package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public class NodeGetter implements Getter {

    String term;
    Class<? extends LinkedNode> type;

    public NodeGetter(String term, Class<? extends LinkedNode> type) {
        this.term = term;
        this.type = type;
    }

    public Object get(LinkedFragment source) throws NodeAdapterError {

        if (type.isAssignableFrom(LinkedFragment.class)) {
            return source.fragment(term);
        }
        if (type.isAssignableFrom(LinkedLiteral.class)) {
            return source.literal(term, LinkedLiteral.class);
        }
        if (type.isAssignableFrom(LinkedContainer.class)) {
            return source.container(term);
        }

        return source.node(term);
    }

}

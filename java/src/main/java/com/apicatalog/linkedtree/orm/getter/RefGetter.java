package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public record RefGetter(String term) implements Getter {

    public Object get(LinkedFragment source) throws NodeAdapterError {
        return source.uri(term);
    }

}

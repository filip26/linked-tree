package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public record LangMapGetter(String term) implements Getter {

    public Object get(LinkedFragment source) throws NodeAdapterError {
        return source.languageMap(term);
    }

}

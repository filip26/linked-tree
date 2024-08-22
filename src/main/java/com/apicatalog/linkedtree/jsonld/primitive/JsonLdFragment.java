package com.apicatalog.linkedtree.jsonld.primitive;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.primitive.GenericLinkedFragment;

public class JsonLdFragment extends GenericLinkedFragment {

    protected String index;

    protected JsonLdFragment() {
        // protected
    }

    public static JsonLdFragment of(Link id, Collection<String> type, Map<String, LinkedContainer> data, String index) {
        final JsonLdFragment node = new JsonLdFragment();
        node.id = id;
        node.types = type;
        node.data = data;
        node.index = index;
        return node;
    }

    public String index() {
        return index;
    }

}

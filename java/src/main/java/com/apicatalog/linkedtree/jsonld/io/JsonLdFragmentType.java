package com.apicatalog.linkedtree.jsonld.io;

import java.util.Collection;

public class JsonLdFragmentType {

    Collection<String> context;
    Collection<JsonLdProperty> methods;
    
    public JsonLdFragmentType(Collection<String> context, Collection<JsonLdProperty> methods) {
        this.context = context;
        this.methods = methods;
    }

    public Collection<String> context() {
        return context;
    }

    public Collection<JsonLdProperty> methods() {
        return methods;
    }

}

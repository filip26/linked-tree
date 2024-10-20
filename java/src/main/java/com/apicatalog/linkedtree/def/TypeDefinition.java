package com.apicatalog.linkedtree.def;

import java.util.Collection;

public class TypeDefinition {

    String name;
    
    String vocab;
    
    Collection<String> context;
    
    PropertyDefinition id;
    
    Collection<PropertyDefinition> methods;
    
    public TypeDefinition(
            Collection<String> context,
            PropertyDefinition id,
            Collection<PropertyDefinition> methods
            
            ) {
        this.context = context;
        this.id = id;
        this.methods = methods;
    }

    public Collection<String> context() {
        return context;
    }

    public Collection<PropertyDefinition> methods() {
        return methods;
    }

    public PropertyDefinition id() {
        return id;
    }
    
    public String name() {
        return name;
    }
    
    public String vocab() {
        return vocab;
    }
}

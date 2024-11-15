package com.apicatalog.linkedtree.def;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.orm.mapper.ObjectWriter;

public class TypeDefinition {

    Collection<String> types;
    String vocab;
    
    Collection<String> context;
    
    PropertyDefinition id;
    PropertyDefinition type;
    
    Collection<PropertyDefinition> methods;
    
    Map<Class<?>, ObjectWriter<?>> writers;
    
    public TypeDefinition(
            String vocab,
            Collection<String> types,
            Collection<String> context,
            PropertyDefinition id,
            PropertyDefinition type,
            Collection<PropertyDefinition> methods,
            Map<Class<?>, ObjectWriter<?>> writers        
            ) {
        this.vocab = vocab;
        this.context = context;
        this.types = types;
        this.id = id;
        this.type = type;
        this.methods = methods;
        this.writers = writers;
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
    
    public PropertyDefinition type() {
        return type;
    }
    
    public String name() {
        return types != null && types.size() > 0 ? types.iterator().next() : null;
    }
    
    public String vocab() {
        return vocab;
    }

    public ObjectWriter<?> objectWriter(Class<?> clazz) {
        return writers.get(clazz);
    }
    
    public Collection<String> types() {
        return types;
    }
}

package com.apicatalog.linkedtree.def;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.literal.adapter.DataTypeNormalizer;

public class TypeDefinition {

    String name;
    
    String vocab;
    
    Collection<String> context;
    
    PropertyDefinition id;
    PropertyDefinition type;
    
    Collection<PropertyDefinition> methods;
    
    Map<Class<?>, DataTypeNormalizer<?>> normalizers;
    
    public TypeDefinition(
            String name,
            Collection<String> context,
            PropertyDefinition id,
            PropertyDefinition type,
            Collection<PropertyDefinition> methods,
            Map<Class<?>, DataTypeNormalizer<?>> normalizers        
            ) {
        this.name = name;
        this.context = context;
        this.id = id;
        this.type = type;
        this.methods = methods;
        this.normalizers = normalizers;
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
        return name;
    }
    
    public String vocab() {
        return vocab;
    }

    public DataTypeNormalizer<?> normalizer(Class<?> clazz) {
        return normalizers.get(clazz);
    }
}

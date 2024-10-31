package com.apicatalog.linkedtree.def;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.literal.adapter.DataTypeNormalizer;

public class TypeDefinition {

    Collection<String> types;
    String vocab;
    
    Collection<String> context;
    
    PropertyDefinition id;
    PropertyDefinition type;
    
    Collection<PropertyDefinition> methods;
    
    Map<Class<?>, DataTypeNormalizer<?>> normalizers;
    
    public TypeDefinition(
            Collection<String> types,
            Collection<String> context,
            PropertyDefinition id,
            PropertyDefinition type,
            Collection<PropertyDefinition> methods,
            Map<Class<?>, DataTypeNormalizer<?>> normalizers        
            ) {
        this.context = context;
        this.types = types;
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
        return types != null && types.size() > 0 ? types.iterator().next() : null;
    }
    
    public String vocab() {
        return vocab;
    }

    public DataTypeNormalizer<?> normalizer(Class<?> clazz) {
        return normalizers.get(clazz);
    }
    
    public Collection<String> types() {
        return types;
    }
}

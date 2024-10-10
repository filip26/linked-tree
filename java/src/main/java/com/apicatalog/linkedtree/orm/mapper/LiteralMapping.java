package com.apicatalog.linkedtree.orm.mapper;

import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedLiteral;

public class LiteralMapping {

    final Map<MappingKey, LiteralMapper<?, ?>> mapping;

    public LiteralMapping() {
        this.mapping = new LinkedHashMap<>();
    }

    public <T extends LinkedLiteral, R>  LiteralMapping add(
            Class<T> typeInterface, 
            Class<R> targetType,
            LiteralMapper<T, R> mapper) {
        mapping.put(new MappingKey(typeInterface, targetType), mapper);
        
        return this;
    }
    
    public LiteralMapper find(Class<? extends LinkedLiteral> typeInterface, Class<?> returnType) {
        // TODO Auto-generated method stub
        return null;
    }

    static class MappingKey {
        Class<? extends LinkedLiteral> literalType;
        Class<?> targetType;

        public MappingKey(Class<? extends LinkedLiteral> literalType, Class<?> targetType) {
            this.literalType = literalType;
            this.targetType = targetType;
        }
    }

}

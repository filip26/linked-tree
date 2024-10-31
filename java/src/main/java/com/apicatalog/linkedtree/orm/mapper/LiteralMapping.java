package com.apicatalog.linkedtree.orm.mapper;

import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedLiteral;

public class LiteralMapping {

    final Map<MappingKey, LiteralMapper<? extends LinkedLiteral, ?>> mapping;

    public LiteralMapping() {
        this.mapping = new LinkedHashMap<>();
    }

    public <T extends LinkedLiteral, R> LiteralMapping add(
            Class<T> typeInterface,
            Class<R> targetType,
            LiteralMapper<T, R> mapper) {
        mapping.put(new MappingKey(typeInterface, targetType), mapper);

        return this;
    }

    @SuppressWarnings("unchecked")
    public <T extends LinkedLiteral, R> LiteralMapper<LinkedLiteral, ?> find(Class<T> typeInterface, Class<R> targetType) {
        
        if (targetType.isAssignableFrom(typeInterface)) {
            return literal -> literal;
        }
        
        return (LiteralMapper<LinkedLiteral, ?>) mapping.entrySet().stream()
                .filter(e -> e.getKey().match(typeInterface, targetType))
                .findFirst()
                .map(Map.Entry::getValue)
                .orElse(null);

    }

    static class MappingKey {

        Class<? extends LinkedLiteral> literal;
        Class<?> target;

        public MappingKey(Class<? extends LinkedLiteral> literalType, Class<?> targetType) {
            this.literal = literalType;
            this.target = targetType;
        }

        public boolean match(Class<? extends LinkedLiteral> typeInterface, Class<?> targetType) {
            return literal.isAssignableFrom(typeInterface) && targetType.isAssignableFrom(target);
        }
    }

}

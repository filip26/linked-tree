package com.apicatalog.linkedtree.orm.mapper;

import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedLiteral;

class ObjectReaderProvider {

    final Map<MappingKey, ObjectReader<?, ?>> mapping;

    public ObjectReaderProvider() {
        this.mapping = new LinkedHashMap<>();
    }

    public <T, R> ObjectReaderProvider add(
            Class<T> sourceType,
            Class<R> targetType,
            ObjectReader<T, R> mapper) {
        mapping.put(new MappingKey(sourceType, targetType), mapper);

        return this;
    }

    @SuppressWarnings("unchecked")
    public <T extends LinkedLiteral, R> ObjectReader<LinkedLiteral, ?> find(Class<T> sourceType, Class<R> targetType) {
        
        if (targetType.isAssignableFrom(sourceType)) {
            return targetType::cast;
        }
        
        return (ObjectReader<LinkedLiteral, ?>) mapping.entrySet().stream()
                .filter(e -> e.getKey().match(sourceType, targetType))
                .findFirst()
                .map(Map.Entry::getValue)
                .orElse(null);

    }

    static class MappingKey {

        Class<?> literal;
        Class<?> target;

        public MappingKey(Class<?> literalType, Class<?> targetType) {
            this.literal = literalType;
            this.target = targetType;
        }

        public boolean match(Class<?> typeInterface, Class<?> targetType) {
            return literal.isAssignableFrom(typeInterface) && targetType.isAssignableFrom(target);
        }
    }

}

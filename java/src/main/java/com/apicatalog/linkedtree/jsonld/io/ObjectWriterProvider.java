package com.apicatalog.linkedtree.jsonld.io;

import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.orm.mapper.ObjectWriter;

class ObjectWriterProvider {

    final Map<Class<?>, ObjectWriter<?>> mapping;

    public ObjectWriterProvider() {
        this.mapping = new LinkedHashMap<>();
    }

    public <T, R extends LinkedLiteral> ObjectWriterProvider add(
            Class<T> sourceType,
            Class<R> targetType,
            ObjectWriter<T> mapper) {
        mapping.put(sourceType, mapper);
        return this;
    }

    public <T, R extends LinkedLiteral> ObjectWriter<?> find(Class<T> sourceType) {

        if (LinkedLiteral.class.isAssignableFrom(sourceType)) {
            return LinkedLiteral.class::cast;
        }

        return (ObjectWriter<?>) mapping.get(sourceType);
    }

    static class MappingKey {

        Class<?> source;
        Class<? extends LinkedLiteral> target;

        public MappingKey(Class<?> sourceType, Class<? extends LinkedLiteral> targetType) {
            this.target = targetType;
            this.source = sourceType;
        }

        public boolean match(Class<?> sourceType,  Class<? extends LinkedLiteral> targetType) {
            return source.isAssignableFrom(sourceType) && targetType.isAssignableFrom(target);
        }
    }

}

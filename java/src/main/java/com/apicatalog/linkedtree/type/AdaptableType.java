package com.apicatalog.linkedtree.type;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public class AdaptableType implements Type {

    protected final Map<String, TypeAdapter> type;
    protected LinkedFragment fragment;

    protected AdaptableType(Map<String, TypeAdapter> type) {
        this.type = type;
        this.fragment = null;
    }

    public static AdaptableType of(
            Collection<String> type) {
        return new AdaptableType(
                type.stream()
                        .collect(
                                HashMap::new,
                                (c, t) -> c.put(t, null), HashMap::putAll));
    }

    public void node(LinkedFragment fragment) {
        this.fragment = fragment;
    }

    @Override
    public Iterator<String> iterator() {
        return type.keySet().iterator();
    }

    @Override
    public boolean contains(String name) {
        return type.containsKey(name);
    }

    @Override
    public boolean isEmpty() {
        return type.isEmpty();
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T materialize(Class<T> clazz) throws NodeAdapterError {
        return (T) type.values()
                .stream()
                .filter(Objects::nonNull)
                .filter(t -> clazz.isAssignableFrom(t.typeInterface()))
                .findFirst()
                .orElseThrow(NodeAdapterError::new)
                .materialize(fragment);
    }

    @Override
    public boolean isAdaptableTo(Class<?> clazz) {
        return type.values()
                .stream()
                .filter(Objects::nonNull)
                .map(TypeAdapter::typeInterface)
                .anyMatch(clazz::isAssignableFrom);
    }

    public TypeAdapter adapter(String name) {
        return type.get(name);
    }

    public void adapter(String name, TypeAdapter adapter) {
        type.put(name, adapter);
    }

    static Type EMPTY = new Type() {

        @Override
        public Iterator<String> iterator() {
            return Collections.emptyIterator();
        }

        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public boolean contains(String type) {
            return false;
        }

        @Override
        public <T> T materialize(Class<T> clazz) throws ClassCastException {
            throw new ClassCastException("A type set is empty. It cannot be cast to an unknown interface/class " + clazz);
        }
    };
}

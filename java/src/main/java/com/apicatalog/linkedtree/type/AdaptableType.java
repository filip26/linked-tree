package com.apicatalog.linkedtree.type;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.linkedtree.LinkedNode;

public class AdaptableType implements Type {

    protected final Map<String, TypeAdapter> type;
    protected LinkedNode node;

    protected AdaptableType(Map<String, TypeAdapter> type) {
        this.type = type;
        this.node = null;
    }

    public static AdaptableType of(
            Collection<String> type) {
        return new AdaptableType(
                type.stream()
                        .collect(
                                HashMap::new,
                                (c, t) -> c.put(t, null), HashMap::putAll));
    }

    public void node(LinkedNode node) {
        this.node = node;
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

    @Override
    public <T> T adapt(Class<T> clazz) throws TypeAdapterError, ClassCastException {
        return type.values()
                .stream()
                .filter(Objects::nonNull)
                .filter(t -> t.typeInterface().isAssignableFrom(clazz))
                .findFirst()
                .orElseThrow(ClassCastException::new)
                .adapt(node);
    }

    @Override
    public boolean isAdaptableTo(Class<?> clazz) {
        return type.values()
                .stream()
                .filter(Objects::nonNull)
                .anyMatch(t -> t.typeInterface().isAssignableFrom(clazz));
    }

    public TypeAdapter getAdapter(String name) {
        return type.get(name);
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
        public <T> T adapt(Class<T> clazz) throws ClassCastException {
            throw new ClassCastException("A type set is empty. It cannot be cast to an unknown interface/class " + clazz);
        }
    };
}

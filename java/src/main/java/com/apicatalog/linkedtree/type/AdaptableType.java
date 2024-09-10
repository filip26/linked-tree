package com.apicatalog.linkedtree.type;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

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
                        .collect(Collectors.toMap(
                                Function.identity(),
                                null)));
    }

    public void node(LinkedNode node) {
        this.node = node;
    }
//    public static GenericTypeSet of(
//            LinkedNode node,
//            GenericTypeSet typeSet) {
//        return new GenericTypeSet(node, typeSet);
//    }

    @Override
    public Iterator<String> iterator() {
        return type.keySet().iterator();
//        return types.stream().map(Type::uri).iterator();
    }

    @Override
    public boolean contains(String name) {
        return type.containsKey(name);
//        return types.stream().map(Type::uri).anyMatch(type::equals);
    }

//    @Override
//    public Type get(String type) {
////        return types.stream().filter(t -> type.equals(t.uri())).findFirst().orElse(null);
//    }

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
}

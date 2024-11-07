package com.apicatalog.linkedtree.orm.getter;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URI;
import java.util.Collection;
import java.util.Objects;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapter;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.type.FragmentType;

public record TypeGetter(
        Class<?> typeInterface,
        NodeAdapter<LinkedFragment, ?> adapter) implements Getter {

    public TypeGetter {
        Objects.requireNonNull(typeInterface);
    }

    @Override
    public Object get(LinkedFragment source) throws NodeAdapterError {
        return adapter.materialize(source);
    }

    public static Getter instance(Method method) {
        return instance(method.getReturnType(), method.getGenericReturnType());
    }

    static Getter instance(Class<?> typeInterface, Type type) {

        NodeAdapter<LinkedFragment, ?> adapter = null;

        if (typeInterface.isAssignableFrom(FragmentType.class)) {
            adapter = source -> source.type();

        } else if (typeInterface.isAssignableFrom(URI.class)) {
            adapter = source -> source.type().isEmpty()
                    ? null
                    : URI.create(source.type().iterator().next());

        } else if (typeInterface.isAssignableFrom(String.class)) {

            adapter = source -> source.type().isEmpty()
                    ? null
                    : source.type().iterator().next();

        } else if (Collection.class.isAssignableFrom(typeInterface) && type != null) {
            Class<?> componentClass = (Class<?>) ((ParameterizedType) type).getActualTypeArguments()[0];

            if (componentClass.isAssignableFrom(URI.class)) {
                adapter = source -> source.type().stream()
                        .map(URI::create).toList();

            } else if (componentClass.isAssignableFrom(String.class)) {
                adapter = source -> source.type().stream().toList();
            }
        }

        if (adapter == null) {
            throw new IllegalArgumentException("Return type " + typeInterface + " is not supported. Try URI, String or FragmentType ");
        }

        return new TypeGetter(typeInterface, adapter);
    }
}

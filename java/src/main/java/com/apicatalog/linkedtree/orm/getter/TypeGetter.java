package com.apicatalog.linkedtree.orm.getter;

import java.net.URI;
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

    public static TypeGetter instance(Class<?> typeInterface) {

        final NodeAdapter<LinkedFragment, ?> adapter;

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

        } else {
            throw new IllegalArgumentException("Return type " + typeInterface + " is not supported. Try URI, String or FragmentType ");
        }

        return new TypeGetter(typeInterface, adapter);
    }
}

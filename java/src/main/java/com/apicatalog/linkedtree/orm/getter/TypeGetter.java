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
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
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

    @SuppressWarnings({ "unchecked", "rawtypes" })
    static Getter instance(Class<?> typeInterface, Type type) {

        if (typeInterface.isAssignableFrom(Collection.class) && type != null) {
            Class<?> componentClass = (Class<?>) ((ParameterizedType) type).getActualTypeArguments()[0];

            return CollectionGetter.of(JsonLdKeyword.TYPE, typeInterface, componentClass,
                    (NodeAdapter)adapter(componentClass));
        }

        return new TypeGetter(typeInterface, adapter(typeInterface));
    }

    static NodeAdapter<LinkedFragment, ?> adapter(Class<?> typeInterface) {

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

        return adapter;
    }
}

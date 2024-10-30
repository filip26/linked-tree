package com.apicatalog.linkedtree.def;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.util.Collection;

import com.apicatalog.linkedtree.literal.adapter.DataTypeNormalizer;
import com.apicatalog.linkedtree.orm.Fragment;

public class PropertyDefinition {

    protected String vocab;
    protected String name;
    protected Method method;
    protected boolean targetFragment;
    protected DataTypeNormalizer<?> normalizer;

    PropertyDefinition() {
    }

    public static PropertyDefinition of(String name, String vocab, Method method, DataTypeNormalizer<?> normalizer) {

        var def = new PropertyDefinition();
        def.name = name;
        def.method = method;
        def.vocab = vocab;
        def.normalizer = normalizer;

        Class<?> type = null;

        if (Collection.class.isAssignableFrom(method.getReturnType())) {
            type = (Class<?>) ((ParameterizedType) method.getGenericReturnType()).getActualTypeArguments()[0];
        }

        if (type == null) {
            type = method.getReturnType();
        }

        def.targetFragment = type.isAnnotationPresent(Fragment.class);

        return def;
    }

    public Object invoke(Object object) {
        try {
            return method.invoke(object);
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            throw new IllegalStateException(e);
        }
    }

    public String name() {
        return name;
    }

    public String vocab() {
        return vocab;
    }

    public boolean isTargetFragment() {
        return targetFragment;
    }

    public DataTypeNormalizer<?> normalizer() {
        return normalizer;
    }
}

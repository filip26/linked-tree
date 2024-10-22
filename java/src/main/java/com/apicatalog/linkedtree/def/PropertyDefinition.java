package com.apicatalog.linkedtree.def;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import com.apicatalog.linkedtree.literal.adapter.DataTypeNormalizer;

public class PropertyDefinition {

    protected String vocab;
    protected String name;
    protected Method method;
    protected boolean targetFragment;
    protected DataTypeNormalizer<?> normalizer;

    public PropertyDefinition(String name, String vocab, Method method, boolean targetFragment, DataTypeNormalizer<?> normalizer) {
        this.name = name;
        this.method = method;
        this.vocab = vocab;
        this.targetFragment = targetFragment;
        this.normalizer = normalizer;
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

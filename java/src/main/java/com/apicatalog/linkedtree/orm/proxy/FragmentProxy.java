package com.apicatalog.linkedtree.orm.proxy;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Map;

import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.orm.getter.Getter;
import com.apicatalog.linkedtree.type.TypeAdapter;

/**
 * Creates a new instance of a {@link FragmentProxy#typeInterface}.
 */
public class FragmentProxy implements TypeAdapter {

    static final Method LD_METHOD = Linkable.method();

    final String typeName;
    final Class<?> typeInterface;
    final Map<Method, Getter> getters;

    public FragmentProxy(Class<?> typeInterface, String typeName, Map<Method, Getter> getters) {
        this.typeInterface = typeInterface;
        this.typeName = typeName;
        this.getters = getters;
    }

    @Override
    public Object materialize(final LinkedFragment source) throws NodeAdapterError {
        return Proxy.newProxyInstance(
                typeInterface.getClassLoader(),
                new Class<?>[] { typeInterface, Linkable.class },
                new FragmentProxyInvocation(FragmentProxy.this, source));
    }

    @Override
    public Class<?> typeInterface() {
        return typeInterface;
    }

    public String typeName() {
        return typeName;
    }
}

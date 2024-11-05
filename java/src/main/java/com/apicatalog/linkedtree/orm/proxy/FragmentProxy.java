package com.apicatalog.linkedtree.orm.proxy;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Collection;
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

    protected final String typeName;
    protected final Class<?> typeInterface;
    protected final Map<Method, Getter> getters;

    protected final Class<?>[] interfaces;
    protected final boolean eager;

    protected FragmentProxy(Class<?> typeInterface, String typeName, Map<Method, Getter> getters, Class<?>[] interfaces, boolean eager) {
        this.typeInterface = typeInterface;
        this.typeName = typeName;
        this.getters = getters;
        this.interfaces = interfaces;
        this.eager = eager;
    }
    
    public static FragmentProxy of(Class<?> typeInterface, String typeName, Map<Method, Getter> getters, boolean mutable, boolean eager, boolean linkable) {
        
        Collection<Class<?>> interfaces = new ArrayList<>(3);
        interfaces.add(typeInterface);
        
        if (mutable) {
            interfaces.add(PropertyValueConsumer.class);
        }
        if (linkable) {
            interfaces.add(Linkable.class);
        }
        
        return new FragmentProxy(typeInterface, typeName, getters, interfaces.toArray(Class[]::new), eager);
    }

    @Override
    public Object materialize(final LinkedFragment source) throws NodeAdapterError {
        return Proxy.newProxyInstance(
                typeInterface.getClassLoader(),
                interfaces,
                new FragmentProxyInvocation(FragmentProxy.this, source));
    }

    @Override
    public Class<?> typeInterface() {
        return typeInterface;
    }

    @Override
    public String type() {
        return typeName;
    }
}

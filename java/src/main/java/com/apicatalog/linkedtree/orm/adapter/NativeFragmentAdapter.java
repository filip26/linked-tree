package com.apicatalog.linkedtree.orm.adapter;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Map;

import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.orm.getter.Getter;
import com.apicatalog.linkedtree.type.TypeAdapter;

public class NativeFragmentAdapter implements TypeAdapter {

    static final Method LD_METHOD = Linkable.method(); 
    
    final Class<?> typeInterface;
    final Map<Method, Getter> getters;

    public NativeFragmentAdapter(Class<?> type, String typeName, Map<Method, Getter> getters) {
        this.typeInterface = type;
        this.getters = getters;

    }

    @Override
    public Object materialize(final LinkedFragment source) throws NodeAdapterError {
        return Proxy.newProxyInstance(typeInterface.getClassLoader(),
                new Class<?>[] { typeInterface, Linkable.class }, new InvocationHandler() {

                    @Override
                    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {

                        if (LD_METHOD.equals(method)) {
                            return source;
                        }
                        
                        final Getter getter = getters.get(method);

                        if (getter != null) {
                            return getters.get(method).materialize(source);
                        }

                        throw new UnsupportedOperationException();
                    }
                });
    }

    @Override
    public Class<?> typeInterface() {
        return typeInterface;
    }

}

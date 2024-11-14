package com.apicatalog.linkedtree.builder;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodHandles.Lookup;
import java.lang.invoke.MethodType;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public class FragmentComposer {

    protected final Map<String, Object> values;

    protected final float javaVersion;

    protected FragmentComposer() {
        this.values = new HashMap<>();
        this.javaVersion = Float.parseFloat(System.getProperty("java.class.version"));
    }

    public static FragmentComposer create() {
        return new FragmentComposer();
    }

    public FragmentComposer set(String term, Object value) {
        values.put(term, value);
        return this;
    }

    @SuppressWarnings("unchecked")
    public <T> T get(Class<T> type) throws NodeAdapterError {
        return (T) Proxy.newProxyInstance(
                type.getClassLoader(),
                new Class<?>[] { type },
                new InvocationHandler() {

                    @Override
                    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {

                        if (method.isDefault()) {
                            if (javaVersion <= 52) {
                                final Constructor<Lookup> constructor = Lookup.class.getDeclaredConstructor(Class.class);
                                constructor.setAccessible(true);

                                final Class<?> clazz = method.getDeclaringClass();

                                return constructor.newInstance(clazz)
                                        .in(clazz)
                                        .unreflectSpecial(method, clazz)
                                        .bindTo(proxy)
                                        .invokeWithArguments(args);

                            }
                            return MethodHandles.lookup()
                                    .findSpecial(
                                            method.getDeclaringClass(),
                                            method.getName(),
                                            MethodType.methodType(method.getReturnType(), method.getParameterTypes()),
                                            method.getDeclaringClass())
                                    .bindTo(proxy)
                                    .invokeWithArguments(args);
                        }

                        if (values.containsKey(method.getName())) {
                            return values.get(method.getName());
                        }

                        if (Object.class.equals(method.getDeclaringClass())) {
                            return method.invoke(proxy, args);
                        }

                        if (method.getParameterCount() == 0 &&
                                Object.class.isAssignableFrom(method.getReturnType())) {
                            return null;
                        }
                        throw new UnsupportedOperationException(method.toGenericString());
                    }
                });
    }

}

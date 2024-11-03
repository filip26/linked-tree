package com.apicatalog.linkedtree.orm.proxy;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodHandles.Lookup;
import java.lang.invoke.MethodType;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.orm.Provided;
import com.apicatalog.linkedtree.orm.getter.Getter;

public class FragmentProxyInvocation implements InvocationHandler {

    static final Method LD_METHOD = Linkable.method();

    final float javaVersion;

    final FragmentProxy fragmentProxy;
    final LinkedFragment source;

    final Map<String, Object> cache;

    public FragmentProxyInvocation(final FragmentProxy fragmentProxy, final LinkedFragment source) {
        this.fragmentProxy = fragmentProxy;
        this.source = source;

        this.javaVersion = Float.parseFloat(System.getProperty("java.class.version"));
        this.cache = new HashMap<>(); // TODO LRU cache
    }

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
                            MethodType.methodType(method.getReturnType(), new Class[0]),
                            method.getDeclaringClass())
                    .bindTo(proxy)
                    .invokeWithArguments(args);
        }

        if (LD_METHOD.equals(method)) {
            return source;
        }

        if (cache.containsKey(method.getName())) {
            return cache.get(method.getName());
        }

        if (method.isAnnotationPresent(Provided.class)) {
            if (method.getReturnType().isAssignableFrom(Collection.class)) {
                return Collections.emptyList();
            }
            return null;
        }

        final Getter getter = fragmentProxy.getters.get(method);

        LinkedFragment target = source;

        if (getter != null) {
            return cache(method.getName(), getter.get(target));
        }
        if (fragmentProxy.typeInterface.equals(method.getDeclaringClass())) {
            return cache(method.getName(), method.invoke(target, args));
        }
        if (Object.class.equals(method.getDeclaringClass())) {
            return method.invoke(target, args);
        }
        if (PropertyValueConsumer.class.equals(method.getDeclaringClass())
                && "acceptFragmentPropertyValue".equals(method.getName())
                && args.length == 2) {
            cache((String) args[0], args[1]);
            return null;
        }

        throw new UnsupportedOperationException(method.toGenericString());
    }

    Object cache(String method, Object value) {
        cache.put(method, value);
        return value;
    }
}

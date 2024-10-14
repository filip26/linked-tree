package com.apicatalog.linkedtree.orm.proxy;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodHandles.Lookup;
import java.lang.invoke.MethodType;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.orm.getter.Getter;

public class FragmentProxyInvocation implements InvocationHandler {

    static final Method LD_METHOD = Linkable.method();

    final FragmentProxy fragmentProxy;
    final LinkedFragment source;

    final Map<Method, Object> cache;

    public FragmentProxyInvocation(final FragmentProxy fragmentProxy, final LinkedFragment source) {
        this.fragmentProxy = fragmentProxy;
        this.source = source;

        this.cache = new HashMap<>(); // TODO LRU cache
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {

        if (method.isDefault()) {
//            final float version = Float.parseFloat(System.getProperty("java.class.version"));
//            if (version <= 52) {
//                final Constructor<Lookup> constructor = Lookup.class.getDeclaredConstructor(Class.class);
//                constructor.setAccessible(true);
//
//                final Class<?> clazz = method.getDeclaringClass();
//                return constructor.newInstance(clazz)
//                        .in(clazz)
//                        .unreflectSpecial(method, clazz)
//                        .bindTo(proxy)
//                        .invokeWithArguments(args);
//            } else {
                return MethodHandles.lookup()
                        .findSpecial(
                                method.getDeclaringClass(),
                                method.getName(),
                                MethodType.methodType(method.getReturnType(), new Class[0]),
                                method.getDeclaringClass())
                        .bindTo(proxy)
                        .invokeWithArguments(args);
//            }
        }

        if (LD_METHOD.equals(method)) {
            return source;
        }

        if (cache.containsKey(method)) {
            return cache.get(method);
        }

        final Getter getter = fragmentProxy.getters.get(method);

        if (getter != null) {
            return cache(method, fragmentProxy.getters.get(method).get(source));
        }
        if (fragmentProxy.typeInterface.equals(method.getDeclaringClass())) {
            return cache(method, method.invoke(source, args));
        }
        if (Object.class.equals(method.getDeclaringClass())) {
            return method.invoke(source, args);
        }

        throw new UnsupportedOperationException(method.toGenericString());
    }

    Object cache(Method method, Object value) {
        cache.put(method, value);
        return value;
    }
}

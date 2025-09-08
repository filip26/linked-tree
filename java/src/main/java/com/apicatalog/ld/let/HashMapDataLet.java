package com.apicatalog.ld.let;

import java.util.Collections;
import java.util.HashMap;
import java.util.Optional;
import java.util.Set;

public final class HashMapDataLet extends HashMap<Class<?>, Object> implements DataLet {

    private static final long serialVersionUID = -5360701962597737860L;

    @Override
    public Set<Class<?>> interfaces() {
        return Collections.unmodifiableSet(keySet());
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> Optional<T> get(Class<T> iface) {
        Object impl = super.get(iface);
        if (impl == null) {
            return Optional.empty();
        }
        return Optional.of((T) impl);
    }

    @Override
    public <T> void set(Class<T> face, T implementation) {
        if (face == null || implementation == null) {
            throw new IllegalArgumentException("Interface and implementation cannot be null");
        }
        if (!face.isInterface()) {
            throw new IllegalArgumentException("Key must be an interface");
        }
        if (!face.isInstance(implementation)) {
            throw new IllegalArgumentException("Implementation does not implement the interface");
        }

        super.put(face, implementation);
    }

    @Override
    public boolean contains(Class<?> iface) {
        return super.containsKey(iface);
    }
}

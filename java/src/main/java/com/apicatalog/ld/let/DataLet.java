package com.apicatalog.ld.let;

import java.util.Objects;
import java.util.Optional;
import java.util.Set;

public interface DataLet {
    
    /**
     * Returns the Java interface types of all currently materialized representations.
     */
    Set<Class<?>> interfaces();

    /**
     * Gets a materialization (implementation) of the requested interface if available.
     */
    <T> Optional<T> get(Class<T> face);

    /**
     * Adds or replaces the materialization of the given interface.
     */
    <T> void set(Class<T> face, T implementation);

    /**
     * Checks if a materialization of the requested interface currently exists.
     */
    default boolean contains(Class<?> face) {
        Objects.requireNonNull(face, "Interface class cannot be null");
        if (!face.isInterface()) {
            throw new IllegalArgumentException("Parameter must be an interface type");
        }
        return get(face).isPresent();
    }
}

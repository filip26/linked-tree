package com.apicatalog.linkedtree.type;

import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public interface FragmentType extends Iterable<String> {

    public static FragmentType empty() {
        return EmptyType.instance();
    }
    
    default boolean contains(String type) {
        return stream().anyMatch(type::equals);
    }

    boolean isEmpty();

    /**
     * Creates a new instance T initialized
     * 
     * @param clazz
     * @return
     * @throws NodeAdapterError
     */
    default <T> T materialize(Class<T> clazz) throws NodeAdapterError {
        throw new ClassCastException();
    }

    default boolean isAdaptableTo(Class<?> clazz) {
        return false;
    }

    default Stream<String> stream() {
        return StreamSupport.stream(spliterator(), false);
    }
}

package com.apicatalog.linkedtree.type;

import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public interface Type extends Iterable<String> {

    public static Type empty() {
        return AdaptableType.EMPTY;
    }
    
    boolean contains(String type);

//    Type get(String type);

    boolean isEmpty();

    /**
     * Creates a new instance T initialized
     * 
     * @param clazz
     * @return
     * @throws ClassCastException
     */
    default <T> T adapt(Class<T> clazz) throws TypeAdapterError, ClassCastException {
        throw new ClassCastException();
    }

    default boolean isAdaptableTo(Class<?> clazz) {
        return false;
    }

    default Stream<String> stream() {
        return StreamSupport.stream(spliterator(), false);
    }

}

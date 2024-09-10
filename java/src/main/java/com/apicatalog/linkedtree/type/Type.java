package com.apicatalog.linkedtree.type;

import java.util.Collections;
import java.util.Iterator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public interface Type extends Iterable<String> {

    public static Type EMPTY = new Type() {

        @Override
        public Iterator<String> iterator() {
            return Collections.emptyIterator();
        }

        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public boolean contains(String type) {
            return false;
        }

//        @Override
//        public Type get(String type) {
//            return null;
//        }

        @Override
        public <T> T adapt(Class<T> clazz) throws ClassCastException {
            throw new ClassCastException("A type set is empty. It cannot be cast to an unknown interface/class " + clazz);
        }
    };

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

package com.apicatalog.linkedtree.type;

import java.util.Collections;
import java.util.Iterator;

final class EmptyType implements Type {

    static final Type EMPTY = new EmptyType();
    
    static final Type instance() {
        return EMPTY;
    }

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

    @Override
    public <T> T materialize(Class<T> clazz) throws ClassCastException {
        throw new ClassCastException("A type set is empty. It cannot be cast to an unknown interface/class " + clazz);
    }

    @Override
    public String toString() {
        return "Type.empty()";
    }
}

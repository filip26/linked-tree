package com.apicatalog.linkedtree;

import java.lang.reflect.Method;

/**
 * Allows to hide {@link LinkedNode} and inherited interfaces. Only
 * {@link Linkable#ld()} method is exposed.
 */
public interface Linkable {

    static Method method() {
        try {
            return Linkable.class.getDeclaredMethod("ld");
        } catch (NoSuchMethodException | SecurityException e) {
            // does not happen
            throw new IllegalStateException(e);
        }
    }

    LinkedNode ld();
}

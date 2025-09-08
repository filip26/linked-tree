package com.apicatalog.linkedtree;

import java.lang.reflect.Method;

/**
 * Allows to hide {@link LinkedNode} and inherited interfaces. Only
 * {@link LinkedData#ld()} method is exposed.
 */
public interface LinkedData {

    static Method method() {
        try {
            return LinkedData.class.getDeclaredMethod("ld");
        } catch (NoSuchMethodException | SecurityException e) {
            // does not happen
            throw new IllegalStateException(e);
        }
    }

    LinkedNode ld();
}

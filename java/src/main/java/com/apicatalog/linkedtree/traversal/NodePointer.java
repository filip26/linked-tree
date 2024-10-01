package com.apicatalog.linkedtree.traversal;

import java.util.Objects;

public record NodePointer(
        int[] indices,
        String[] terms) {

    public NodePointer {
        Objects.requireNonNull(indices);
        Objects.requireNonNull(terms);

        if (indices.length < terms.length) {
            throw new IllegalArgumentException("Terms length [" + terms.length + "] is greater than indices length [" + indices.length + "]");
        }
    }

    public boolean match(Object[] path) {
        if (path.length != (indices.length + terms.length - 1)) {
            return false;
        }
        for (int i = 0; i < indices.length + terms.length - 1; i++) {
            if (i % 2 == 0 && indices[i / 2] != (int) path[i]
                    || i % 2 == 1 && !terms[i / 2].equals(path[i])) {
                return false;
            }
        }
        return true;
    }

    public String term() {
        return terms.length > 0 ? terms[terms.length - 1] : null;
    }

    public int index() {
        return indices[indices.length - 1];
    }

    public static NodePointer of(Object... pointer) {

        int[] indices = new int[pointer.length / 2 + pointer.length % 2];
        String[] terms = new String[pointer.length / 2];

        for (int i = 0; i < pointer.length; i++) {
            if (i % 2 == 0) {
                indices[i / 2] = (int) pointer[i];
            } else {
                terms[i / 2] = (String) pointer[i];
            }
        }

        return new NodePointer(indices, terms);
    }

    @Override
    public final String toString() {
        var builder = new StringBuilder();
        for (int i = 0; i < indices.length; i++) {
            if (i > 0) {
                builder.append('.');
            }
            builder.append(indices[i]);
            if (i < terms.length) {
                builder.append('.');
                builder.append(terms[i]);
            }
        }
        return builder.toString();
    }
}

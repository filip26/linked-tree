package com.apicatalog.linkedtree.traversal;

public record NodePointer(
        int[] indices,
        String[] terms) {

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
        return terms[terms.length - 1];
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

}

package com.apicatalog.linkedtree.builder;

/**
 * A tree builder, a common ancestor to all exceptions related to building
 * linked trees.
 */
public class TreeBuilderError extends Exception {

    private static final long serialVersionUID = 1425627206854208226L;

    String term;

    public TreeBuilderError() {
        super();
    }

    public TreeBuilderError(Throwable cause) {
        super(cause);
    }

    public TreeBuilderError(Throwable cause, String term) {
        super(cause);
        this.term = term;
    }

    public String term() {
        return term;
    }
}

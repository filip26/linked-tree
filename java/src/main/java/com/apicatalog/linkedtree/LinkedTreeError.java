package com.apicatalog.linkedtree;

/**
 * A general error, a common ancestor to all exceptions related to linked trees.
 */
public class LinkedTreeError extends Exception {

    private static final long serialVersionUID = 1425627206854208226L;

    public LinkedTreeError() {
        super();
    }

    public LinkedTreeError(Throwable cause) {
        super(cause);
    }
}

package com.apicatalog.linkedtree.adapter;

public class NodeAdapterError extends Exception {

    private static final long serialVersionUID = -6115617775469766306L;

    public NodeAdapterError() {
        super();
    }

    public NodeAdapterError(String message) {
        super(message);
    }

    public NodeAdapterError(Throwable e) {
        super(e);
    }
}

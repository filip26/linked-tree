package com.apicatalog.linkedtree.adapter;

public class AdapterError extends Exception {

    private static final long serialVersionUID = -6115617775469766306L;

    public AdapterError() {
        super();
    }

    public AdapterError(String message) {
        super(message);
    }

    public AdapterError(Throwable e) {
        super(e);
    }


}

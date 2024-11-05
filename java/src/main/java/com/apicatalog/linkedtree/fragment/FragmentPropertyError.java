package com.apicatalog.linkedtree.fragment;

import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public class FragmentPropertyError extends NodeAdapterError {

    private static final long serialVersionUID = -317259685013176997L;

    protected final String propertyName;

    public FragmentPropertyError(String message, String propertyName) {
        super(message);
        this.propertyName = propertyName;
    }

    public FragmentPropertyError(Throwable e, String propertyName) {
        super(e);
        this.propertyName = propertyName;
    }
    
    public String getPropertyName() {
        return propertyName;
    }
}

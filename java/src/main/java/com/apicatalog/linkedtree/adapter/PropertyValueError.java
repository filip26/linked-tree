package com.apicatalog.linkedtree.adapter;

public class PropertyValueError extends NodeAdapterError {

    private static final long serialVersionUID = -317259685013176997L;

    protected final String propertyName;
    
    public PropertyValueError(Throwable e, String propertyName) {
        super(e);
        this.propertyName = propertyName;
    }
    
    public String getPropertyName() {
        return propertyName;
    }
}

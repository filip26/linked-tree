package com.apicatalog.linkedtree.orm.mapper;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

class DatatypeEnforcer<T extends LinkedLiteral> implements ObjectReader<T, Object>{

    protected final String datatype;
    protected final ObjectReader<T, ?> reader;


    public DatatypeEnforcer(String datatype, ObjectReader<T, ?> reader) {
        this.datatype = datatype;
        this.reader = reader;
    }
    
    @Override
    public Object object(T literal) throws NodeAdapterError {
        
        if (datatype.equals(literal.datatype())) {
            return reader.object(literal);
        }
        
        throw new NodeAdapterError("Expected datatype + " + datatype + " but got " + literal);
    }

}

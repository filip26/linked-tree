package com.apicatalog.linkedtree;

import com.apicatalog.linkedtree.selector.InvalidSelector;
import com.apicatalog.linkedtree.type.TypeAdapter;
import com.apicatalog.linkedtree.type.TypeAdapterError;

public class AlumniCredential extends VerifiableCredential {

    public static final String TYPE = "https://www.w3.org/ns/credentials/examples#AlumniCredential";
    
    protected AlumniCredential() {
        super();
    }

    public static AlumniCredential of(LinkedFragment fragment) throws TypeAdapterError {
        try {
            return (AlumniCredential) setup(new AlumniCredential(), fragment);
        } catch (InvalidSelector e) {
            throw new TypeAdapterError(e);
        }

    }
    
    static final TypeAdapter ADAPTER = new TypeAdapter() {

        @Override
        public Class<?> typeInterface() {
            return AlumniCredential.class;
        }

        @Override
        public Object materialize(LinkedFragment fragment) throws TypeAdapterError {
            return AlumniCredential.of(fragment);
        }
    };

    public static TypeAdapter typeAdapter() {
        return ADAPTER;
    }

}

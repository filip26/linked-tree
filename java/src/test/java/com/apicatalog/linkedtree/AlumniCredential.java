package com.apicatalog.linkedtree;

import com.apicatalog.linkedtree.type.TypeAdapter;
import com.apicatalog.linkedtree.type.TypeAdapterError;

public class AlumniCredential extends VerifiableCredential {

    public static final String TYPE = "https://www.w3.org/ns/credentials/examples#AlumniCredential";
    
    protected AlumniCredential(LinkedFragment fragment) {
        super(fragment);
        // TODO Auto-generated constructor stub
    }

    public static AlumniCredential of(boolean x) {
        System.out.println("A");
        return null;
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

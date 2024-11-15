package com.apicatalog.linkedtree.test;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.type.GenericTypeAdapter;
import com.apicatalog.linkedtree.type.TypeAdapter;

public class AlumniCredential extends VerifiableCredential {

    public static final String TYPE = "https://www.w3.org/ns/credentials/examples#AlumniCredential";

    protected AlumniCredential() {
        super();
    }

    public static AlumniCredential of(LinkedFragment fragment) throws NodeAdapterError {
        return (AlumniCredential) setup(new AlumniCredential(), fragment);
    }

    static final TypeAdapter ADAPTER = new GenericTypeAdapter(
            TYPE,
            AlumniCredential.class,
            AlumniCredential::of);

    public static TypeAdapter typeAdapter() {
        return ADAPTER;
    }

}

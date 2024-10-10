package com.apicatalog.linkedtree;

import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.selector.InvalidSelector;
import com.apicatalog.linkedtree.type.GenericTypeAdapter;
import com.apicatalog.linkedtree.type.TypeAdapter;

public class AlumniCredential extends VerifiableCredential {

    public static final String TYPE = "https://www.w3.org/ns/credentials/examples#AlumniCredential";

    protected AlumniCredential() {
        super();
    }

    public static AlumniCredential of(LinkedFragment fragment) throws NodeAdapterError {
        try {
            return (AlumniCredential) setup(new AlumniCredential(), fragment);
        } catch (InvalidSelector e) {
            throw new NodeAdapterError(e);
        }
    }

    static final TypeAdapter ADAPTER = new GenericTypeAdapter(
            TYPE,
            AlumniCredential.class,
            AlumniCredential::of);

    public static TypeAdapter typeAdapter() {
        return ADAPTER;
    }

}

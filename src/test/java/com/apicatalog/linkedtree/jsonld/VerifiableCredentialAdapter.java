package com.apicatalog.linkedtree.jsonld;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.io.LinkedFragmentAdapter;
import com.apicatalog.linkedtree.link.Link;

public class VerifiableCredentialAdapter implements LinkedFragmentAdapter {

    static final String VC_TYPE = "https://www.w3.org/2018/credentials#VerifiableCredential";

    @Override
    public boolean accepts(String id, Collection<String> types) {
        return types != null && types.contains(VC_TYPE);
    }

    @Override
    public LinkedFragment read(Link id, Collection<String> types, Map<String, LinkedContainer> properties, Object meta) {
        return VerifiableCredential.of(properties, meta);
    }

}

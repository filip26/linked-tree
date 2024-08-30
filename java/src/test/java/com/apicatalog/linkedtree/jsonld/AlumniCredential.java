package com.apicatalog.linkedtree.jsonld;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedContainer;

public class AlumniCredential extends VerifiableCredential {

    protected AlumniCredential(Link id, Collection<String> type, Map<String, LinkedContainer> properties) {
        super(id, type, properties);
        // TODO Auto-generated constructor stub
    }

    public static AlumniCredential of(boolean x) {
        System.out.println("A");
        return null;
    }
}

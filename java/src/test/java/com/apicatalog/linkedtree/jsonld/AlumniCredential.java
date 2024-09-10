package com.apicatalog.linkedtree.jsonld;

import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.VerifiableCredential;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.type.Type;

public class AlumniCredential extends VerifiableCredential {

    protected AlumniCredential(Link id, Type type, Map<String, LinkedContainer> properties) {
        super(id, type, properties);
        // TODO Auto-generated constructor stub
    }

    public static AlumniCredential of(boolean x) {
        System.out.println("A");
        return null;
    }
}

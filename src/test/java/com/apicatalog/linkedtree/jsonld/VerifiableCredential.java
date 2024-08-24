package com.apicatalog.linkedtree.jsonld;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.lang.LangStringSelector;

public class VerifiableCredential implements LinkedFragment {

    LangStringSelector name;
    LangStringSelector description;
    
    public static VerifiableCredential of(boolean x) {
        System.out.println("B");
        return null;
    }

}

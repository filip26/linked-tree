package com.apicatalog.linkedtree.value;

import com.apicatalog.linkedtree.LinkedLiteral;

public interface LangString extends LinkedLiteral {

    /**
     * An optional language tag. If this value is specified, {@link #datatype()}
     * returns rdf:langString.
     *
     * @return language tag or <code>null</code> if not set
     */
    String language();

//    public enum DirectionType {
//
//        LTR,
//
//        RTL,
//
//        NULL
//
//    }
}

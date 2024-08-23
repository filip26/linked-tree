package com.apicatalog.linkedtree.literal;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.xsd.XsdConstants;

public interface LangString extends LinkedLiteral {

    @Override
    default String datatype() {
        return XsdConstants.STRING;
    }
    
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

package com.apicatalog.linkedtree.lang;

import com.apicatalog.linkedtree.LinkedLiteral;

public interface LangStringLiteral extends LinkedLiteral {

    public enum LanguageDirection {
        LTR,
        RTL,
        NULL
    }

    @Override
    default String datatype() {
        return "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString";
    }

    /**
     * An optional language tag. If this value is specified, {@link #datatype()}
     * returns rdf:langString.
     *
     * @return language tag or <code>null</code> if not set
     */
    String language();

    LanguageDirection direction();
}

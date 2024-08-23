package com.apicatalog.linkedtree;

public non-sealed interface LinkedLiteral extends LinkedNode {

    @Override
    default boolean isLiteral() {
        return true;
    }

    @Override
    default LinkedLiteral asLiteral() {
        return this;
    }

    /**
     * Get the lexical value of the literal.
     *
     * @return lexical value, never <code>null</code>
     */
    String value();

    /**
     * An absolute IRI denoting the datatype IRI of the literal. If the value is
     * rdf:langString, {@link #language()} value is present.
     *
     * @return an absolute IRI, never <code>null</code>
     */
    String datatype();
}

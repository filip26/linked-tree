package com.apicatalog.linkedtree;

public non-sealed interface LinkedValue extends LinkedData {

    /**
     * Get the lexical value of the literal.
     *
     * @return lexical value, never <code>null</code>
     */
//    @Override
    String value();

    /**
     * An absolute IRI denoting the datatype IRI of the literal. If the value is
     * rdf:langString, {@link #language()} value is present.
     *
     * @return an absolute IRI, never <code>null</code>
     */
    String datatype();

    /**
     * An optional language tag. If this value is specified, {@link #datatype()} returns rdf:langString.
     *
     * @return language tag or {@link Optional#empty()} if not set
     */
//    Optional<String> language();

}

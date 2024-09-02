package com.apicatalog.linkedtree;

public interface LinkedLiteral extends LinkedNode {

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
    String lexicalValue();

    /**
     * An absolute IRI denoting the datatype IRI of the literal.
     *
     * @return an absolute IRI, never <code>null</code>
     */
    String datatype();

    @SuppressWarnings("unchecked")
    default <T> T cast(Class<T> clazz) {
        return (T) this;
    }
    
    default Linkable cast() {
        return this;
    }
}

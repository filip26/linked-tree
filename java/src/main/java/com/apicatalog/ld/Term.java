package com.apicatalog.ld;

import java.io.Serializable;
import java.util.Objects;

import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;

public final class Term implements Serializable {

    private static final long serialVersionUID = -543670450852760002L;

    public static final Term ID = new Term(JsonLdKeyword.ID);
    public static final Term TYPE = new Term(JsonLdKeyword.TYPE);

    final String name;
    final String vocabulary;
    final String uri;

    Term(String name) {
        this.name = name;
        this.vocabulary = null;
        this.uri = name;
    }

    Term(String name, String vocabulary) {
        this.name = name;
        this.vocabulary = vocabulary;
        this.uri = vocabulary + name;
    }

    public static final Term create(String name, String vocabulary) {
        return new Term(name, vocabulary);
    }

    public String uri() {
        return uri;
    }

    public String name() {
        return name;
    }

    @Override
    public int hashCode() {
        return Objects.hash(uri);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Term other = (Term) obj;
        return Objects.equals(uri, other.uri);
    }

    @Override
    public String toString() {
        return uri;
    }
}

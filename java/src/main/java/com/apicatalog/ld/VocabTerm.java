package com.apicatalog.ld;

import java.io.Serializable;
import java.util.Objects;

import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;

public final class VocabTerm implements Serializable {

    private static final long serialVersionUID = -543670450852760002L;

    public static final VocabTerm ID = new VocabTerm(JsonLdKeyword.ID);
    public static final VocabTerm TYPE = new VocabTerm(JsonLdKeyword.TYPE);

    final String name;
    final String vocabulary;
    final String uri;

    VocabTerm(String name) {
        this.name = name;
        this.vocabulary = null;
        this.uri = name;
    }

    VocabTerm(String name, String vocabulary) {
        this.name = name;
        this.vocabulary = vocabulary;
        this.uri = vocabulary + name;
    }

    public static final VocabTerm create(String name, String vocabulary) {
        return new VocabTerm(name, vocabulary);
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
        VocabTerm other = (VocabTerm) obj;
        return Objects.equals(uri, other.uri);
    }

    @Override
    public String toString() {
        return uri;
    }
}

package com.apicatalog.linkedtree.lang;

import java.util.Objects;

import com.apicatalog.linkedtree.LinkedTree;

public record ImmutableLangString(
        String lexicalValue,
        String language,
        LanguageDirectionType direction,
        LinkedTree root) implements LangString {

    @Override
    public String toString() {
        return "ImmutableLangString [lexicalValue=" + lexicalValue + ", language=" + language + ", direction=" + direction + "]";
    }

    @Override
    public int hashCode() {
        return Objects.hash(direction, language, lexicalValue);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ImmutableLangString other = (ImmutableLangString) obj;
        return direction == other.direction && Objects.equals(language, other.language) && Objects.equals(lexicalValue, other.lexicalValue);
    }
}

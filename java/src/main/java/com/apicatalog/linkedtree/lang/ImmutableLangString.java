package com.apicatalog.linkedtree.lang;

public record ImmutableLangString(
        String lexicalValue,
        String language,
        LanguageDirectionType direction) implements LangString {
}

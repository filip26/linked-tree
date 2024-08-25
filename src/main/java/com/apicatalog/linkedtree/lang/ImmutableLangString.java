package com.apicatalog.linkedtree.lang;

public record ImmutableLangString(
        String value,
        String language,
        DirectionType direction,
        Object pi) implements LangString {
}

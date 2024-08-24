package com.apicatalog.linkedtree.lang;

public class ImmutableLangString implements LangString {

    protected final String value;
    protected final String language;
    protected final DirectionType direction;
    protected final Object meta;

    protected ImmutableLangString(String value, String language, DirectionType direction, Object meta) {
        this.value = value;
        this.language = language;
        this.direction = direction;
        this.meta = meta;
    }

    public static ImmutableLangString of(String value, String language, String direction, Object meta) {
        //TODO direction
        return new ImmutableLangString(value, language, null, meta);
    }

    @Override
    public String value() {
        return value;
    }

    @Override
    public String language() {
        return language;
    }

    @Override
    public Object metadata() {
        return meta;
    }

    @Override
    public DirectionType direction() {
        return direction;
    }
}

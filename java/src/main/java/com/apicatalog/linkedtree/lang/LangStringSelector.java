package com.apicatalog.linkedtree.lang;

import java.util.Collection;

public interface LangStringSelector {

    Collection<LangString> values();

    Collection<String> languages();

    LangString locale(String langCode);

    int size();

    default LangString first() {
        final Collection<LangString> strings = values();
        if (strings != null && !strings.isEmpty()) {
            return strings.iterator().next();
        }
        return null;
    }
}

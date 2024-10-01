package com.apicatalog.linkedtree.lang;

import java.util.Collection;
import java.util.Collections;

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

    static LangStringSelector empty() {
        return EMPTY;
    }

    final static LangStringSelector EMPTY = new LangStringSelector() {

        @Override
        public Collection<LangString> values() {
            return Collections.emptyList();
        }

        @Override
        public int size() {
            return 0;
        }

        @Override
        public LangString locale(String langCode) {
            return null;
        }

        @Override
        public Collection<String> languages() {
            return Collections.emptyList();
        }
    };

}

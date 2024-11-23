package com.apicatalog.linkedtree.lang;

import java.util.Collection;
import java.util.Collections;

public interface LocalizedString {

    Collection<LangStringLiteral> values();

    Collection<String> languages();

    LangStringLiteral get(String langCode);

    int size();

    default LangStringLiteral first() {
        final Collection<LangStringLiteral> strings = values();
        if (strings != null && !strings.isEmpty()) {
            return strings.iterator().next();
        }
        return null;
    }

    static LocalizedString empty() {
        return EMPTY;
    }

    final static LocalizedString EMPTY = new LocalizedString() {

        @Override
        public Collection<LangStringLiteral> values() {
            return Collections.emptyList();
        }

        @Override
        public int size() {
            return 0;
        }

        @Override
        public LangStringLiteral get(String langCode) {
            return null;
        }

        @Override
        public Collection<String> languages() {
            return Collections.emptyList();
        }
    };

}

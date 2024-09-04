package com.apicatalog.linkedtree.lang;

import com.apicatalog.linkedtree.LinkedTree;

public record ImmutableLangString(
        String lexicalValue,
        String language,
        LanguageDirectionType direction,
        LinkedTree root
        ) implements LangString {
}

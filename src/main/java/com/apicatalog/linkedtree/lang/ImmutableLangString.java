package com.apicatalog.linkedtree.lang;

import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public record ImmutableLangString(
        String value,
        String language,
        LanguageDirectionType direction,
        ProcessingInstruction pi) implements LangString {
}

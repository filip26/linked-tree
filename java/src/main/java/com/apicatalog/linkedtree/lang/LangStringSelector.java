package com.apicatalog.linkedtree.lang;

import java.util.Collection;

public interface LangStringSelector {

    Collection<LangString> strings();
    
    Collection<String> langCodes();
    
    LangString get(String langCode);

    int size();
    
    default LangString single() {
        final Collection<LangString> strings = strings();
        if (strings == null || strings.size() != 1) {
            throw new IllegalStateException();
        }
        return strings.iterator().next();
    }
    
}

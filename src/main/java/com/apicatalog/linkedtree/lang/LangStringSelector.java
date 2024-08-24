package com.apicatalog.linkedtree.lang;

import java.util.Collection;

public interface LangStringSelector {

    Collection<LangString> strings();
    
    Collection<String> langCodes();
    
    LangString get(String langCode);
    
}

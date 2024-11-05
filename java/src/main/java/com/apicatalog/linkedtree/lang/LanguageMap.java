package com.apicatalog.linkedtree.lang;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public class LanguageMap implements LangStringSelector {

    protected Map<String, LangString> langMap;

    protected LanguageMap(Map<String, LangString> langMap) {
        this.langMap = langMap;
    }

    public static LanguageMap of(LinkedContainer container) throws NodeAdapterError {

        final Map<String, LangString> map = new HashMap<>(container.size());

        for (LinkedNode node : container) {
            if (!node.isLiteral()) {
                throw new IllegalArgumentException();
            }

            if (node instanceof LangString langString) {
                map.put(langString.language(), langString);
                continue;
            }

            throw new IllegalArgumentException("Expected a string or an array of string but got " + node);
        }

        return new LanguageMap(map);
    }

    @Override
    public Collection<LangString> values() {
        return langMap.values();
    }

    @Override
    public Collection<String> languages() {
        return langMap.keySet();
    }

    @Override
    public LangString locale(String langCode) {
        return langMap.get(langCode);
    }

    @Override
    public int size() {
        return langMap.size();
    }
}

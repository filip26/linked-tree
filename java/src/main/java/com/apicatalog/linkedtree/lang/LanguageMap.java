package com.apicatalog.linkedtree.lang;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public class LanguageMap implements LocalizedString {

    protected Map<String, LangStringLiteral> langMap;

    protected LanguageMap(Map<String, LangStringLiteral> langMap) {
        this.langMap = langMap;
    }

    public static LanguageMap of(LinkedContainer container) throws NodeAdapterError {

        final Map<String, LangStringLiteral> map = new HashMap<>(container.size());

        for (LinkedNode node : container) {
            if (!node.isLiteral()) {
                throw new IllegalArgumentException();
            }

            if (node instanceof LangStringLiteral langString) {
                map.put(langString.language(), langString);
                continue;
            }

            throw new IllegalArgumentException("Expected a string or an array of string but got " + node);
        }

        return new LanguageMap(map);
    }

    @Override
    public Collection<LangStringLiteral> values() {
        return langMap.values();
    }

    @Override
    public Collection<String> languages() {
        return langMap.keySet();
    }

    @Override
    public LangStringLiteral get(String langCode) {
        return langMap.get(langCode);
    }

    @Override
    public int size() {
        return langMap.size();
    }
}

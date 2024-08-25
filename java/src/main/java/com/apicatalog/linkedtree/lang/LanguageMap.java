package com.apicatalog.linkedtree.lang;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;

public class LanguageMap implements LangStringSelector {

    protected Map<String, LangString> langMap;

    protected LanguageMap(Map<String, LangString> langMap) {
        this.langMap = langMap;
    }

    public static LanguageMap of(LinkedContainer container) {

        final Map<String, LangString> map = new HashMap<>(container.size());

        for (LinkedNode node : container.nodes()) {
            if (!node.isLiteral()) {
                throw new IllegalArgumentException();
            }

            if (node instanceof LangString langString) {
                map.put(langString.language(), langString);
                continue;
            }

            throw new IllegalStateException();
//TODO            
//            final LinkedLiteral literal = node.asLiteral();
//                
//             if (XsdConstants.STRING.equals(literal.datatype())
//                     || RdfConstants.LANG_STRING.equals(literal.datatype())
//                     ) {
//                map.put(null, langString)
//            }
        }

        return new LanguageMap(map);
    }

    @Override
    public Collection<LangString> strings() {
        return langMap.values();
    }

    @Override
    public Collection<String> langCodes() {
        return langMap.keySet();
    }

    @Override
    public LangString get(String langCode) {
        return langMap.get(langCode);
    }

    @Override
    public int size() {
        return langMap.size();
    }

}

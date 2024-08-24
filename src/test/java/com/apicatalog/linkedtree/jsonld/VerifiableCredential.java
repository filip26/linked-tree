package com.apicatalog.linkedtree.jsonld;

import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.lang.LanguageMap;

public class VerifiableCredential implements LinkedFragment {

    protected LangStringSelector name;
    protected LangStringSelector description;
    protected Object pi;

    protected VerifiableCredential() {
        // protected
    }

    public static VerifiableCredential of(Map<String, LinkedContainer> properties, Object meta) {
        return setup(new VerifiableCredential(), properties, meta);
    }

    public static VerifiableCredential setup(VerifiableCredential credential, Map<String, LinkedContainer> properties, Object meta) {

        credential.name = getLangMap(properties, "https://schema.org/name");
        credential.description = getLangMap(properties, "https://schema.org/description");

        return credential;
    }

    protected static LangStringSelector getLangMap(Map<String, LinkedContainer> properties, String term) {
        final LinkedContainer container = properties.get(term);
        if (container != null) {
            return LanguageMap.of(container);
        }
        return null;
    }

    public LangStringSelector name() {
        return name;
    }

    public LangStringSelector description() {
        return description;
    }

    @Override
    public Object pi() {
        return pi;
    }
}

package com.apicatalog.linkedtree.jsonld;

import java.time.Instant;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.xsd.XsdDateTime;

public class VerifiableCredential implements LinkedFragment {

    protected LangStringSelector name;
    protected LangStringSelector description;

    protected Instant validFrom;
    protected Instant validUntil;

    protected Object pi;

    protected VerifiableCredential() {
        // protected
    }

    public static VerifiableCredential of(Map<String, LinkedContainer> properties, Object meta) {
        return setup(new VerifiableCredential(), properties, meta);
    }

    protected static VerifiableCredential setup(VerifiableCredential credential, Map<String, LinkedContainer> properties, Object meta) {

        credential.name = getLangMap(properties, "https://schema.org/name");
        credential.description = getLangMap(properties, "https://schema.org/description");

        credential.validFrom = properties.containsKey("https://www.w3.org/2018/credentials#validFrom")
                ? properties.get("https://www.w3.org/2018/credentials#validFrom")
                        .singleLiteral(XsdDateTime.class)
                        .datetime()
                : null;

        credential.validUntil = properties.containsKey("https://www.w3.org/2018/credentials#validUntil")
                ? properties.get("https://www.w3.org/2018/credentials#validUntil")
                        .single()
                        .asLiteral()
                        .cast(XsdDateTime.class)
                        .datetime()
                : null;

        return credential;
    }

    protected static LangStringSelector getLangMap(Map<String, LinkedContainer> properties, String term) {
        final LinkedContainer container = properties.get(term);
        if (container != null) {
            return LanguageMap.of(container);
        }
        return null;
    }

    @Override
    public Object pi() {
        return pi;
    }
}

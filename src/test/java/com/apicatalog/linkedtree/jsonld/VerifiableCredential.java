package com.apicatalog.linkedtree.jsonld;

import java.time.Instant;
import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.xsd.XsdDateTime;

public class VerifiableCredential implements LinkedFragment {

    static final String TYPE = "https://www.w3.org/2018/credentials#VerifiableCredential";

    protected LangStringSelector name;
    protected LangStringSelector description;

    protected Instant validFrom;
    protected Instant validUntil;

    protected LinkedContainer subject;

    protected Link id;
    protected Collection<String> type;
    protected Map<String, LinkedContainer> properties;

    protected Object pi;

    protected VerifiableCredential(Link id, Collection<String> type, Map<String, LinkedContainer> properties) {
        this.id = id;
        this.type = type;
        this.properties = properties;
    }

    public static VerifiableCredential of(Link id, Collection<String> type, Map<String, LinkedContainer> properties, Object meta) {
        return setup(new VerifiableCredential(id, type, properties), properties, meta);
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

    public LinkedContainer subject() {
        return properties.get("https://www.w3.org/2018/credentials#credentialSubject");
    }

    public LinkedFragment issuer() {
        return properties.containsKey("https://www.w3.org/2018/credentials#issuer")
                ? properties.get("https://www.w3.org/2018/credentials#issuer")
                        .single()
                        .asFragment()
                : null;

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

    @Override
    public Link id() {
        return id;
    }

    @Override
    public Collection<String> type() {
        return type;
    }

    @Override
    public Collection<String> terms() {
        return properties.keySet();
    }

    @Override
    public LinkedContainer property(String term) {
        return properties.get(term);
    }
}

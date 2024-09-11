package com.apicatalog.linkedtree;

import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.Collection;
import java.util.Map;
import java.util.function.Supplier;

import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.type.Type;
import com.apicatalog.linkedtree.type.TypeAdapterError;
import com.apicatalog.linkedtree.xsd.XsdDateTime;

public class VerifiableCredential implements LinkedFragment {

    public static final String TYPE = "https://www.w3.org/2018/credentials#VerifiableCredential";

    protected LangStringSelector name;
    protected LangStringSelector description;

    protected Instant validFrom;
    protected Instant validUntil;

    protected LinkedContainer subject;

    protected Link id;
    protected Type type;
    protected Map<String, LinkedContainer> properties;
    protected Supplier<LinkedTree> treeSupplier;

    protected VerifiableCredential(Link id, Type type, Map<String, LinkedContainer> properties) {
        this.id = id;
        this.type = type;
        this.properties = properties;
    }

    public static VerifiableCredential of(Link id, Type type, Map<String, LinkedContainer> properties) throws TreeBuilderError {
        try {
            return setup(new VerifiableCredential(id, type, properties), properties);
        } catch (DateTimeParseException | ClassCastException | TypeAdapterError e) {
            throw new TreeBuilderError(e);
        }
    }

    protected static VerifiableCredential setup(VerifiableCredential credential, Map<String, LinkedContainer> properties) throws DateTimeParseException, ClassCastException, TypeAdapterError {

        credential.name = getLangMap(properties, "https://schema.org/name");
        credential.description = getLangMap(properties, "https://schema.org/description");

        credential.validFrom = properties.containsKey("https://www.w3.org/2018/credentials#validFrom")
                ? properties.get("https://www.w3.org/2018/credentials#validFrom")
                        .single(XsdDateTime.class)
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
    public Link id() {
        return id;
    }

    @Override
    public Type type() {
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

    public LangStringSelector name() {
        return name;
    }

    public LangStringSelector description() {
        return name;
    }

    public Instant validFrom() {
        return validFrom;
    }

    public Instant validUntil() {
        return validUntil;
    }

}

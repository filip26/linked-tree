package com.apicatalog.linkedtree;

import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.Collection;

import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.type.TypeAdapterError;

public class VerifiableCredential {

    public static final String TYPE = "https://www.w3.org/2018/credentials#VerifiableCredential";

    protected LangStringSelector name;
    protected LangStringSelector description;

    protected Instant validFrom;
    protected Instant validUntil;

    protected LinkedContainer subject;

    protected LinkedFragment source;

    protected VerifiableCredential(LinkedFragment source) {
        this.source = source;
    }

    public static VerifiableCredential of(LinkedFragment fragment) throws TypeAdapterError {
        try {
            return setup(
                    new VerifiableCredential(fragment),
                    fragment);
        } catch (DateTimeParseException | ClassCastException e) {
            throw new TypeAdapterError(e);
        }
    }

    protected static VerifiableCredential setup(VerifiableCredential credential, LinkedFragment source) throws DateTimeParseException, ClassCastException, TypeAdapterError {

        credential.name = getLangMap(source, "https://schema.org/name");
        credential.description = getLangMap(source, "https://schema.org/description");

//        credential.validFrom = source.containsKey("https://www.w3.org/2018/credentials#validFrom")
//                ? source.property("https://www.w3.org/2018/credentials#validFrom")
//                        .single(XsdDateTime.class)
//                        .datetime()
//                : null;
//
//        credential.validUntil = source.containsKey("https://www.w3.org/2018/credentials#validUntil")
//                ? source.get("https://www.w3.org/2018/credentials#validUntil")
//                        .single()
//                        .asLiteral()
//                        .cast(XsdDateTime.class)
//                        .datetime()
//                : null;

        return credential;
    }

    public LinkedContainer subject() {
//        return properties.get("https://www.w3.org/2018/credentials#credentialSubject");
        return null;
    }

    public LinkedFragment issuer() {
//        return properties.containsKey("https://www.w3.org/2018/credentials#issuer")
//                ? properties.get("https://www.w3.org/2018/credentials#issuer")
//                        .single()
//                        .asFragment()
//                : null;
        return null;
    }

    protected static LangStringSelector getLangMap(LinkedFragment fragment, String term) {
        final LinkedContainer container = fragment.property(term);
        if (container != null) {
            return LanguageMap.of(container);
        }
        return null;
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

    public String id() {
        // TODO Auto-generated method stub
        return null;
    }

    public Collection<String> type() {
        // TODO Auto-generated method stub
        return null;
    }

}

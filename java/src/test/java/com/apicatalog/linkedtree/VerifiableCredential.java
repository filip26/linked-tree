package com.apicatalog.linkedtree;

import java.time.Instant;

import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.selector.InvalidSelector;
import com.apicatalog.linkedtree.type.Type;
import com.apicatalog.linkedtree.type.TypeAdapter;
import com.apicatalog.linkedtree.type.TypeAdapterError;

public class VerifiableCredential {

    public static final String TYPE = "https://www.w3.org/2018/credentials#VerifiableCredential";

    protected String id;
    protected Type type;

    protected LangStringSelector name;
    protected LangStringSelector description;

    protected Instant validFrom;
    protected Instant validUntil;

    protected LinkedContainer subject;
    protected LinkedFragment issuer;

    protected VerifiableCredential() {
    }

    public static VerifiableCredential of(LinkedFragment fragment) throws TypeAdapterError {
        try {
            return setup(new VerifiableCredential(), fragment);
        } catch (InvalidSelector e) {
            throw new TypeAdapterError(e);
        }
    }

    protected static VerifiableCredential setup(VerifiableCredential credential, LinkedFragment source) throws InvalidSelector {

        credential.id = source.id().uri();
        credential.type = source.type();

        credential.name = source.langMap(
                "https://schema.org/name");

        credential.description = source.langMap(
                "https://schema.org/description");

        credential.validFrom = source.xsdDateTime(
                "https://www.w3.org/2018/credentials#validFrom");

        credential.validUntil = source.xsdDateTime(
                "https://www.w3.org/2018/credentials#validUntil");

        credential.subject = source.property(
                "https://www.w3.org/2018/credentials#credentialSubject");

        credential.issuer = source.singleFragment(
                "https://www.w3.org/2018/credentials#issuer");

        return credential;
    }

    public LinkedContainer subject() {
        return subject;
    }

    public LinkedFragment issuer() {
        return issuer;
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
        return id;
    }

    public Type type() {
        return type;
    }

    static final TypeAdapter ADAPTER = new TypeAdapter() {

        @Override
        public Class<?> typeInterface() {
            return VerifiableCredential.class;
        }

        @Override
        public Object materialize(LinkedFragment fragment) throws TypeAdapterError {
            return VerifiableCredential.of(fragment);
        }
    };

    public static TypeAdapter typeAdapter() {
        return ADAPTER;
    }

}

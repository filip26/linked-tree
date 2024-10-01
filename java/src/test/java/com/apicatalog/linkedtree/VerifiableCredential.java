package com.apicatalog.linkedtree;

import java.net.URI;
import java.time.Instant;
import java.util.Collection;

import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.selector.InvalidSelector;
import com.apicatalog.linkedtree.type.GenericTypeAdapter;
import com.apicatalog.linkedtree.type.Type;
import com.apicatalog.linkedtree.type.TypeAdapter;

public class VerifiableCredential {

    public static final String TYPE = "https://www.w3.org/2018/credentials#VerifiableCredential";

    protected URI id;
    protected Type type;

    protected LangStringSelector name;
    protected LangStringSelector description;

    protected Instant validFrom;
    protected Instant validUntil;

    protected LinkedContainer subject;
    protected URI issuer;

    protected Collection<Status> status;

    protected VerifiableCredential() {
    }

    public static VerifiableCredential of(LinkedFragment fragment) throws NodeAdapterError {
        try {
            return setup(new VerifiableCredential(), fragment);
        } catch (InvalidSelector e) {
            throw new NodeAdapterError(e);
        }
    }

    protected static VerifiableCredential setup(VerifiableCredential credential, LinkedFragment source) throws InvalidSelector {

        credential.id = source.uri();
        credential.type = source.type();

        credential.name = source.languageMap(
                "https://schema.org/name");

        credential.description = source.languageMap(
                "https://schema.org/description");

        credential.validFrom = source.xsdDateTime(
                "https://www.w3.org/2018/credentials#validFrom");

        credential.validUntil = source.xsdDateTime(
                "https://www.w3.org/2018/credentials#validUntil");

        credential.subject = source.container(
                "https://www.w3.org/2018/credentials#credentialSubject");

        credential.issuer = source.uri(
                "https://www.w3.org/2018/credentials#issuer");

        credential.status = source.collection(
                "https://www.w3.org/2018/credentials#credentialStatus",
                Status.class,
                UnknownStatus::new);

        return credential;
    }

    public LinkedContainer subject() {
        return subject;
    }

    public URI issuer() {
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

    public URI id() {
        return id;
    }

    public Type type() {
        return type;
    }

    public Collection<Status> status() {
        return status;
    }

    static final TypeAdapter ADAPTER = new GenericTypeAdapter(
            VerifiableCredential.class,
            VerifiableCredential::of);

    public static TypeAdapter typeAdapter() {
        return ADAPTER;
    }

}

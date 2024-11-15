package com.apicatalog.linkedtree.test;

import java.net.URI;
import java.time.Instant;
import java.util.Collection;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.type.FragmentType;
import com.apicatalog.linkedtree.type.GenericTypeAdapter;
import com.apicatalog.linkedtree.type.TypeAdapter;

public class VerifiableCredential {

    public static final String TYPE = "https://www.w3.org/2018/credentials#VerifiableCredential";

    static final TypeAdapter ADAPTER = new GenericTypeAdapter(
            TYPE,
            VerifiableCredential.class,
            VerifiableCredential::of);

    protected URI id;
    protected FragmentType type;

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
        return setup(new VerifiableCredential(), fragment);
    }

    protected static VerifiableCredential setup(VerifiableCredential credential, LinkedFragment source) throws NodeAdapterError {

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

    public FragmentType type() {
        return type;
    }

    public Collection<Status> status() {
        return status;
    }

    public static TypeAdapter typeAdapter() {
        return ADAPTER;
    }

}

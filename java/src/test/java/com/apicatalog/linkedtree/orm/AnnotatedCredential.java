package com.apicatalog.linkedtree.orm;

import java.net.URI;
import java.time.Instant;
import java.util.Collection;

import com.apicatalog.linkedtree.Status;
import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.type.Type;
import com.apicatalog.linkedtree.xsd.XsdDateTimeAdapter;

@Fragment
@Term("VerifiableCredential")
@Vocab("https://www.w3.org/2018/credentials#")
public interface AnnotatedCredential {

    @Id
    URI id();

    // implicit type selector, detected by return type
    Type type();

    @Vocab("https://schema.org/")
    LangStringSelector name();

    @Vocab("https://schema.org/")
    LangStringSelector description();

    @Term("credentialSubject")
    GenericSubject subject();

    URI issuer();

    @Literal(XsdDateTimeAdapter.class)
    Instant validFrom();

    @Literal(XsdDateTimeAdapter.class)
    Instant validUntil();

    Collection<Status> status();
}

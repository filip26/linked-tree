package com.apicatalog.linkedtree.orm.test;

import java.net.URI;
import java.time.Instant;
import java.util.Collection;

import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.orm.Context;
import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Id;
import com.apicatalog.linkedtree.orm.Literal;
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Vocab;
import com.apicatalog.linkedtree.test.Status;
import com.apicatalog.linkedtree.type.Type;
import com.apicatalog.linkedtree.xsd.XsdDateTimeAdapter;

@Fragment
@Term("VerifiableCredential")
@Vocab("https://www.w3.org/2018/credentials#")
@Context({
//        "https://www.w3.org/2018/credentials/v1",
//        "https://w3id.org/security/data-integrity/v2",
//        "https://www.w3.org/ns/credentials/examples/v1"
    "https://www.w3.org/ns/credentials/v2"
})
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

    @Term("credentialStatus")
    Collection<Status> status();
}

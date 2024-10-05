package com.apicatalog.linkedtree;

import java.net.URI;
import java.time.Instant;
import java.util.Collection;

import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Id;
import com.apicatalog.linkedtree.orm.Literal;
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Vocab;
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
    LinkedContainer subject();

    URI issuer();

    @Literal(adapter = XsdDateTimeAdapter.class)
    Instant validFrom();

    @Literal(adapter = XsdDateTimeAdapter.class)
    Instant validUntil();

    Collection<Status> status();
}

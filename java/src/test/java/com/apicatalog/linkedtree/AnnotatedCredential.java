package com.apicatalog.linkedtree;

import java.net.URI;
import java.time.Instant;
import java.util.Collection;

import com.apicatalog.ld.LdId;
import com.apicatalog.ld.LdType;
import com.apicatalog.ld.LdVocab;
import com.apicatalog.ld.LdTerm;
import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.type.Type;

@LdType("VerifiableCredential")
@LdVocab("https://www.w3.org/2018/credentials#")
public interface AnnotatedCredential {

    @LdId
    URI id();
    
    // implicit type selector, detected by return type
    Type type();
    
    @LdVocab("https://schema.org/")
    LangStringSelector name();

    @LdVocab("https://schema.org/")
    LangStringSelector description();

    @LdTerm
    LinkedContainer subject();

    @LdTerm
    URI issuer();

    @LdTerm
    Instant validFrom();

    @LdTerm
    Instant validUntil();

    @LdTerm
    Collection<Status> status();
}

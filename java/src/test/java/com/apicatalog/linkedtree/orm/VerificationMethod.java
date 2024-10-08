package com.apicatalog.linkedtree.orm;

import java.net.URI;
import java.time.Instant;

import com.apicatalog.linkedtree.type.Type;
import com.apicatalog.linkedtree.xsd.XsdDateTimeAdapter;

/**
 * Represents a verification method declaration.
 * 
 * https://www.w3.org/TR/controller-document/#verification-methods
 */
@Fragment(generic = true)
@Vocab("https://w3id.org/security#")
public interface VerificationMethod {

    @Id
    URI id();

    Type type();

    URI controller();

    @Literal(XsdDateTimeAdapter.class)
    Instant revoked();

    @Literal(XsdDateTimeAdapter.class)
    @Term("expiration")
    Instant expires();

}
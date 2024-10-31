package com.apicatalog.linkedtree.orm.test;

import java.net.URI;
import java.time.Instant;

import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Id;
import com.apicatalog.linkedtree.orm.Literal;
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Vocab;
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
    @Term(value = "expiration", compact = false)
    Instant expires();

}
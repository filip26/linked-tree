package com.apicatalog.linkedtree.orm.test;

import java.net.URI;
import java.time.Instant;
import java.util.Objects;

import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Id;
import com.apicatalog.linkedtree.orm.Literal;
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Vocab;
import com.apicatalog.linkedtree.type.FragmentType;
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

    FragmentType type();

    URI controller();

    @Literal(XsdDateTimeAdapter.class)
    Instant revoked();

    @Literal(XsdDateTimeAdapter.class)
    @Term(value = "expiration", compact = false)
    Instant expires();

    static boolean equals(VerificationMethod k1, VerificationMethod k2) {
        if (k1 == null || k2 == null) {
            return k1 == k2;

        }
        return Objects.equals(k1.id(), k2.id())
                && Objects.equals(k1.type(), k2.type())
                && Objects.equals(k1.controller(), k2.controller())
                && Objects.equals(k1.expires(), k2.expires())
                && Objects.equals(k1.revoked(), k2.revoked());
    }
}
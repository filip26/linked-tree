package com.apicatalog.linkedtree.orm.test;

import com.apicatalog.linkedtree.orm.Context;
import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Vocab;

import jakarta.json.JsonValue;

@Fragment
@Vocab("https://w3id.org/security#")
@Context("https://w3id.org/security/jwk/v1")
public interface JsonWebKey extends VerificationMethod {

    @Term("publicKeyJwk")
    JsonValue publicKey();

    @Term("secretKeyJwk")
    JsonValue privateKey();
}

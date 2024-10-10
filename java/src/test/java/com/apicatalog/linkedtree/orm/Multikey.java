package com.apicatalog.linkedtree.orm;

@Fragment
@Vocab("https://w3id.org/security#")
public interface Multikey extends VerificationMethod {

    @Term("publicKeyMultibase")
    @Literal(EncodedKeyAdapter.class)
    EncodedKey publicKey();

    @Term("secretKeyMultibase")
    @Literal(EncodedKeyAdapter.class)
    EncodedKey privateKey();

}

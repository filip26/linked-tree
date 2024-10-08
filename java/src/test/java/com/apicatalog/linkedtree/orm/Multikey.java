package com.apicatalog.linkedtree.orm;

@Fragment
@Vocab("https://w3id.org/security#")
public interface Multikey extends VerificationMethod {

    @Term("publicKeyMultibase")
    @Literal(EncodedKeyMapper.class)
    EncodedKey publicKey();

    @Term("secretKeyMultibase")
    @Literal(EncodedKeyMapper.class)
    EncodedKey privateKey();

}

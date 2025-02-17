package com.apicatalog.linkedtree.orm.test;

import com.apicatalog.linkedtree.orm.Context;
import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Mapper;
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Vocab;

@Fragment
@Vocab("https://w3id.org/security#")
@Context("https://w3id.org/security/multikey/v1")
public interface Multikey extends VerificationMethod {

    @Term("publicKeyMultibase")
    @Mapper(EncodedKeyAdapter.class)
    EncodedKey publicKey();

    @Term("secretKeyMultibase")
    @Mapper(EncodedKeyAdapter.class)
    EncodedKey privateKey();

}

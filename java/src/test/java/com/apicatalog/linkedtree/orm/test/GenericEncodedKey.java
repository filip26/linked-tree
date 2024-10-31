package com.apicatalog.linkedtree.orm.test;

import com.apicatalog.linkedtree.LinkedLiteral;

public record GenericEncodedKey(String encodedKey) implements EncodedKey, LinkedLiteral {

    @Override
    public String lexicalValue() {
        return encodedKey;
    }

    @Override
    public String datatype() {
        return "https://w3id.org/security#multibase";
    }

}

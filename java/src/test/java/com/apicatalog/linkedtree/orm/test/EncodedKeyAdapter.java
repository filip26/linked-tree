package com.apicatalog.linkedtree.orm.test;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.literal.ImmutableLiteral;
import com.apicatalog.linkedtree.orm.mapper.ObjectMapper;

public class EncodedKeyAdapter implements ObjectMapper<LinkedLiteral, EncodedKey> {

//    @Override
//    public LinkedLiteral materialize(String source) throws NodeAdapterError {
//        return new GenericEncodedKey(source);
//    }

//    @Override
//    public String normalize(EncodedKey value) {
//        return value.encodedKey();
//    }
//    
//    @Override
//    public String datatype() {
//        return "https://w3id.org/security#multibase";
//    }

//    @Override
//    public Class<? extends LinkedLiteral> typeInterface() {
//        return GenericEncodedKey.class;
//    }

    public static EncodedKey map(LinkedLiteral literal) {
        return getKey(literal.lexicalValue());
    }

    protected static final EncodedKey getKey(final String encodedKey) {
        return () -> encodedKey;
    }

    @Override
    public EncodedKey object(LinkedLiteral literal) {
        return map(literal);
    }

    @Override
    public LinkedLiteral literal(EncodedKey object) {
        // TODO Auto-generated method stub
        return new ImmutableLiteral(object.encodedKey(), "https://w3id.org/security#multibase");
    }
}

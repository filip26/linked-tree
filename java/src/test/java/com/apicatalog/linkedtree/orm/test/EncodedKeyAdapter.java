package com.apicatalog.linkedtree.orm.test;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.literal.ImmutableLiteral;
import com.apicatalog.linkedtree.literal.adapter.DataTypeNormalizer;
import com.apicatalog.linkedtree.orm.mapper.LiteralMapper;

public class EncodedKeyAdapter implements LiteralMapper<LinkedLiteral, EncodedKey>, DataTypeNormalizer<EncodedKey> {

//    @Override
//    public LinkedLiteral materialize(String source) throws NodeAdapterError {
//        return new GenericEncodedKey(source);
//    }

    @Override
    public String normalize(EncodedKey value) {
        return value.encodedKey();
    }
    
    @Override
    public String datatype() {
        return "https://w3id.org/security#multibase";
    }

//    @Override
//    public Class<? extends LinkedLiteral> typeInterface() {
//        return GenericEncodedKey.class;
//    }

    public static EncodedKey map(LinkedLiteral literal) throws NodeAdapterError {
        return getKey(literal.lexicalValue());
    }

    protected static final EncodedKey getKey(final String encodedKey) throws NodeAdapterError {
        return () -> encodedKey;
    }

    @Override
    public EncodedKey object(LinkedLiteral literal) throws NodeAdapterError {
        return map(literal);
    }
}

package com.apicatalog.linkedtree.orm;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.literal.ImmutableLiteral;
import com.apicatalog.linkedtree.literal.adapter.TypedLiteralAdapter;

public class EncodedKeyAdapter implements TypedLiteralAdapter {

    @Override
    public LinkedLiteral materialize(String source, LinkedTree root) throws NodeAdapterError {
        return new ImmutableLiteral(source, datatype(), root);
//      if (literal instanceof ImmutableLiteral il) {
//      return getKey(il.lexicalValue());
//  }
//
//  return literal;
    }

    @Override
    public String datatype() {
        return "https://w3id.org/security#multibase";
    }

    @Override
    public Class<? extends LinkedLiteral> typeInterface() {
        return ImmutableLiteral.class;
    }

    public static EncodedKey map(ImmutableLiteral literal) throws NodeAdapterError {
        return getKey(literal.lexicalValue());
    }

    protected static final EncodedKey getKey(final String encodedKey) throws NodeAdapterError {
        return () -> encodedKey;
    }
}

package com.apicatalog.linkedtree.orm;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.literal.ImmutableLiteral;
import com.apicatalog.linkedtree.literal.adapter.TypedLiteralAdapter;
import com.apicatalog.linkedtree.orm.mapper.LiteralMapper;

public class EncodedKeyMapper 
//implements LiteralMapper<LinkedLiteral, Object> 
{

//    @Override
//    public Object map(Class<?> type, LinkedLiteral literal) throws NodeAdapterError {
//
//        if (literal instanceof ImmutableLiteral il) {
//            return getKey(il.lexicalValue());
//        }
//
//        return literal;
//    }
//
//    @Override
//    public TypedLiteralAdapter adapter() {
//        return new TypedLiteralAdapter() {            
//            @Override
//            public LinkedLiteral materialize(String source, LinkedTree root) throws NodeAdapterError {
//                return new ImmutableLiteral(source, datatype(), root);
//            }
//            
//            @Override
//            public String datatype() {
//                return "https://w3id.org/security#multibase";
//            }
//        };
//    }

    protected static final EncodedKey getKey(final String encodedKey) throws NodeAdapterError {

        return new EncodedKey() {

            @Override
            public String encodedKey() {
                return encodedKey;
            }
        };
    }

//    @Override
//    public Object map(Class<Object> type, LinkedLiteral literal) throws NodeAdapterError {
//        if (literal instanceof ImmutableLiteral il) {
//            return getKey(il.lexicalValue());
//        }
//
//        return literal;
//    }
}

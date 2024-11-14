package com.apicatalog.linkedtree.orm.mapper;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.literal.adapter.DataTypeAdapter;

public interface LiteralMapper<T extends LinkedLiteral, R> {

    R object(T literal) throws NodeAdapterError;

//    default <D extends DataTypeAdapter> D adapter() {
//        return null;
//    }
    
//    T literal(R object) throws NodeAdapterError;
//    T toLiteral(R object) throws NodeAdapterError; 

    public static LiteralMapper<LinkedLiteral, LinkedLiteral> identity() {
        return i -> i;
//        return new LiteralMapper<LinkedLiteral, LinkedLiteral>() {
//            @Override
//            public LinkedLiteral object(LinkedLiteral literal) throws NodeAdapterError {
//                return literal;
//            }
//
//            @Override
//            public LinkedLiteral literal(LinkedLiteral object) throws NodeAdapterError {
//                return object;
//            }
//        };
    }

}

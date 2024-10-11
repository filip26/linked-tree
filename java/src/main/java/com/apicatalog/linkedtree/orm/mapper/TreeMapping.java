package com.apicatalog.linkedtree.orm.mapper;

import java.util.Map;

import com.apicatalog.linkedtree.literal.adapter.LiteralAdapter;
import com.apicatalog.linkedtree.type.TypeAdapter;

public record TreeMapping(
        Map<Class<?>, TypeAdapter> typeAdapters,
        Map<String, TypeAdapter> fragmentAdapters,
        Map<String, LiteralAdapter> literalAdapters
        ) {

    public static TreeMapperBuilder createBuilder() {
        return new TreeMapperBuilder().defaults();
    }
    
    public static TreeMapperBuilder createGenericBuilder() {
        return new TreeMapperBuilder();
    }
}

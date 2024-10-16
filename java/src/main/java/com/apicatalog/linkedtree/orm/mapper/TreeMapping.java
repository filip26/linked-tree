package com.apicatalog.linkedtree.orm.mapper;

import java.util.Map;

import com.apicatalog.linkedtree.literal.adapter.LiteralAdapter;
import com.apicatalog.linkedtree.type.TypeAdapter;

public record TreeMapping(
        Map<Class<?>, TypeAdapter> typeAdapters,
        Map<String, TypeAdapter> fragmentAdapters,
        Map<String, LiteralAdapter> literalAdapters
        ) {

    public static TreeMappingBuilder createBuilder() {
        return new TreeMappingBuilder().defaults();
    }
    
    public static TreeMappingBuilder createGenericBuilder() {
        return new TreeMappingBuilder();
    }
}
